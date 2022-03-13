## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(sf)
library(plotly)
library(janitor)
library(tmap)
library(tmaptools)
library(tidytext)


## ---- cache=TRUE,warning=F,message=FALSE--------------------------------------------------------------------------------------------------------------
############ Dataset #1 ############
PP_2005 <- read_csv("http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2005.csv",col_names = F) # Load UK Archived Price Paid Data for the year 2005

############ Dataset #2 ############
PP_2010 <- read_csv("http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2010.csv",col_names = F) # Load UK Archived Price Paid Data for the year 2010

############ Dataset #3 ############
EW <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson")
LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",locale = locale(encoding = "latin1"),na = "n/a")


## -----------------------------------------------------------------------------------------------------------------------------------------------------
Days_to_filter <- 30 # Days to filter before and after the bombing
date_of_bombing <- as.Date("2005-07-07") # Date of bombing

############ Dataset #1 ############
PP_2005_f <- PP_2005
PP_2005_f <- PP_2005_f %>% filter(X13 %in% c("CITY OF LONDON","CAMDEN","CITY OF WESTMINSTER")) # Filter the three cities where the bombing happened
PP_2005_f$date <- as.Date(PP_2005_f$X3) # Create a new column with the class date
PP_2005_f <- PP_2005_f %>% group_by( week_n = paste(week(date))) # Add week number column
PP_2005_f <- PP_2005_f %>% filter(date < date_of_bombing+Days_to_filter & date > date_of_bombing-Days_to_filter) # Filter for the date of bombing +- Days_to_filter
PP_2005_f <- PP_2005_f %>% mutate(days_from_bombing=(date-date_of_bombing)) # Create a new variable with the number of days from the date_of_bombing

PP_2005_f$Year <- 2005
PP_2005_f_ave <- PP_2005_f %>% dplyr::group_by(Year,days_from_bombing,X13) %>% dplyr::summarize(avg=mean(X2), n=n(), sd=sd(X2), se=sd/sqrt(n)) # Create a summary dataframe with the average paid prices per date_of_bombing
PP_2005_f_ave_week <- PP_2005_f %>% dplyr::group_by(Year,week_n,X13) %>% dplyr::summarize(avg=mean(X2), n=n(), sd=sd(X2), se=sd/sqrt(n)) # Create a summary dataframe with the average paid prices per week number

############ Dataset #2 ############
PP_2010_f <- PP_2010
PP_2010_f <- PP_2010_f %>% filter(X13 %in% c("CITY OF LONDON","CAMDEN","CITY OF WESTMINSTER")) # Filter the three cities where the bombing happened
PP_2010_f$date <- as.Date(PP_2010_f$X3) # Create a new column with the class date
PP_2010_f <- PP_2010_f %>% group_by( week_n = paste(week(date))) # Add week number column
PP_2010_f <- PP_2010_f %>% filter(date < date_of_bombing %m+% years(5)+Days_to_filter & date > date_of_bombing %m+% years(5)-Days_to_filter) # Filter for the date of bombing +- Days_to_filter
PP_2010_f <- PP_2010_f %>% mutate(days_from_bombing=(date-date_of_bombing)) # Create a new variable with the number of days from the date_of_bombing
PP_2010_f$Year <- 2010
PP_2010_f_ave <- PP_2010_f %>% group_by(Year,days_from_bombing,X13) %>% dplyr::summarize(avg=mean(X2), n=n(), sd=sd(X2), se=sd/sqrt(n)) # Create a summary dataframe with the average paid prices per date_of_bombing
PP_2010_f_ave_week <- PP_2010_f %>% group_by(Year,week_n,X13) %>% dplyr::summarize(avg=mean(X2), n=n(), sd=sd(X2), se=sd/sqrt(n)) # Create a summary dataframe with the average paid prices per week number

PP_f_ave_combined <- rbind(PP_2005_f_ave,PP_2010_f_ave)
PP_2010_f_ave_week_combined <- rbind(PP_2005_f_ave_week,PP_2010_f_ave_week)
colnames(PP_f_ave_combined) <- c("Year", "Days from Bombing","City", "Average House Prices", "n", "SD", "SE")
colnames(PP_2010_f_ave_week_combined) <- c("Year", "Week Number","City", "Average House Prices", "n", "SD", "SE")
############ Dataset #3 ############



## ---- fig.width=12, fig.height=8, fig.align="center"--------------------------------------------------------------------------------------------------
PP_2010_f_ave_week_combined$Year <- as.factor(PP_2010_f_ave_week_combined$Year)
PP_2010_f_ave_week_combined %>% 
  ggplot(aes(x=`Week Number`, y=`Average House Prices`, group=Year, colour = Year)) +
  geom_errorbar(aes(ymin=`Average House Prices`-SD, ymax=`Average House Prices`+SD), width=.1,) +
  geom_line() + geom_point()+ ggtitle("The Average UK House Prices in 2005 vs 2010 in the three cities that were targeted in 2005 Bombings")+
  theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))+ geom_vline(xintercept = 0, linetype="dotted", 
                color = "red", size=1)+facet_grid(City~.)


## ---- fig.width=12, fig.height=8, fig.align="center"--------------------------------------------------------------------------------------------------
PP_f_ave_combined %>% 
  filter(Year==2005) %>%
  ggplot(aes(x=`Days from Bombing`, y=`Average House Prices`, group=City, colour = City)) +
  geom_errorbar(aes(ymin=`Average House Prices`-SD, ymax=`Average House Prices`+SD), width=.1,) +
  geom_line() + geom_point()+ ggtitle("The Average UK House Prices 30 Days Before and After The Bombing")+
  theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))+ geom_vline(xintercept = 0, linetype="dotted", 
                color = "red", size=1)


## ---- fig.width=12, fig.height=8, fig.align="center"--------------------------------------------------------------------------------------------------
p <- PP_2005_f %>% 
    filter(Year==2005) %>% filter(`week_n` %in% c(26,27,28)) %>%
    ggplot(aes(x=`week_n`, y=X2, group=`week_n`, colour = `week_n`))+geom_boxplot()+facet_grid(~X13)+geom_vline(xintercept = 27, linetype="dotted",color = "red", size=1)+ylab("The Average UK House Prices")+xlab("bombing occured at the end of week 26 of 2005")+ggtitle("The Average UK House During The Weeks Before and After Bombing")
ggplotly(p)


## ---- fig.width=12, fig.height=8, fig.align="center"--------------------------------------------------------------------------------------------------
LondonData <- clean_names(LondonData)
BoroughDataMap <- EW %>%
  clean_names()%>%
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)

BoroughDataMap <- BoroughDataMap %>% mutate(b_n=ifelse(ward_name %in% c("City of London","Westminster"),1,ifelse(ward_name=="Camden",2,0)))
BoroughDataMap <- BoroughDataMap %>% mutate(`Bomb Location`=ifelse(ward_name %in% c("City of London","Westminster","Camden"),ward_name,0))
BoroughDataMap$b_n <- as.factor(BoroughDataMap$b_n)

tmap_mode("plot")
qtm(BoroughDataMap,title = "Map Showing The Targeted Wards",
    fill = "Bomb Location")

tmap_mode("plot")
qtm(BoroughDataMap, fill.palette = c("white","red"),title = "Map Showing The Number of Bombs in Each Wards",
    fill = "b_n")


## -----------------------------------------------------------------------------------------------------------------------------------------------------
require(rvest)
this_url <- "https://www.britannica.com/event/London-bombings-of-2005"
request <- read_html(this_url)
link_elements= str_split(this_url, "-")
year = substr(link_elements[[1]][4],1,4)
article <- html_node(request,"body")
paragraphs <- html_nodes(article, "p")
text_list <- html_text(paragraphs)
brief_text <- paste(text_list, collapse = "")
text.df <- tibble(text=brief_text)
word.tokens.df =unnest_tokens(text.df, word_tokens, text, token="words")
no.sw.unhcr = anti_join(word.tokens.df, stop_words, by=c("word_tokens" = "word"))

#get most popular words from text
most_popular_words = count(no.sw.unhcr, word_tokens, sort=TRUE)

#create graph of number of occurrences of most common words
most.pop.20 =head(most_popular_words, 20)

ggplot(most.pop.20, aes(x=word_tokens, y=n)) + geom_bar(stat="identity") + scale_x_discrete(guide = guide_axis(angle=45)) +xlab("Word")


## -----------------------------------------------------------------------------------------------------------------------------------------------------
PP_2005_model_df <- PP_f_ave_combined %>% filter(Year==2005)
model <- lm(`Average House Prices`~City+`Days from Bombing`,PP_2005_model_df)
summary(model)
plot(model)


## ----performance, message=FALSE, warning=FALSE, paged.print=TRUE--------------------------------------------------------------------------------------
PP_2005_model_df$prediction <- predict(model, newdata = PP_2005_model_df)
ggplot(PP_2005_model_df, aes(x = prediction, y = `Average House Prices`)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

PP_2005_model_df$residuals <- PP_2005_model_df$`Average House Prices` - PP_2005_model_df$prediction

ggplot(data = PP_2005_model_df, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")

ggplot(PP_2005_model_df, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "blue") +
  ggtitle("Histogram of residuals")

