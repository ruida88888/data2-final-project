###################################################################################################
# SET UP
###################################################################################################

path <- "C:/Users/balas/OneDrive/Documents/Harris/Data_and_Programming_II/Homework/final_project/data_skills_2_r_project/"

#libraries that may need to be installed

#install.packages("ggthemes")
#install.packages("wordcloud2")
#install.packages("readxl")

# load libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(ggthemes)
library(tidytext)
library(wordcloud2)
library(plotly)

###################################################################################################
# Component 1: Data wrangling
###################################################################################################


#################################################
# Giraffe Data Cleaning
#################################################


# function that loads and cleans the two giraffe data sets
giraffe_data <- function(file_name, giraffe_name) {
  read_excel(file_name, skip = 2) %>%
    mutate(construction_status = case_when(`Sample date` > "2014-10-16" & `Sample date` < "2014-12-21" ~ "Demo",
                                           `Sample date` > "2014-12-21" & `Sample date` < "2015-04-06" ~ "No Construction",
                                           `Sample date` > "2015-04-05" & `Sample date` < "2015-11-30" ~ "Active Construction"),
           name = giraffe_name,
           construction_dummy = ifelse(construction_status == "Demo" | construction_status == "Active Construction", 1, 0),
           `Sample date` = as.Date(`Sample date`)) %>%
    select(everything(), -`Sample #`)
}


# load and clean two giraffe data sets using giraffe_data function
etana <- giraffe_data("Etana EIA Results - Updated.xlsx", "Etana")
sabrena <- giraffe_data("Sabrena EIA Results - Updated.xlsx", "Sabrena")


# merge two giraffe data sets
giraffes <- full_join(sabrena, etana, by = c(colnames(etana)))


#################################################
# Peabody Data Cleaning
#################################################

# function that loads and cleans Peabody survey data
evals <- function(sheet_name, num_lines) {
  object <- read_excel("_Evaluation A and B Text Responses ONLY.xlsx", sheet = sheet_name) 
  
  object <- object %>% 
    rename_all(funs(str_c(colnames(object), object[1,], sep = ": "))) %>%
    slice(-c(1)) %>%
    slice(c(1:num_lines))    
  #https://stackoverflow.com/questions/34092237/applying-dplyrs-rename-to-all-columns-while-using-pipe-operator/56304086
  
}


# use function to load and clean Peabody survey data
evaluation_a <- evals("Evauluation A Text Responses ON", 70)
evaluation_b <- evals("Evalution B Text Responses ONLY", 13)


# function to tidy survey data for text analysis 
clean <- function(object, eval_name) {
  
  all_cols <- list(ncol(object))
  for (i in seq_along(object)) {
    all_cols[[i]] <- paste(unlist(object[[i]]), collapse = " ")
  }
  
  all_cols <- tibble(text = all_cols)
  
  ques <- tibble(colnames(object)) %>%
    rename(questions = "colnames(object)")
  
  all_cols <- all_cols %>%
    mutate(question = ques$questions,
           evaluation = eval_name)
  
  all <- unnest_tokens(all_cols, word_tokens, text, token = "words")
}


# use function to tidy survey data for text analysis on Evaluations A and B
all_a <- clean(evaluation_a, "Evaluation A")
all_b <- clean(evaluation_b, "Evaluation B")


###################################################################################################
# Component 2: Plotting
###################################################################################################

###################################################################################################
# Wordcloud Evaluation A and B
###################################################################################################


# merge Evaluation A and B together and add BING sentiments
merged <- full_join(all_a, all_b) %>%
  left_join(get_sentiments("bing"), by = c("word_tokens" = "word"))

# remove stop words and NAs 
evaluations <- anti_join(merged, stop_words, by = c("word_tokens" = "word")) %>%
  filter(word_tokens != "na")

evaluations_count <- evaluations %>%
  count(word_tokens) %>%
  rename(word = "word_tokens",
         freq = "n") %>%
  select(word, freq) %>%
  arrange(desc(freq))

# list of questions for drop-down menu of filtered sentiment analyses and word clouds
eval_a_questions <- evaluations %>%
  filter(evaluation == "Evaluation A") %>%
  distinct(question)

eval_b_questions <- evaluations %>%
  filter(evaluation == "Evaluation B") %>%
  distinct(question)


###################################################################################################
# Sentiment Analysis Evaluation A and B
###################################################################################################


sentiments_all <- evaluations %>%
  filter(!is.na(sentiment)) %>%
  ggplot(aes(sentiment)) +
  geom_histogram(stat = "count", fill = c("firebrick1", "green2")) +
  labs(title = "Sentiment Analysis: Evaluations A and B (BING)", 
       x = "Sentiment Classification", 
       y = "Word Count") +
  scale_x_discrete(labels = c("Negative", "Positive")) +
  theme_clean() +
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"))


###################################################################################################
# Giraffe Graphs
###################################################################################################


# graph plotting stress levels of giraffes during different construction phases
both_giraffes <- giraffes %>%
  ggplot(aes(x = `Sample date`, y = `CC (pg/well)`, color = name)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("darkgreen", "orangered")) +
  geom_vline(xintercept = as.numeric(as.Date(c("2014-12-21", "2015-04-06"))), linetype = "dashed", 
             color = "black", size = 1.5) +
  geom_text(x = as.numeric(as.Date("2014-11-11")), y = 225, label = "Demo", show.legend = FALSE, color = "blue") +
  geom_text(x = as.numeric(as.Date("2015-02-11")), y = 225, label = "No Construction", show.legend = FALSE, color = "blue") +
  geom_text(x = as.numeric(as.Date("2015-08-11")), y = 225, label = "Active Construction", show.legend = FALSE, color = "blue") +
  labs(title = "Cortisol Metabolite Levels During Construction Phases", x = "Date of Sample Taken", color = "Giraffe") +
  theme_clean() +
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"))
#https://stackoverflow.com/questions/5388832/how-to-get-a-vertical-geom-vline-to-an-x-axis-of-class-date


# graph plotting stress levels of Etana during different construction phases with regression lines
etana_graph <- etana %>%
  ggplot(aes(x = `Sample date`, y = `CC (ng/g)`, color = construction_status)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_color_manual(values = c("gold3", "midnightblue", "firebrick3")) +
  geom_vline(xintercept = as.numeric(as.Date(c("2014-12-21", "2015-04-06"))), linetype = "dashed", 
             color = "black", size = 1.5) +
  geom_text(x = as.numeric(as.Date("2014-11-11")), y = 250, label = "Demo", show.legend = FALSE, color = "blue") +
  geom_text(x = as.numeric(as.Date("2015-02-11")), y = 250, label = "No Construction", show.legend = FALSE, color = "blue") +
  geom_text(x = as.numeric(as.Date("2015-08-11")), y = 250, label = "Active Construction", show.legend = FALSE, color = "blue") +
  labs(title = "Etana: Cortisol Metabolite Levels with Linear Regression Line for each Construction Phase", x = "Date of Sample Taken", color = "Construction Status") +
  theme_clean() +
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"))


# graph plotting stress levels of Sabrena during different construction phases with regression lines
sabrena_graph <- sabrena %>%
  ggplot(aes(x = `Sample date`, y = `CC (ng/g)`, color = construction_status)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_color_manual(values = c("gold3", "midnightblue", "firebrick3")) +
  geom_vline(xintercept = as.numeric(as.Date(c("2014-12-21", "2015-04-06"))), linetype = "dashed", 
             color = "black", size = 1.5) +
  geom_text(x = as.numeric(as.Date("2014-11-11")), y = 700, label = "Demo", show.legend = FALSE, color = "blue") +
  geom_text(x = as.numeric(as.Date("2015-02-11")), y = 700, label = "No Construction", show.legend = FALSE, color = "blue") +
  geom_text(x = as.numeric(as.Date("2015-08-11")), y = 700, label = "Active Construction", show.legend = FALSE, color = "blue") +
  labs(title = "Sabrena: Cortisol Metabolite Levels with Linear Regression Line for each Construction Phase", x = "Date of Sample Taken", color = "Construction Status") +
  theme_clean() +
  theme(axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"))


###################################################################################################
# Shiny App
###################################################################################################


ui <- 
  
  navbarPage("Neeti's Research Projects",
             
             tabPanel("Home",
                      fluidRow(column(width = 12, 
                                      tags$h1("Neeti's Research Projects", align = "center"),
                                      tags$hr()),
                      ),
                      fluidRow(column(width = 12, 
                                      tags$h3("Peabody", align = "center"),
                      ),
                      ), 
                      fluidRow(column(width = 12, 
                                      tags$p("The data Neeti shared with me from the Peabody came from two separate surveys: 
                                             Evaluation A was administered to paid Yale students and Peabody members who 
                                             responded to an e-newsletter, and Evaluation B was administered to Yale students 
                                             that work for the interpretation team at Peabody. The surveys asked participants 
                                             to read labels that appear alongside museum exhibits and respond to several 
                                             questions about these labels. Neeti asked me to find general trends from these 
                                             responses and words or sentiments that were most frequently used. The survey 
                                             responses were download onto an Excel file in separate sheets from the survey 
                                             software Qualtrics. I imported the two sheets with the two evaluations of 
                                             interests and cleaned and merged the two data sets organized by question and 
                                             evaluation. I cleaned the data so as to only include the text responses from 
                                             the individual respondents. After talking over the research goals and what my 
                                             analysis could offer, Neeti indicated that classifying words used by respondents 
                                             as positive or negative would be most helpful to her. I also included a word cloud 
                                             of the most frequently used words used in both evaluations combined as well as an 
                                             option to generate word clouds specific to individual survey questions.")),
                      ),
                      fluidRow(column(width = 12, 
                                      tags$h3("Lincoln Park Zoo", align = "center"),
                      ),
                      ), 
                      fluidRow(column(width = 12, 
                                      tags$p("The giraffe data Neeti shared with me was from the beginning stages of her research
                                             over the period of late 2014 through November 2015 when there were three separate 
                                             phases of the giraffe exhibit reconstruction: demolition, a pause on construction 
                                             activities, and then active construction. The research team collected fecal samples 
                                             from two female giraffes, Etana and Sabrena, consistently throughout this period and 
                                             collected data on several hormonal levels. I plot these data by phases of construction 
                                             and fit a model to see if there are statistically significant changes in CC (ng/g) 
                                             levels during construction (demolition and active construction) relative to the period 
                                             of no construction.")),
                      ),
             ),
             tabPanel("Peabody",
                      fluidRow(plotlyOutput("sent")),
                      fluidRow(selectInput(inputId = "qq",
                                           label = "Choose a question",
                                           list(
                                             "Evaluation A" = eval_a_questions$question,
                                             "Evaluation B" = eval_b_questions$question))
                      ),
                      fluidRow(plotlyOutput("sent2")),
                      fluidRow(column(width = 12, 
                                      tags$h2("Word Cloud: Evaluations A and B Combined", align = "center"),
                                      tags$hr()),
                      ),
                      fluidRow(wordcloud2Output("first")), 
                      fluidRow(column(width = 12,
                                      tags$h2("Word Cloud: Individual Questions", align = "center"))
                      ),
                      fluidRow(selectInput(inputId = "q",
                                           label = "Choose a question",
                                           list(
                                             "Evaluation A" = eval_a_questions$question,
                                             "Evaluation B" = eval_b_questions$question))
                      ),
                      fluidRow(wordcloud2Output("ques")),
             ),
             
             tabPanel("Lincoln Park Zoo",
                      fluidRow(column(width = 12, 
                                      tags$h1("Giraffe Data", align = "center"),
                                      tags$hr())
                      ),
                      fluidRow(plotlyOutput("both")),
                      fluidRow(plotlyOutput("etana")),
                      fluidRow(plotlyOutput("sabrena"))
             )
             
             
             
  )  
#multiphttps://community.rstudio.com/t/shiny-app-composed-of-many-many-pages/7698
#https://github.com/daattali/advanced-shiny/blob/master/dropdown-groups/app.R

server <- function(input, output) {
  
  filtered <- reactive({
    wrdcloud <- evaluations %>% 
      filter(question == input$q) %>%
      count(word_tokens) %>%
      rename(word = "word_tokens",
             freq = "n") %>%
      arrange(desc(freq))
  })
  
  evaluations_filter <- reactive({
    evaluations %>%
      filter(!is.na(sentiment),
             question == input$qq) %>%
      ggplot(aes(sentiment)) +
      geom_histogram(stat = "count", fill = c("firebrick1", "green2")) +
      labs(title = "Sentiment Analysis: Individual Questions (BING)", 
           x = "Sentiment Classification", 
           y = "Word Count") +
      scale_x_discrete(labels = c("Negative", "Positive")) +
      theme_clean() +
      theme(axis.title.x = element_text(size = 11, face = "bold"),
            axis.title.y = element_text(size = 11, face = "bold"))
  })
  
  output$sent <- renderPlotly({
    ggplotly(sentiments_all)
  })
  
  output$sent2 <- renderPlotly({
    ggplotly(evaluations_filter())
  })
  
  output$first <- renderWordcloud2({
    wordcloud2(evaluations_count)
  })
  
  
  
  output$ques <- renderWordcloud2({
    wordcloud2(filtered())
  })  
  #https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html
  
  output$both <- renderPlotly({
    ggplotly(both_giraffes)
  })
  
  output$etana <- renderPlotly({
    ggplotly(etana_graph)
  })
  
  output$sabrena <- renderPlotly({
    ggplotly(sabrena_graph)
  })
  
}

shinyApp(ui = ui, server = server)
