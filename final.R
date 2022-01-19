###################################################################################################
# SET UP
###################################################################################################

#Grading comments:
# - Good use of tidyverse style and dplyr
# - Nice functions for generalization
# - I like your ggplot work
# - Good descriptive comments

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

###################################################################################################
# Component 1: Data wrangling
###################################################################################################


#################################################
# Giraffe Data Cleaning
#################################################


# function that loads and cleans the two giraffe data sets
giraffe_data <- function(file_name, giraffe_name) {
    read_excel(paste0(path, file_name), skip = 2) %>%
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


write_csv(giraffes, file = file.path(paste0(path, "giraffes_clean.csv")))


#################################################
# Peabody Data Cleaning
#################################################

# function that loads and cleans Peabody survey data
evals <- function(sheet_name, num_lines) {
  object <- read_excel(paste0(path, "_Evaluation A and B Text Responses ONLY.xlsx"), sheet = sheet_name) 
           
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


# graph plotting stress levels of giraffes during different construction phases
png(filename = file.path(path, "giraffe.png"), width = 800, height = 400)
giraffes %>%
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
dev.off()
#https://stackoverflow.com/questions/5388832/how-to-get-a-vertical-geom-vline-to-an-x-axis-of-class-date


# graph plotting stress levels of Etana during different construction phases with regression lines
png(filename = file.path(path, "etana.png"), width = 800, height = 400)
etana %>%
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
dev.off()


# graph plotting stress levels of Sabrena during different construction phases with regression lines
png(filename = file.path(path, "sabrena.png"), width = 800, height = 400)
sabrena %>%
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
dev.off()


### SEE APP FOR INTERACTIVE PLOTS ###

###################################################################################################
# Component 3: Text processing
###################################################################################################


# merge Evaluation A and B together and add BING sentiments
merged <- full_join(all_a, all_b) %>%
  left_join(get_sentiments("bing"), by = c("word_tokens" = "word"))


# remove stop words and NAs 
evaluations <- anti_join(merged, stop_words, by = c("word_tokens" = "word")) %>%
  filter(word_tokens != "na")


write_csv(evaluations, file = file.path(paste0(path, "evaluations_clean.csv")))


# BING sentiment analysis for Evaluations A and B combined
png(filename = file.path(path, "evaluations.png"), width = 800, height = 400)
evaluations %>%
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
dev.off()


### SEE APP FOR INTERACTIVE PLOTS AND WORD CLOUDS ###


###################################################################################################
# Component 4: Analysis
###################################################################################################

# fitting linear models of stress hormone levels (CC (ng/g)) on construction dummy
etana_reg <- lm(`CC (ng/g)` ~ construction_dummy, data = etana)
summary(etana_reg)

sabrena_reg <- lm(`CC (ng/g)` ~ construction_dummy, data = sabrena)
summary(sabrena_reg)



