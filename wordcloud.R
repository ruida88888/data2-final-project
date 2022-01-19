library(shiny)
library(plotly)
library(scales)
library(tidyverse)
library(dplyr)
library(readxl)
library(tidytext)
library(wordcloud)
library(ggthemes)
library(tm)
library(wordcloud2)

path <- "C:/Users/balas/OneDrive/Documents/Harris/Data_and_Programming_II/Homework/final_project/data_skills_2_r_project/"

# read in evaluations a and b
evaluation_a <- read_excel(paste0(path, "_Evaluation A and B Text Responses ONLY.xlsx"), 
                           sheet = "Evauluation A Text Responses ON")
evaluation_b <- read_excel(paste0(path, "_Evaluation A and B Text Responses ONLY.xlsx"), 
                           sheet = "Evalution B Text Responses ONLY")

#https://stackoverflow.com/questions/34092237/applying-dplyrs-rename-to-all-columns-while-using-pipe-operator/56304086

###################################################################################################
# Evaluation A clean-up
###################################################################################################

# rename columns adding question from row 1 and then delete row 1
evaluation_a <- evaluation_a %>% 
  rename_all(funs(str_c(colnames(evaluation_a), evaluation_a[1,], sep = ": "))) %>%
  slice(-c(1)) %>%
  slice(c(1:70))

all_cols_a <- list(ncol(evaluation_a))
for (i in seq_along(evaluation_a)) {
  all_cols_a[[i]] <- paste(unlist(evaluation_a[[i]]), collapse = " ")
}

all_cols_a <- tibble(text = all_cols_a)

questions_a <- tibble(colnames(evaluation_a)) %>%
  rename(questions = "colnames(evaluation_a)")

all_cols_a <- all_cols_a %>%
  mutate(question = questions_a$questions,
         evaluation = "Evaluation A")

all_a <- unnest_tokens(all_cols_a, word_tokens, text, token = "words")
###################################################################################################
# Evaluation B clean-up
###################################################################################################

# rename columns adding question from row 1 and then delete row 1
evaluation_b <- evaluation_b %>% 
  rename_all(funs(str_c(colnames(evaluation_b), evaluation_b[1,], sep = ": "))) %>%
  slice(-c(1)) %>%
  slice(c(1:13))

# unnest all columns
all_cols <- list(ncol(evaluation_b))
for (i in seq_along(evaluation_b)) {
  all_cols[[i]] <- paste(unlist(evaluation_b[[i]]), collapse = " ")
}

all_cols <- tibble(text = all_cols)

questions_b <- tibble(colnames(evaluation_b)) %>%
  rename(questions = "colnames(evaluation_b)")

all_cols <- all_cols %>%
  mutate(question = questions_b$questions,
         evaluation = "Evaluation B")

all_b <- unnest_tokens(all_cols, word_tokens, text, token = "words")

###################################################################################################
# Wordcloud Evaluation A and B
###################################################################################################

merged <- full_join(all_a, all_b) %>%
  left_join(get_sentiments("bing"), by = c("word_tokens" = "word"))

evaluations <- anti_join(merged, stop_words, by = c("word_tokens" = "word")) %>%
  filter(word_tokens != "na")

evaluations_count_grouped <- evaluations %>%
  group_by(evaluation, question) %>%
  count(word_tokens) %>%
  rename(word = "word_tokens",
         freq = "n") %>%
  ungroup() %>%
  select(word, freq) %>%
  arrange(desc(freq))

evaluations_count <- evaluations %>%
  count(word_tokens) %>%
  rename(word = "word_tokens",
         freq = "n") %>%
  select(word, freq) %>%
  arrange(desc(freq))

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
# Read Giraffe Data
###################################################################################################

etana <- read_excel(paste0(path, "Etana EIA Results - Updated.xlsx"), skip = 2)

etana <- etana %>%
  mutate(construction_status = case_when(`Sample date` > "2014-10-16" & `Sample date` < "2014-12-21" ~ "Demo",
                                         `Sample date` > "2014-12-21" & `Sample date` < "2015-04-06" ~ "No Construction",
                                         `Sample date` > "2015-04-05" & `Sample date` < "2015-11-30" ~ "Active Construction"),
         name = "Etana",
         construction_dummy = ifelse(construction_status == "Demo" | construction_status == "Active Construction", 1, 0),
         `Sample date` = as.Date(`Sample date`)) %>%
  select(everything(), -`Sample #`)

sabrena <- read_excel(paste0(path, "Sabrena EIA Results - Updated.xlsx"), skip = 2)

sabrena <- sabrena %>%
  mutate(construction_status = case_when(`Sample date` > "2014-10-16" & `Sample date` < "2014-12-21" ~ "Demo",
                                         `Sample date` > "2014-12-21" & `Sample date` < "2015-04-06" ~ "No Construction",
                                         `Sample date` > "2015-04-05" & `Sample date` < "2015-11-30" ~ "Active Construction"),
         name = "Sabrena",
         construction_dummy = ifelse(construction_status == "Demo" | construction_status == "Active Construction", 1, 0),
         `Sample date` = as.Date(`Sample date`)) %>%
  select(everything(), -`Sample #`)

giraffes <- full_join(sabrena, etana, by = c(colnames(etana))) 

###################################################################################################
# Giraffe Graphs
###################################################################################################

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

# lm by construction status
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
                             tags$p("It is required that you use GitHub, and I will use your past commits to understand your thought process for partial credit, and to monitor group participation.  You will not get full credit if your repository does not show multiple commits as you build your project, especially for groups.  Expectations for the scope of the project will be higher for groups than for individuals, and the division of labor will be up to each group.  Note again that I will be using your GitHub commit history in grading, so while I will lean toward giving the same grade to everyone in a group, it is possible that group members will recieve different grades.")),
                      ),
             fluidRow(column(width = 12, 
                             tags$h3("Lincoln Park Zoo", align = "center"),
                             ),
                        ), 
             fluidRow(column(width = 12, 
                             tags$p("It is required that you use GitHub, and I will use your past commits to understand your thought process for partial credit, and to monitor group participation.  You will not get full credit if your repository does not show multiple commits as you build your project, especially for groups.  Expectations for the scope of the project will be higher for groups than for individuals, and the division of labor will be up to each group.  Note again that I will be using your GitHub commit history in grading, so while I will lean toward giving the same grade to everyone in a group, it is possible that group members will recieve different grades.")),
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
