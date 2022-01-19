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

question1 <- evaluations %>%
  filter(evaluation == "Evaluation A",
         question == "Three sisters 3: Was there anything confusing about either text?") %>%
  count(word_tokens) %>%
  rename(word = "word_tokens",
         freq = "n") %>%
  arrange(desc(freq))

wordcloud2(question1)

eval_a_questions <- evaluations %>%
  filter(evaluation == "Evaluation A") %>%
  distinct(question)

eval_b_questions <- evaluations %>%
  filter(evaluation == "Evaluation B") %>%
  distinct(question)

###################################################################################################
# Sentiment Analysis Evaluation A and B
###################################################################################################

ggplot(data = filter(merged, !is.na(sentiment))) +
  geom_histogram(aes(sentiment), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Evaluation A and B Responses BING Sentiments")

sentiment_all <- evaluations %>%
  filter(!is.na(sentiment)) %>%
  ggplot(aes(sentiment)) +
    geom_histogram(stat = "count", fill = c("firebrick1", "green2")) +
    labs(title = "Seniment Analysis: Evaluations A and B (BING)", 
         x = "Sentiment Classification", 
         y = "Word Count") +
    scale_x_discrete(labels = c("Negative", "Positive")) +
    theme_clean() +
    theme(axis.title.x = element_text(size = 11, face = "bold"),
          axis.title.y = element_text(size = 11, face = "bold"))

print(sentiment_all)

###################################################################################################
# split evaluation b into 2 dataframes of survey results and survey data analysis
evaluation_b_poll <- slice(evaluation_b, (15:23))

# https://dplyr.tidyverse.org/reference/slice.html  


# unnest one column 
killers_4 <- evaluation_b_responses %>% select(`Killers w/ hooves 4`)
# https://stackoverflow.com/questions/20854615/r-merge-multiple-rows-of-text-data-frame-into-one-cell

killers_4 <- paste(unlist(killers_4), collapse = " ")

killers_4 <- tibble(text = killers_4)

unnest_tokens(killers_4, word_tokens, 
              text, token = "words")


paste(unlist(evaluation_b_responses), collapse = " ")



# sentiments

for (s in c("nrc", "afinn", "bing")) {
  all_b <- all_b %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

ggplot(data = filter(all_b, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Evaluation B Responses NRC Sentiments")

ggplot(data = filter(all_b, !is.na(bing))) +
  geom_histogram(aes(bing), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Evaluation B Responses BING Sentiments")

ggplot(data = filter(all_b, !is.na(afinn))) +
  geom_histogram(aes(afinn), stat = "count") +
  scale_x_continuous(n.breaks = 7) +
  labs(title = "Evaluation B Responses AFINN Sentiments")

# wordcloud b

no_sw_all_b <- anti_join(all_b, stop_words, by = c("word_tokens" = "word"))

all_b_freq <- no_sw_all_b %>%
  group_by(question) %>%
  count(word_tokens)

no_sw_freq <- no_sw_all_b %>%
  count(word_tokens) %>%
  rename(word = "word_tokens",
         freq = "n") %>%
  arrange(desc(freq))

q1 <- no_sw_all_b %>%
  filter(question == 1) %>%
  count(word_tokens) %>%
  rename(word = "word_tokens",
         freq = "n") %>%
  arrange(desc(freq))

wordcloud2(q1)
#https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html





