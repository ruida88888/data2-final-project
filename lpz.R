library(tidyverse)
library(dplyr)
library(readxl)
library(ggthemes)

path <- "C:/Users/balas/OneDrive/Documents/Harris/Data_and_Programming_II/Homework/final_project/data_skills_2_r_project/"

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

giraffes %>%
  mutate(`Sample date` = as.Date(`Sample date`))

colnames(etana)

etana_reg <- lm(`CC (ng/g)` ~ construction_dummy, data = etana)
summary(etana_reg)
# scatter plots 

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
    theme_clean() 
    
#https://stackoverflow.com/questions/5388832/how-to-get-a-vertical-geom-vline-to-an-x-axis-of-class-date

etana %>%
  ggplot(aes(x = `Sample date`, y = `CC (pg/well)`, color = construction_status)) +
    geom_point() +
    stat_smooth(method = "lm")

etana %>%
  ggplot(aes(x = `Sample date`, y = `CC (pg/ml)`)) +
  geom_point()

etana %>%
  ggplot(aes(x = `Sample date`, y = Amount)) +
  geom_point()

# line graphs
etana %>%
  ggplot(aes(x = `Sample date`, y = `CC (pg/well)`)) +
  geom_line()

etana %>%
  ggplot(aes(x = `Sample date`, y = `CC (pg/ml)`)) +
  geom_line()

etana %>%
  ggplot(aes(x = `Sample date`, y = Amount)) +
  geom_line()

# lm by construction status
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
  theme_clean() 

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
  theme_clean() 

# histograms
etana %>%
  ggplot(aes(x = `CC (ng/g)`)) +
  geom_histogram(bins = 80)

etana %>%
  filter(construction_status == "Demo") %>%
  ggplot(aes(x = `CC (ng/g)`)) +
    geom_histogram(bins = 10)

df %>%
  ggplot(aes(x = `CC (ng/g)`)) +
  geom_histogram(bins = 50)

sabrena %>%
  filter(construction_status == "Demo") %>%
  ggplot(aes(x = `CC (ng/g)`)) +
  geom_histogram(bins = 30)
