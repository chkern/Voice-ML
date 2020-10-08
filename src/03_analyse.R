library(tidyverse)
library(ggmosaic)
library(caret)

## Change path
wdpath <- "~/Uni/Forschung/Article/2020 - VoiceML/data"
setwd(wdpath)

voice <- readRDS("voice_q1_vignette_<6s.rds")
survey <- load("survey_data_voice_experiment_2020_06_29.RData")

survey_c <-
  survey %>%
  left_join(voice, by = c("tic" = "id")) 

survey_c <- survey_c %>%
  mutate_at(c("e1_class", "e2_class", "e3_class"), as.factor) %>%
  mutate(e1_class = fct_recode(e1_class, 
                               anger = "e1_anger_m", 
                               boredom = "e1_boredom_m",
                               disgust = "e1_disgust_m",
                               fear = "e1_fear_m",
                               happiness = "e1_happiness_m",
                               neutral = "e1_neutral_m",
                               sadness = "e1_sadness_m"),
         e2_class = fct_recode(e2_class, 
                               agressiv = "e2_agressiv_m", 
                               cheerful = "e2_cheerful_m",
                               intoxicated = "e2_intoxicated_m",
                               nervous = "e2_nervous_m",
                               tired = "e2_tired_m"),
         e3_class = fct_recode(e3_class, 
                               disinterest = "e3_loi1_m", 
                               normal = "e3_loi2_m",
                               high_interest = "e3_loi3_m"))

# Description Emotion Predictions

survey_c %>%
  drop_na(e1_class) %>%
  ggplot() +
  geom_bar(aes(x = e1_class))

survey_c %>%
  drop_na(e2_class) %>%
  ggplot() +
  geom_bar(aes(x = e2_class))

survey_c %>%
  drop_na(e1_class) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e1_class, e2_class), fill = e1_class)) + 
  theme(legend.title = element_blank())

survey_c %>%
  drop_na(e1_class) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e1_class, e3_class), fill = e1_class)) + 
  theme(legend.title = element_blank())

prop.table(table(survey_c$e1_class, survey_c$e2_class))
prop.table(table(survey_c$e1_class, survey_c$e3_class))

# Emotion Predictions and Survey Responses
