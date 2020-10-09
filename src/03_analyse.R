library(tidyverse)
library(ggmosaic)
library(caret)

## Change path
wdpath <- "~/Uni/Forschung/Article/2020 - VoiceML/data"
setwd(wdpath)

voice1 <- readRDS("voice_q1_vignette_6s.rds")
voice2 <- readRDS("voice_q1_vignette_6s-15s.rds")
load("survey_voice_experiment_2020_07_09_updated.RData")

voice <- rbind(voice1, voice2)

survey_c <-
  data_voice_experiment %>%
  left_join(voice, by = c("tic" = "id"))

survey_c <- survey_c %>%
  mutate_at(c("e1_class", "e2_class", "e3_class"), as.factor) %>%
  mutate(e1_class = fct_recode(e1_class, 
                               neutral = "e1_neutral_m",
                               anger = "e1_anger_m", 
                               boredom = "e1_boredom_m",
                               disgust = "e1_disgust_m",
                               fear = "e1_fear_m",
                               happiness = "e1_happiness_m",
                               sadness = "e1_sadness_m"),
         e2_class = fct_recode(e2_class, 
                               neutral = "e2_neutral_m",
                               agressiv = "e2_agressiv_m", 
                               cheerful = "e2_cheerful_m",
                               intoxicated = "e2_intoxicated_m",
                               nervous = "e2_nervous_m",
                               tired = "e2_tired_m"),
         e3_class = fct_recode(e3_class, 
                               disinterest = "e3_loi1_m", 
                               normal = "e3_loi2_m",
                               high_interest = "e3_loi3_m"))

survey_c <- survey_c %>%
  mutate(gender = case_when(gender == 1 ~ "male",
                            gender == 2 ~ "female",
                            gender == 3 ~ "diverse"),
         age = case_when(birthyear == 1 ~ ">70",
                         birthyear == 2 ~ "66-70",
                         birthyear == 3 ~ "61-65",
                         birthyear == 4 ~ "56-60",
                         birthyear == 5 ~ "51-55",
                         birthyear == 6 ~ "46-50",
                         birthyear == 7 ~ "41-45",
                         birthyear == 8 ~ "36-40",
                         birthyear == 9 ~ "31-35",
                         birthyear == 10 ~ "26-30",
                         birthyear == 11 ~ "21-25",
                         birthyear == 12 ~ "<20"),
         edu = case_when(education == 1 ~ "in school",
                         education == 2 ~ "lower",
                         education == 3 ~ "lower",
                         education == 4 ~ "medium",
                         education == 5 ~ "higher",
                         education == 6 ~ "higher",
                         education == 7 ~ "higher"))

survey_c$e1_class <- relevel(survey_c$e1_class, ref = "neutral")
survey_c$e2_class <- relevel(survey_c$e2_class, ref = "neutral")

survey_c <- survey_c %>%
  mutate(feelings1 = case_when(feelings_aerger < 4 ~ "anger",
                               feelings_ekel < 4 ~ "disgust",
                               feelings_angst < 4 ~ "fear",
                               feelings_freude < 4 ~ "happiness",
                               feelings_traurgkeit < 4 ~ "sadness",
                               feelings_langeweile < 4 ~ "boredom",
                               feelings_ueberraschung < 4 ~ "surprise",
                               TRUE ~ "neutral"),
         feelings2 = case_when(feelings_aerger < 4 ~ "anger",
                               feelings_ekel < 4 ~ "disgust",
                               feelings_angst < 4 ~ "fear",
                               feelings_freude < 4 ~ "happiness",
                               feelings_traurgkeit < 4 ~ "sadness",
                               feelings_langeweile < 4 ~ "boredom",
                               feelings_ueberraschung < 4 ~ "happiness",
                               TRUE ~ "neutral"),
         svyinterest = case_when(survey_interest == 1 ~ "interesting",
                                 survey_interest == 2 ~ "interesting",
                                 survey_interest == 3 ~ "neutral",
                                 survey_interest == 4 ~ "neutral",
                                 survey_interest == 5 ~ "neutral",
                                 survey_interest == 6 ~ "not interesting",
                                 survey_interest >= 7 ~ "not interesting"),
         svyinterest2 = case_when(survey_interest == 1 ~ 7,
                                  survey_interest == 2 ~ 6,
                                  survey_interest == 3 ~ 5,
                                  survey_interest == 4 ~ 4,
                                  survey_interest == 5 ~ 3,
                                  survey_interest == 6 ~ 2,
                                  survey_interest == 7 ~ 1)) %>%
  mutate_at(c("feelings1", "feelings2", "svyinterest"), as.factor)

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

table(survey_c$e1_class, survey_c$e2_class)
table(survey_c$e1_class, survey_c$e3_class)

# Emotion Predictions and Survey Responses

survey_c %>%
  drop_na(e1_class) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e1_class, feelings1), fill = e1_class)) + 
  theme(legend.title = element_blank())

survey_c %>%
  drop_na(e1_class) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e2_class, feelings1), fill = e2_class)) + 
  theme(legend.title = element_blank())

survey_c %>%
  drop_na(e1_class) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e3_class, feelings1), fill = feelings1)) + 
  theme(legend.title = element_blank())

survey_c %>%
  drop_na(e1_class) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e3_class, svyinterest), fill = svyinterest)) + 
  theme(legend.title = element_blank())

confusionMatrix(survey_c$e1_class, reference = survey_c$feelings2)

table(survey_c$e1_class, survey_c$feelings1)
table(survey_c$e2_class, survey_c$feelings1)
table(survey_c$e3_class, survey_c$feelings1)

# Models

m1 <- lm(feelings_aerger ~ e1_class, data = survey_c)
m2 <- lm(feelings_ekel ~ e1_class, data = survey_c)
m3 <- lm(feelings_angst ~ e1_class, data = survey_c)
m4 <- lm(feelings_freude ~ e1_class, data = survey_c)
m5 <- lm(feelings_traurgkeit ~ e1_class, data = survey_c)
m6 <- lm(feelings_langeweile ~ e1_class, data = survey_c)
m7 <- lm(feelings_ueberraschung ~ e1_class, data = survey_c)

m8 <- lm(svyinterest2 ~ e1_class, data = survey_c)
m9 <- lm(svyinterest2 ~ e1_class + gender + age + edu, data = survey_c)
