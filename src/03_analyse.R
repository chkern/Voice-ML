library(tidyverse)
library(ggmosaic)
library(caret)
library(stargazer)

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

survey_c$e1_class <- relevel(survey_c$e1_class, ref = "boredom")
survey_c$e2_class <- relevel(survey_c$e2_class, ref = "tired")

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
         svyinterest = case_when(survey_interest == 1 ~ 7,
                                 survey_interest == 2 ~ 6,
                                 survey_interest == 3 ~ 5,
                                 survey_interest == 4 ~ 4,
                                 survey_interest == 5 ~ 3,
                                 survey_interest == 6 ~ 2,
                                 survey_interest == 7 ~ 1),
         svydifficult = case_when(survey_difficulty == 1 ~ 7,
                                  survey_difficulty == 2 ~ 6,
                                  survey_difficulty == 3 ~ 5,
                                  survey_difficulty == 4 ~ 4,
                                  survey_difficulty == 5 ~ 3,
                                  survey_difficulty == 6 ~ 2,
                                  survey_difficulty == 7 ~ 1)) %>%
  mutate_at(c("feelings1", "feelings2"), as.factor)

# Description Emotion Predictions

survey_c %>%
  drop_na(e1_class) %>%
  ggplot() +
  geom_bar(aes(x = e1_class))

tab1 <- survey_c %>% filter(n_preds >= 1) %>% count(e1_class) %>% mutate(prop = prop.table(n)) %>% arrange(desc(prop))
tab2 <- survey_c %>% filter(n_preds >= 2) %>% count(e1_class) %>% mutate(prop = prop.table(n)) %>% arrange(desc(prop))

tab <- bind_cols(tab1, tab2[, 2:3]) %>% mutate(prop...3 = round(prop...3, 3),
                                               prop...5 = round(prop...5, 3))

stargazer(tab, summary = F, out = "emo_t1.tex")

survey_c %>%
  drop_na(e2_class) %>%
  ggplot() +
  geom_bar(aes(x = e2_class))

tab1 <- survey_c %>% filter(n_preds >= 1) %>% count(e2_class) %>% mutate(prop = prop.table(n)) %>% arrange(desc(prop))
tab2 <- survey_c %>% filter(n_preds >= 2) %>% count(e2_class) %>% mutate(prop = prop.table(n)) %>% arrange(desc(prop))

tab <- bind_cols(tab1, tab2[, 2:3]) %>% mutate(prop...3 = round(prop...3, 3),
                                               prop...5 = round(prop...5, 3))

stargazer(tab, summary = F, out = "emo_t2.tex")

tab1 <- survey_c %>% filter(n_preds >= 1) %>% count(e3_class) %>% mutate(prop = prop.table(n)) %>% arrange(desc(prop))
tab2 <- survey_c %>% filter(n_preds >= 2) %>% count(e3_class) %>% mutate(prop = prop.table(n)) %>% arrange(desc(prop))

tab <- bind_cols(tab1, tab2[, 2:3]) %>% mutate(prop...3 = round(prop...3, 3),
                                               prop...5 = round(prop...5, 3))

stargazer(tab, summary = F, out = "emo_t3.tex")

p <- survey_c %>%
  drop_na(e1_class) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e1_class, e2_class), fill = e1_class)) + 
  labs(x = "Predicted Emotions (abcAffect)", y = "Predicted Emotions (emodb)") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

temp <- ggplot_build(p)$data[[1]] %>% mutate(wt = ifelse(.wt < 5, NA, .wt))
p + geom_text(data = temp, aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=wt))

ggsave("emo1-1.pdf", width = 9, height = 7)

p <- survey_c %>%
  drop_na(e1_class) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e1_class, e3_class), fill = e1_class))  + 
  labs(x = "Predicted Interest (avic)", y = "Predicted Emotions (emodb)") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

temp <- ggplot_build(p)$data[[1]] %>% mutate(wt = ifelse(.wt < 5, NA, .wt))
p + geom_text(data = temp, aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=wt))

ggsave("emo1-2.pdf", width = 9, height = 7)

table(survey_c$e1_class, survey_c$e2_class)
table(survey_c$e1_class, survey_c$e3_class)

# Emotion Predictions and Survey Responses

p <- survey_c %>%
  drop_na(e1_class) %>%
 # filter(n_preds >= 2) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e1_class, feelings1), fill = e1_class)) + 
  labs(x = "Feelings (Survey)", y = "Predicted Emotions (emodb)") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

temp <- ggplot_build(p)$data[[1]] %>% mutate(wt = ifelse(.wt < 5, NA, .wt))
p + geom_text(data = temp, aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=wt))

ggsave("emo2-1.pdf", width = 9, height = 7)

p <- survey_c %>%
  drop_na(e2_class) %>%
 # filter(n_preds >= 2) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e2_class, feelings1), fill = e2_class)) + 
  labs(x = "Feelings (Survey)", y = "Predicted Emotions (abcAffect)") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

temp <- ggplot_build(p)$data[[1]] %>% mutate(wt = ifelse(.wt < 5, NA, .wt))
p + geom_text(data = temp, aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=wt))

ggsave("emo2-2.pdf", width = 9, height = 7)

p <- survey_c %>%
  drop_na(e3_class) %>%
 # filter(n_preds >= 2) %>%
  ggplot() +
  geom_mosaic(aes(x = product(e3_class, feelings1), fill = e3_class))  + 
  labs(x = "Feelings (Survey)", y = "Predicted Interest (avic)") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

temp <- ggplot_build(p)$data[[1]] %>% mutate(wt = ifelse(.wt < 5, NA, .wt))
p + geom_text(data = temp, aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=wt))

ggsave("emo2-3.pdf", width = 9, height = 7)

confusionMatrix(survey_c$e1_class, reference = survey_c$feelings2)

table(survey_c$e1_class, survey_c$feelings1)
table(survey_c$e2_class, survey_c$feelings1)
table(survey_c$e3_class, survey_c$feelings1)

cor(survey_c[, c(10:16, 27:33)], use = "pairwise.complete.obs")

# Models

m1 <- lm(feelings_aerger ~ e1_class, data = survey_c)
m2 <- lm(feelings_ekel ~ e1_class, data = survey_c)
m3 <- lm(feelings_angst ~ e1_class, data = survey_c)
m4 <- lm(feelings_freude ~ e1_class, data = survey_c)
m5 <- lm(feelings_traurgkeit ~ e1_class, data = survey_c)
m6 <- lm(feelings_langeweile ~ e1_class, data = survey_c)
m7 <- lm(feelings_ueberraschung ~ e1_class, data = survey_c)

m8 <- lm(svyinterest ~ e1_class, data = survey_c)
m9 <- lm(svyinterest ~ e1_class + gender + age + edu, data = survey_c)

stargazer(m8, m9, 
          keep = c("Constant", "e1_classneutral", "e1_classanger", "e1_classdisgust", "e1_classfear", "e1_classhappiness", "e1_classsadness"), 
          report = ('vcsp'), 
          add.lines = list(c("Demographic controls", "", "Yes")), 
          omit.stat = c("adj.rsq", "ser"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "emo_m1.tex")

m10 <- lm(svydifficult ~ e1_class, data = survey_c)
m11 <- lm(svydifficult ~ e1_class + gender + age + edu, data = survey_c)

stargazer(m10, m11, 
          keep = c("Constant", "e1_classneutral", "e1_classanger", "e1_classdisgust", "e1_classfear", "e1_classhappiness", "e1_classsadness"), 
          report = ('vcsp'), 
          add.lines = list(c("Demographic controls", "", "Yes")), 
          omit.stat = c("adj.rsq", "ser"), omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, 
          out = "emo_m2.tex")
