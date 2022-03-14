
#Main analysis

library(tidyverse)
library(stargazer)
library(sjPlot)
library(lme4)
library(lmerTest)
library(mitml)
library(ordinal)

load("voiceData.RData")

glimpse(survey_c)

# Data prep
dataPaper <- survey_c %>% 
  mutate(party = as.factor(party),
         party = fct_relevel(party, "CDU", "SPD", "Greens", "AfD"),
         partyPreference = as.factor(partyPreference),
         partyPreference = fct_recode(partyPreference, Others = "FDP", Others = "Linke", Others = "NA"),
         partyPreference = relevel(partyPreference, ref = "Others"),
         e3_class = as.factor(e3_class),
         e3_class_m = relevel(e3_class, ref = "normal"),
         partyPref = ifelse(as.character(party) == as.character(partyPreference), "Yes", "No"))

dataPaper <- dataPaper %>% 
  mutate(minsentiment = min(sentiment, na.rm = T),
         maxsentiment = max(sentiment, na.rm = T),
         diffsentiment = maxsentiment - minsentiment,
         sentiment_norm = sentiment - minsentiment,
         sentiment_norm = if_else(diffsentiment != 0, sentiment_norm / diffsentiment, sentiment_norm))

e3_loi3_075 <- quantile(dataPaper$e3_loi3_m, 0.75, na.rm = T)
e3_loi3_05 <- quantile(dataPaper$e3_loi3_m, 0.5, na.rm = T)
e3_loi2_05 <- quantile(dataPaper$e3_loi2_m, 0.5, na.rm = T)

dataPaper <- dataPaper %>% 
  mutate(e3_loi_c = case_when(e3_loi3_m >= e3_loi3_075 ~ "high",
                              e3_loi3_m < e3_loi3_075 & e3_loi3_m >= e3_loi3_05 ~ "med high",
                              e3_loi3_m < e3_loi3_05 & e3_loi2_m >= e3_loi2_05 ~ "med low",
                              e3_loi3_m < e3_loi3_05 & e3_loi2_m < e3_loi2_05 ~ "low"),
         e3_loi_c = as.factor(e3_loi_c),
         e3_loi_c = fct_relevel(e3_loi_c, "low", "med low", "med high", "high"))

dataPaper %>%
  filter(duration >= 2) %>%
  group_by(e3_class, e3_loi_c) %>%
  tally() %>%
  spread(e3_loi_c, n)

#Aggregate on respondent level 
dataResp <- dataPaper %>%
  filter(no_voice == 0) %>%
  group_by(lfdn) %>%
  summarize(e3_loi1_mm = mean(e3_loi1_m, na.rm = T),
            e3_loi2_mm = mean(e3_loi2_m, na.rm = T),
            e3_loi3_mm = mean(e3_loi3_m, na.rm = T),
            e3_loi1_mv = var(e3_loi1_m, na.rm = T),
            e3_loi2_mv = var(e3_loi2_m, na.rm = T),
            e3_loi3_mv = var(e3_loi3_m, na.rm = T),
            e3_loi_cm = names(which.max(table(e3_loi_c))),
            e3_class_m = names(which.max(table(e3_class))),
            e3_loi_cm = as.factor(e3_loi_cm),
            e3_loi_cm = fct_relevel(e3_loi_cm, "low", "med low", "med high", "high"),
            e3_class_m = as.factor(e3_class_m),
            e3_class_m = relevel(e3_class_m, ref = "disinterest"),
            NToken_m = mean(NToken, na.rm = T),
            duration_m = mean(duration, na.rm = T),
            surveyinterest = max(surveyInterest),
            surveyint = as.factor(surveyinterest),
            surveyint = fct_relevel(surveyint, "0", "1", "2", "3", "4", "5", "6"),
            age = max(age),
            education = names(which.max(table(education))), 
            motherTongueGerman = max(motherTongueGerman))

dataResp %>%
  group_by(e3_class_m, e3_loi_cm) %>%
  tally() %>%
  spread(e3_loi_cm, n)

#Distribution of LOI
m0_distribution_m <- dataPaper %>% 
  filter(duration >= 2) %>%
  dplyr::select(party, contains("e3")) %>%
  drop_na() %>% 
  group_by(party) %>% 
  mutate(Count = n()) %>% 
  group_by(party, Count) %>% 
  summarize_at(vars(e3_loi1_m:e3_loi3_m), funs(mean,sd), na.rm = T)
stargazer(as.data.frame(m0_distribution_m), summary = FALSE, digits = 2,
          out = "desc_m0_distribution_m.html")

m0_distribution_e3_loi_c <- dataPaper %>% 
  filter(duration >= 2) %>% 
  dplyr::select(party, e3_loi_c) %>%
  drop_na() %>% 
  group_by(party, e3_loi_c) %>% 
  summarize(Freq = n()) %>%
  mutate(per = Freq/sum(Freq))
stargazer(as.data.frame(m0_distribution_e3_loi_c), summary = FALSE, digits = 2,
          out = "desc_m0_distribution_e3_loi_c.html")

m0_distribution_e3_class <- dataPaper %>% 
  filter(duration >= 2) %>% 
  dplyr::select(party, e3_class) %>%
  drop_na() %>% 
  group_by(party, e3_class) %>% 
  summarize(Freq = n()) %>%
  mutate(per = Freq/sum(Freq))
stargazer(as.data.frame(m0_distribution_e3_class), summary = FALSE, digits = 2,
          out = "desc_m0_distribution_e3_class.html")

#Mean duration and length of answers
meanDurationLength <- dataPaper %>% 
  filter(duration >= 2) %>% 
  group_by(party) %>% 
  summarize(duration_mean = mean(duration, na.rm = T),
            duration_q05 = quantile(duration, 0.05, na.rm = T),
            duration_median = median(duration, na.rm = T),
            duration_q95 = quantile(duration, 0.95, na.rm = T),
            duration_sd = sd(duration, na.rm = T),
            duration_skew = moments::skewness(duration, na.rm = T))
stargazer(as.data.frame(meanDurationLength), summary = FALSE, digits = 2,
          out = "desc_meanDurationLength.html")

meanTokenLength <- dataPaper %>% 
  filter(duration >= 2) %>% 
  group_by(party) %>% 
  summarize(NToken_mean = mean(NToken, na.rm = T),
            NToken_q05 = quantile(NToken, 0.05, na.rm = T),
            NToken_median = median(NToken, na.rm = T),
            NToken_q95 = quantile(NToken, 0.95, na.rm = T),
            NToken_sd = sd(NToken, na.rm = T),
            NToken_skew = moments::skewness(NToken, na.rm = T))
stargazer(as.data.frame(meanTokenLength), summary = FALSE, digits = 2,
          out = "desc_meanTokenLength.html")

meanDurationLengthLog <- dataPaper %>% 
  filter(duration >= 2) %>% 
  group_by(party) %>% 
  summarize(duration_mean = mean(log1p(duration), na.rm = T),
            duration_q05 = quantile(log1p(duration), 0.05, na.rm = T),
            duration_median = median(log1p(duration), na.rm = T),
            duration_q95 = quantile(log1p(duration), 0.95, na.rm = T),
            duration_sd = sd(log1p(duration), na.rm = T),
            duration_skew = moments::skewness(log1p(duration), na.rm = T))
stargazer(as.data.frame(meanDurationLengthLog), summary = FALSE, digits = 2,
          out = "desc_meanDurationLengthLog.html")

meanTokenLengthLog <- dataPaper %>% 
  filter(duration >= 2) %>% 
  group_by(party) %>% 
  summarize(NToken_mean = mean(log1p(NToken), na.rm = T),
            NToken_q05 = quantile(log1p(NToken), 0.05, na.rm = T),
            NToken_median = median(log1p(NToken), na.rm = T),
            NToken_q95 = quantile(log1p(NToken), 0.95, na.rm = T),
            NToken_sd = sd(log1p(NToken), na.rm = T),
            NToken_skew = moments::skewness(log1p(NToken), na.rm = T))
stargazer(as.data.frame(meanTokenLengthLog), summary = FALSE, digits = 2,
          out = "desc_meanTokenLengthLog.html")

N_question <- dataPaper %>% 
  filter(duration >= 2) %>% 
  group_by(party) %>% 
  drop_na(duration) %>% 
  summarize(N = n())

# Plot LOI vs Answer Length 
dataPaper %>%
  filter(duration >= 2) %>%
  ggplot(aes(e3_loi1_m, log(NToken))) +
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth() +
  facet_grid(~ party) +
  labs(y = "log(Words)", x = "Predicted probability: Low Interest") +
  theme(panel.spacing.x = unit(4, "mm"),
        text = element_text(size = 14))

ggsave("p1_loi1-words.png", width = 9, height = 6)

dataPaper %>%
  filter(duration >= 2) %>%
  ggplot(aes(e3_loi2_m, log(NToken))) +
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth() +
  facet_grid(~ party) +
  labs(y = "log(Words)", x = "Predicted probability: Normal Interest") +
  theme(panel.spacing.x = unit(4, "mm"),
        text = element_text(size = 14))

ggsave("p2_loi2-words.png", width = 9, height = 6)

dataPaper %>%
  filter(duration >= 2) %>%
  ggplot(aes(e3_loi3_m, log(NToken))) +
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth() +
  facet_grid(~ party) + 
  labs(y = "log(Words)", x = "Predicted probability: High Interest") +
  theme(panel.spacing.x = unit(4, "mm"),
        text = element_text(size = 14))

ggsave("p3_loi3-words.png", width = 9, height = 6)

dataPaper %>%
  filter(duration >= 2) %>%
  ggplot(aes(e3_loi1_m, log(duration))) +
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth() +
  facet_grid(~ party) +
  labs(y = "log(Duration)", x = "Predicted probability: Low Interest") +
  theme(panel.spacing.x = unit(4, "mm"),
        text = element_text(size = 14))

ggsave("p4_loi1-duration.png", width = 9, height = 6)

dataPaper %>%
  filter(duration >= 2) %>%
  ggplot(aes(e3_loi2_m, log(duration))) +
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth() +
  facet_grid(~ party) +
  labs(y = "log(Duration)", x = "Predicted probability: Normal Interest") +
  theme(panel.spacing.x = unit(4, "mm"),
        text = element_text(size = 14))

ggsave("p5_loi2-duration.png", width = 9, height = 6)

dataPaper %>%
  filter(duration >= 2) %>%
  ggplot(aes(e3_loi3_m, log(duration))) +
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth() +
  facet_grid(~ party) +
  labs(y = "log(Duration)", x = "Predicted probability: High Interest") +
  theme(panel.spacing.x = unit(4, "mm"),
        text = element_text(size = 14))

ggsave("p6_loi3-duration.png", width = 9, height = 6)

dataPaper %>%
  filter(duration >= 2) %>%
  ggplot(aes(e3_loi3_m, sentiment_norm)) +
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth() +
  facet_grid(party ~ partyPreference)

dataPaper %>%
  filter(duration >= 2) %>%
  ggplot(aes(e3_loi3_m, log(NToken))) +
  geom_point(size = 1, alpha = 0.25) +
  geom_smooth() +
  facet_grid(party ~ partyPref)

dataPaper %>%
  filter(duration >= 2) %>%
  ggplot() +
  geom_boxplot(aes(e3_loi3_m, fill = partyPref)) +
  facet_grid(~ party)


### Validity analysis
m0t_validity <- lm(surveyinterest ~ NToken_m, data = dataResp)
m0d_validity <- lm(surveyinterest ~ duration_m, data = dataResp)

m1a_validity <- clm(surveyint ~ e3_loi1_mm + e3_loi1_mv, link = "probit", data = dataResp)
m1b_validity <- clm(surveyint ~ e3_loi1_mm*e3_loi1_mv, link = "probit", data = dataResp)
m1c_validity <- clm(surveyint ~ e3_loi1_mm*e3_loi1_mv + age + education + motherTongueGerman, link = "probit", data = dataResp)
stargazer(m1a_validity, m1b_validity, report = ('vcsp'),
          keep = c("Constant", "e3_loi1_mm", "e3_loi1_mv", "e3_loi1_mm:e3_loi1_mv"),
          omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, out = "models_svyinterestloi1.html")

plot_model(m1c_validity, type = "pred", terms = c("e3_loi1_mm [all]", "e3_loi1_mv"),
           title = "", legend.title = "Variance of\nPred. prob.:\nLow Interest",
           axis.title = c("Predicted probability: Low Interest", "Survey Interest")) +
  theme_grey(base_size = 14)
ggsave("pv_loi1-interest.png", width = 9, height = 6)

m2a_validity <- clm(surveyint ~ e3_loi3_mm + e3_loi3_mv, link = "probit", data = dataResp)
m2b_validity <- clm(surveyint ~ e3_loi3_mm*e3_loi3_mv, link = "probit", data = dataResp)
m2c_validity <- clm(surveyint ~ e3_loi3_mm*e3_loi3_mv + age + education + motherTongueGerman, link = "probit", data = dataResp)
stargazer(m2a_validity, m2b_validity, report = ('vcsp'),
          keep = c("Constant", "e3_loi3_mm", "e3_loi3_mv", "e3_loi3_mm:e3_loi3_mv"),
          omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, out = "models_svyinterestloi3.html")

plot_model(m2c_validity, type = "pred", terms = c("e3_loi3_mm [all]", "e3_loi3_mv"),
           title = "", legend.title = "Variance of\nPred. prob.:\nHigh Interest",
           axis.title = c("Predicted probability: High Interest", "Survey Interest")) +
  theme_grey(base_size = 14)
ggsave("pv_loi3-interest.png", width = 9, height = 6)

m3a_validity <- lm(surveyinterest ~ e3_loi_cm, data = dataResp)
m3b_validity <- lm(surveyinterest ~ e3_loi_cm + NToken_m, data = dataResp)
m4a_validity <- lm(surveyinterest ~ e3_class_m, data = dataResp)
m4b_validity <- lm(surveyinterest ~ e3_class_m + NToken_m, data = dataResp)


### LOI models I
## Prediction of Answer Length - Words

# Non-linear effects
m_lengthWords_loi1 <- lm(log1p(NToken) ~ e3_loi1_m + I(e3_loi1_m^2), data = dataPaper, subset = duration >= 2)
m_lengthWords_loi2 <- lm(log1p(NToken) ~ e3_loi2_m + I(e3_loi2_m^2), data = dataPaper, subset = duration >= 2)
m_lengthWords_loi3 <- lm(log1p(NToken) ~ e3_loi3_m + I(e3_loi3_m^2), data = dataPaper, subset = duration >= 2)

plot_model(m_lengthWords_loi1, type = "eff", terms = "e3_loi1_m")
plot_model(m_lengthWords_loi2, type = "eff", terms = "e3_loi2_m")
plot_model(m_lengthWords_loi3, type = "eff", terms = "e3_loi3_m")

# Regression models
m_lengthWords_loi <- lm(log1p(NToken) ~ e3_loi_c, data = dataPaper, subset = duration >= 2)
m_lengthWords_senti <- lm(log1p(NToken) ~ sentiment_norm + I(sentiment_norm^2), data = dataPaper, subset = duration >= 2)
m_lengthWords_party <- lm(log1p(NToken) ~ e3_loi_c + sentiment_norm + I(sentiment_norm^2) + party + partyPref, data = dataPaper, subset = duration >= 2)
m_lengthWords_controls <- lm(log1p(NToken) ~ e3_loi_c + sentiment_norm + I(sentiment_norm^2) + party + partyPref + age + education + motherTongueGerman, data = dataPaper, subset = duration >= 2)

stargazer(m_lengthWords_loi, m_lengthWords_senti, m_lengthWords_controls, title="Results", align=TRUE, type = "text", report=('vc*p'))

# Multilevel models - e3_loi_c
ml_lengthWords_loi <- lmer(log1p(NToken) ~ e3_loi_c + (1 | lfdn), data = dataPaper, subset = duration >= 2)
ml_lengthWords_senti <- lmer(log1p(NToken) ~ sentiment_norm + I(sentiment_norm^2) + (1 | lfdn), data = dataPaper, subset = duration >= 2)
ml_lengthWords_party <- lmer(log1p(NToken) ~ e3_loi_c + sentiment_norm + I(sentiment_norm^2) + party + partyPref + (1 | lfdn), data = dataPaper, subset = duration >= 2)
ml_lengthWords_controls <- lmer(log1p(NToken) ~ e3_loi_c + sentiment_norm + I(sentiment_norm^2) + party + partyPref + age + education + motherTongueGerman + (1 | lfdn), data = dataPaper, subset = duration >= 2)

multilevelR2(ml_lengthWords_loi)
multilevelR2(ml_lengthWords_senti)
multilevelR2(ml_lengthWords_party)
multilevelR2(ml_lengthWords_controls)

class(ml_lengthWords_loi) <- "lmerMod"
class(ml_lengthWords_senti) <- "lmerMod"
class(ml_lengthWords_party) <- "lmerMod"
class(ml_lengthWords_controls) <- "lmerMod"

stargazer(ml_lengthWords_loi, ml_lengthWords_senti, ml_lengthWords_party, ml_lengthWords_controls, report = ('vcsp'),
          keep = c("Constant", "e3_loi_cmed low", "e3_loi_cmed high", "e3_loi_chigh", "sentiment_norm", "I(sentiment_norm^2)"),
          add.lines = list(c("Party preference", "", "", "X", "X"), c("Demographic controls", "", "", "", "X")),
          omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, out = "models_LengthWords2SecLOG.html")

# Multilevel models - e3_class
ml_lengthWords_loi <- lmer(log1p(NToken) ~ e3_class + (1 | lfdn), data = dataPaper, subset = duration >= 2)
ml_lengthWords_party <- lmer(log1p(NToken) ~ e3_class + sentiment_norm + I(sentiment_norm^2) + party + partyPref + (1 | lfdn), data = dataPaper, subset = duration >= 2)
ml_lengthWords_controls <- lmer(log1p(NToken) ~ e3_class + sentiment_norm + I(sentiment_norm^2) + party + partyPref + age + education + motherTongueGerman + (1 | lfdn), data = dataPaper, subset = duration >= 2)

multilevelR2(ml_lengthWords_loi)
multilevelR2(ml_lengthWords_party)
multilevelR2(ml_lengthWords_controls)

class(ml_lengthWords_loi) <- "lmerMod"
class(ml_lengthWords_party) <- "lmerMod"
class(ml_lengthWords_controls) <- "lmerMod"

stargazer(ml_lengthWords_loi, ml_lengthWords_party, ml_lengthWords_controls, report = ('vcsp'),
          keep = c("Constant", "e3_classnormal", "e3_classhigh_interest", "sentiment_norm", "I(sentiment_norm^2)"),
          add.lines = list(c("Party preference", "", "X", "X"), c("Demographic controls", "", "", "X")),
          omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, out = "models_LengthWords2SecLOGclass.html")


### LOI models II
## Prediction of Answer Length - Duration

# Non-linear effects
m_lengthWords_loi1 <- lm(log1p(duration) ~ e3_loi1_m + I(e3_loi1_m^2), data = dataPaper, subset = duration >= 2)
m_lengthWords_loi2 <- lm(log1p(duration) ~ e3_loi2_m + I(e3_loi2_m^2), data = dataPaper, subset = duration >= 2)
m_lengthWords_loi3 <- lm(log1p(duration) ~ e3_loi3_m + I(e3_loi3_m^2), data = dataPaper, subset = duration >= 2)

plot_model(m_lengthWords_loi1, type = "eff", terms = "e3_loi1_m")
plot_model(m_lengthWords_loi2, type = "eff", terms = "e3_loi2_m")
plot_model(m_lengthWords_loi3, type = "eff", terms = "e3_loi3_m")

# Regression models
m_lengthDuration_loi <- lm(log1p(duration) ~ e3_loi_c, data = dataPaper, subset = duration >= 2)
m_lengthDuration_senti <- lm(log1p(duration) ~ sentiment_norm + I(sentiment_norm^2), data = dataPaper, subset = duration >= 2)
m_lengthDuration_party <- lm(log1p(duration) ~ e3_loi_c + sentiment_norm + I(sentiment_norm^2) + party + partyPref, data = dataPaper, subset = duration >= 2)
m_lengthDuration_controls <- lm(log1p(duration) ~ e3_loi_c + sentiment_norm + I(sentiment_norm^2) + party + partyPref + age + education + motherTongueGerman, data = dataPaper, subset = duration >= 2)

stargazer(m_lengthDuration_loi, m_lengthDuration_senti, m_lengthDuration_controls, title="Results", align=TRUE, type = "text", report=('vc*p'))

# Multilevel models - e3_loi_c
ml_lengthDuration_loi <- lmer(log1p(duration) ~ e3_loi_c + (1 | lfdn), data = dataPaper, subset = duration >= 2)
ml_lengthDuration_senti <- lmer(log1p(duration) ~ sentiment_norm + I(sentiment_norm^2) + (1 | lfdn), data = dataPaper, subset = duration >= 2)
ml_lengthDuration_party <- lmer(log1p(duration) ~ e3_loi_c + sentiment_norm + I(sentiment_norm^2) + party + partyPref + (1 | lfdn), data = dataPaper, subset = duration >= 2)
ml_lengthDuration_controls <- lmer(log1p(duration) ~ e3_loi_c + sentiment_norm + I(sentiment_norm^2) + party + partyPref + age + education + motherTongueGerman + (1 | lfdn), data = dataPaper, subset = duration >= 2)

multilevelR2(ml_lengthDuration_loi)
multilevelR2(ml_lengthDuration_senti)
multilevelR2(ml_lengthDuration_party)
multilevelR2(ml_lengthDuration_controls)

class(ml_lengthDuration_loi) <- "lmerMod"
class(ml_lengthDuration_senti) <- "lmerMod"
class(ml_lengthDuration_party) <- "lmerMod"
class(ml_lengthDuration_controls) <- "lmerMod"

stargazer(ml_lengthDuration_loi, ml_lengthDuration_senti, ml_lengthDuration_party, ml_lengthDuration_controls, report = ('vcsp'),
          keep = c("Constant", "e3_loi_cmed low", "e3_loi_cmed high", "e3_loi_chigh", "sentiment_norm", "I(sentiment_norm^2)"),
          add.lines = list(c("Party preference", "", "", "X", "X"), c("Demographic controls", "", "", "", "X")),
          omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, out = "models_LengthDuration2SecLOG.html")

# Multilevel models - e3_class
ml_lengthDuration_loi <- lmer(log1p(duration) ~ e3_class + (1 | lfdn), data = dataPaper, subset = duration >= 2)
ml_lengthDuration_party <- lmer(log1p(duration) ~ e3_class + sentiment_norm + I(sentiment_norm^2) + party + partyPref + (1 | lfdn), data = dataPaper, subset = duration >= 2)
ml_lengthDuration_controls <- lmer(log1p(duration) ~ e3_class + sentiment_norm + I(sentiment_norm^2) + party + partyPref + age + education + motherTongueGerman + (1 | lfdn), data = dataPaper, subset = duration >= 2)

multilevelR2(ml_lengthDuration_loi)
multilevelR2(ml_lengthDuration_party)
multilevelR2(ml_lengthDuration_controls)

class(ml_lengthDuration_loi) <- "lmerMod"
class(ml_lengthDuration_party) <- "lmerMod"
class(ml_lengthDuration_controls) <- "lmerMod"

stargazer(ml_lengthDuration_loi, ml_lengthDuration_party, ml_lengthDuration_controls, report = ('vcsp'),
          keep = c("Constant", "e3_classnormal", "e3_classhigh_interest", "sentiment_norm", "I(sentiment_norm^2)"),
          add.lines = list(c("Party preference", "", "X", "X"), c("Demographic controls", "", "", "X")),
          omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, out = "models_LengthDuration2SecLOGclass.html")

