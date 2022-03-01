### Authors: Konstantin Gavras, Christoph Kern

library(tidyverse)
library(stargazer)
library(sjPlot)
library(lme4)
library(lmerTest)
library(MuMIn)

###Regression analysis

glimpse(survey_c)

dataPaper <- survey_c %>% 
  mutate(party = as.factor(party),
         partyPreference = as.factor(partyPreference),
         partyPreference = fct_recode(partyPreference, Others = "FDP", Others = "Linke", Others = "NA"),
         partyPreference = relevel(partyPreference, ref = "Others"),
         e3_class = as.factor(e3_class),
         e3_class_m = relevel(e3_class, ref = "normal"),
         partyPref = ifelse(as.character(dataPaper$party) == as.character(dataPaper$partyPreference), "Yes", "No"))

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
  mutate(e3_loi3_c = case_when(e3_loi3_m >= e3_loi3_075 ~ "very high",
                               e3_loi3_m < e3_loi3_075 & e3_loi3_m >= e3_loi3_05 ~ "med high",
                               e3_loi3_m < e3_loi3_05 & e3_loi2_m >= e3_loi2_05 ~ "med low",
                               e3_loi3_m < e3_loi3_05 & e3_loi2_m < e3_loi2_05 ~ "low"),
         e3_loi3_c = as.factor(e3_loi3_c),
         e3_loi3_c = relevel(e3_loi3_c, ref = "low"))

dataPaper %>%
  filter(duration >= 4) %>%
  group_by(e3_class, e3_loi3_c) %>%
  tally() %>%
  spread(e3_loi3_c, n)

#Distribution of LOI
m0_distribution_m <- dataPaper %>% 
  filter(duration >= 4) %>%
  dplyr::select(party, contains("e3")) %>%
  drop_na() %>% 
  group_by(party) %>% 
  mutate(Count = n()) %>% 
  group_by(party, Count) %>% 
  summarize_at(vars(e3_loi1_m:e3_loi3_m), funs(mean,sd), na.rm = T)

m0_distribution_class <- dataPaper %>% 
  filter(duration >= 4) %>% 
  dplyr::select(party, e3_loi3_c) %>%
  drop_na() %>% 
  group_by(party, e3_loi3_c) %>% 
  summarize(Freq = n()) %>%
  mutate(per = Freq/sum(Freq))

#Mean duration and length of answers
meanDurationLength <- dataPaper %>% 
  filter(duration >= 4) %>% 
  group_by(party) %>% 
  summarize(duration_mean = mean(duration, na.rm = T),
            duration_q05 = quantile(duration, 0.05, na.rm = T),
            duration_median = median(duration, na.rm = T),
            duration_q95 = quantile(duration, 0.95, na.rm = T),
            duration_sd = sd(duration, na.rm = T),
            duration_skew = moments::skewness(duration, na.rm = T))

meanTokenLength <- dataPaper %>% 
  filter(duration >= 4) %>% 
  group_by(party) %>% 
  summarize(NToken_mean = mean(NToken, na.rm = T),
            NToken_q05 = quantile(NToken, 0.05, na.rm = T),
            NToken_median = median(NToken, na.rm = T),
            NToken_q95 = quantile(NToken, 0.95, na.rm = T),
            NToken_sd = sd(NToken, na.rm = T),
            NToken_skew = moments::skewness(NToken, na.rm = T))

meanDurationLengthLog <- dataPaper %>% 
  filter(duration >= 4) %>% 
  group_by(party) %>% 
  summarize(duration_mean = mean(log1p(duration), na.rm = T),
            duration_min = min(log1p(duration), na.rm = T),
            duration_median = median(log1p(duration), na.rm = T),
            duration_max = max(log1p(duration), na.rm = T),
            duration_sd = sd(log1p(duration), na.rm = T),
            duration_skew = moments::skewness(log1p(duration), na.rm = T))

meanTokenLengthLog <- dataPaper %>% 
  filter(duration >= 4) %>% 
  group_by(party) %>% 
  summarize(NToken_mean = mean(log1p(NToken), na.rm = T),
            NToken_min = min(log1p(NToken), na.rm = T),
            NToken_median = median(log1p(NToken), na.rm = T),
            NToken_max = max(log1p(NToken), na.rm = T),
            NToken_sd = sd(log1p(NToken), na.rm = T),
            NToken_skew = moments::skewness(log1p(NToken), na.rm = T))

N_question <- dataPaper %>% 
  filter(duration >= 4) %>% 
  group_by(party) %>% 
  drop_na(duration) %>% 
  summarize(N = n())

meanDurationLength4sec <- dataPaper %>% 
  filter(duration >= 4) %>% 
  summarize(duration_m = mean(duration, na.rm = T),
            NToken_m = mean(NToken, na.rm = T))

skewnessDurationLength4sec <- dataPaper %>% 
  filter(duration >= 4) %>% 
  summarize(duration_skew = moments::skewness(duration, na.rm = T),
            NToken_skew = moments::skewness(NToken, na.rm = T))


# Plot LOI vs Answer Length 
dataPaper %>%
  filter(duration >= 4) %>%
  ggplot(aes(e3_loi1_m, log(NToken))) +
  geom_point() +
  geom_smooth() +
  facet_grid(~ party)

dataPaper %>%
  filter(duration >= 4) %>%
  ggplot(aes(e3_loi2_m, log(NToken))) +
  geom_point() +
  geom_smooth() +
  facet_grid(~ party)

dataPaper %>%
  filter(duration >= 4) %>%
  ggplot(aes(e3_loi3_m, log(NToken))) +
  geom_point() +
  geom_smooth() +
  facet_grid(~ party)

dataPaper %>%
  filter(duration >= 4) %>%
  ggplot(aes(e3_loi1_m, log(duration))) +
  geom_point() +
  geom_smooth() +
  facet_grid(~ party)

dataPaper %>%
  filter(duration >= 4) %>%
  ggplot(aes(e3_loi2_m, log(duration))) +
  geom_point() +
  geom_smooth() +
  facet_grid(~ party)

dataPaper %>%
  filter(duration >= 4) %>%
  ggplot(aes(e3_loi3_m, log(duration))) +
  geom_point() +
  geom_smooth() +
  facet_grid(~ party)

dataPaper %>%
  filter(duration >= 4) %>%
  ggplot(aes(e3_loi3_m, log(NToken))) +
  geom_point() +
  geom_smooth() +
  facet_grid(party ~ partyPref)

dataPaper %>%
  filter(duration >= 4) %>%
  ggplot(aes(e3_loi3_m, sentiment_norm)) +
  geom_point() +
  geom_smooth() +
  facet_grid(party ~ partyPreference)


# Validity analysis
m1_validity <- lm(sentiment_norm ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party, data = dataPaper)

m2_validity <- lm(sentiment_norm ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party + age + female + education + partyPreference, data = dataPaper)

stargazer(m1_validity, m2_validity, title="Results", align=TRUE, type = "text")
stargazer(m1_validity, m2_validity, title="Results: Validity", align=TRUE, type = "html", out = "resultsPaper/models_validity.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))


# Validity analysis (>=5)
m1_validity <- lm(sentiment_norm ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party, data = dataPaper, subset = duration >= 5)

m2_validity <- lm(sentiment_norm ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party + age + female + education + partyPreference, data = dataPaper, subset = duration >= 5)

stargazer(m1_validity, m2_validity, title="Results", align=TRUE, type = "text", report=('vc*p'))
stargazer(m1_validity, m2_validity, title="Results: Validity", align=TRUE, type = "html", out = "resultsPaper/models_validity5sec.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
stargazer(m1_validity, m2_validity, title="Results: Validity", align=TRUE, type = "html", out = "resultsPaper/models_validity5sec_AAPOR.htm", no.space = T, single.row = T, digits = 2, report = "vcp", star.cutoffs = c(.05, .01, .001))


# Prediction of Answer Length - Words
m_lengthWords_Emotions <- lm(NToken ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party, data = dataPaper)

m_lengthWords_Sentiment <- lm(NToken ~ sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper)

m_lengthWords_Full <- lm(NToken ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper)

m_lengthWords_Controls <- lm(NToken ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + age + female + education, data = dataPaper)

stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results", align=TRUE, type = "text")
stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results: Length Words", align=TRUE, type = "html", out = "resultsPaper/models_LengthWords.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
plot_model(m_lengthWords_Controls, type = "pred", terms = "sentiment_norm")


# Prediction of Answer Length - Duration
m_lengthDuration_Emotions <- lm(duration ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party, data = dataPaper)

m_lengthDuration_Sentiment <- lm(duration ~ sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper)

m_lengthDuration_Full <- lm(duration ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper)

m_lengthDuration_Controls <- lm(duration ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + age + female + education, data = dataPaper)

stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results", align=TRUE, type = "text")
stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results: Duration", align=TRUE, type = "html", out = "resultsPaper/models_LengthDuration.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
plot_model(m_lengthDuration_Controls, type = "pred", terms = "sentiment_norm")


###Remove short answers (>=5)
# Prediction of Answer Length - Words
m_lengthWords_Emotions <- lm(NToken ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party, data = dataPaper, subset = duration >= 5)

m_lengthWords_Sentiment <- lm(NToken ~ sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 5)

m_lengthWords_Full <- lm(NToken ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 5)

m_lengthWords_Controls <- lm(NToken ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + age + female + education, data = dataPaper, subset = duration >= 5)

stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results", align=TRUE, type = "text")
stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results: Length Words", align=TRUE, type = "html", out = "resultsPaper/models_LengthWordsRobust5Sec.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
plot_model(m_lengthWords_Controls, type = "pred", terms = "sentiment_norm")


# Prediction of Answer Length - Duration
m_lengthDuration_Emotions <- lm(duration ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party, data = dataPaper, subset = duration >= 5)

m_lengthDuration_Sentiment <- lm(duration ~ sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 5)

m_lengthDuration_Full <- lm(duration ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 5)

m_lengthDuration_Controls <- lm(duration ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + age + female + education, data = dataPaper, subset = duration >= 5)

stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results", align=TRUE, type = "text")
stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results: Duration", align=TRUE, type = "html", out = "resultsPaper/models_LengthDurationRobust5Sec.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
plot_model(m_lengthDuration_Controls, type = "pred", terms = "sentiment_norm")


###Log transformation
# Prediction of Answer Length - Words
m_lengthWords_Emotions <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party, data = dataPaper, subset = duration >= 5)

m_lengthWords_Sentiment <- lm(log1p(NToken) ~ sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 5)

m_lengthWords_Full <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 5)

m_lengthWords_Controls <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + age + female + education, data = dataPaper, subset = duration >= 5)

stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))
stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results: Length Words", align=TRUE, type = "html", out = "resultsPaper/models_LengthWordsRobust5SecLOG.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results: Length Words", align=TRUE, type = "html", out = "resultsPaper/models_LengthWordsRobust5SecLOG_AAPOR.htm", no.space = T, single.row = T, digits = 2, report = "vcp", star.cutoffs = c(.05, .01, .001))
plot_model(m_lengthWords_Controls, type = "pred", terms = "sentiment_norm")


# Prediction of Answer Length - Duration
m_lengthDuration_Emotions <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party, data = dataPaper, subset = duration >= 5)

m_lengthDuration_Sentiment <- lm(log1p(duration) ~ sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 5)

m_lengthDuration_Full <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 5)

m_lengthDuration_Controls <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + age + female + education, data = dataPaper, subset = duration >= 5)

stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))
stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results: Duration", align=TRUE, type = "html", out = "resultsPaper/models_LengthDurationRobust5SecLOG.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results: Duration", align=TRUE, type = "html", out = "resultsPaper/models_LengthDurationRobust5SecLOG_AAPOR.htm", no.space = T, single.row = T, digits = 2, report = "vcp", star.cutoffs = c(.05, .01, .001))
plot_model(m_lengthDuration_Controls, type = "pred", terms = "sentiment_norm")


############ADD ANALYSIS###########

###Log transformation
# Prediction of Answer Length - Words

m_lengthWords_loi1 <- lm(log1p(NToken) ~ e3_loi1_m + I(e3_loi1_m^2), data = dataPaper, subset = duration >= 4)
m_lengthWords_loi2 <- lm(log1p(NToken) ~ e3_loi2_m + I(e3_loi2_m^2), data = dataPaper, subset = duration >= 4)
m_lengthWords_loi3 <- lm(log1p(NToken) ~ e3_loi3_m + I(e3_loi3_m^2), data = dataPaper, subset = duration >= 4)

plot_model(m_lengthWords_loi1, type = "eff", terms = "e3_loi1_m")
plot_model(m_lengthWords_loi2, type = "eff", terms = "e3_loi2_m")
plot_model(m_lengthWords_loi3, type = "eff", terms = "e3_loi3_m")

m_lengthWords_loi <- lm(log1p(NToken) ~ e3_loi3_c + party, data = dataPaper, subset = duration >= 4)
m_lengthWords_party <- lm(log1p(NToken) ~ e3_loi3_c + party + partyPref, data = dataPaper, subset = duration >= 4)
m_lengthWords_senti <- lm(log1p(NToken) ~ e3_loi3_c + party + partyPref + sentiment_norm + I(sentiment_norm^2), data = dataPaper, subset = duration >= 4)
m_lengthWords_controls <- lm(log1p(NToken) ~ e3_loi3_c + party + partyPref + sentiment_norm + I(sentiment_norm^2) + age + female + education + motherTongueGerman, data = dataPaper, subset = duration >= 4)

stargazer(m_lengthWords_loi, m_lengthWords_senti, m_lengthWords_controls, title="Results", align=TRUE, type = "text", report=('vc*p'))

# Multilevel models - Words
ml_lengthWords_loi <- lmer(log1p(NToken) ~ e3_loi3_c + (1 | lfdn), data = dataPaper, subset = duration >= 4)
ml_lengthWords_party <- lmer(log1p(NToken) ~ e3_loi3_c + party + partyPref + (1 | lfdn), data = dataPaper, subset = duration >= 4)
ml_lengthWords_senti <- lmer(log1p(NToken) ~ e3_loi3_c + party + partyPref + sentiment_norm + I(sentiment_norm^2) + (1 | lfdn), data = dataPaper, subset = duration >= 4)
ml_lengthWords_controls <- lmer(log1p(NToken) ~ e3_loi3_c + party + partyPref + sentiment_norm + I(sentiment_norm^2) + age + female + education + motherTongueGerman + (1 | lfdn), data = dataPaper, subset = duration >= 4)

multilevelR2(ml_lengthWords_loi)
multilevelR2(ml_lengthWords_party)
multilevelR2(ml_lengthWords_senti)
multilevelR2(ml_lengthWords_controls)

class(ml_lengthWords_loi) <- "lmerMod"
class(ml_lengthWords_party) <- "lmerMod"
class(ml_lengthWords_senti) <- "lmerMod"
class(ml_lengthWords_controls) <- "lmerMod"

stargazer(ml_lengthWords_loi, ml_lengthWords_party, ml_lengthWords_senti, ml_lengthWords_controls, report = ('vcsp'),
          omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, out = "models_LengthWords4SecLOG.html")


# Prediction of Answer Length - Duration

m_lengthWords_loi1 <- lm(log1p(duration) ~ e3_loi1_m + I(e3_loi1_m^2), data = dataPaper, subset = duration >= 4)
m_lengthWords_loi2 <- lm(log1p(duration) ~ e3_loi2_m + I(e3_loi2_m^2), data = dataPaper, subset = duration >= 4)
m_lengthWords_loi3 <- lm(log1p(duration) ~ e3_loi3_m + I(e3_loi3_m^2), data = dataPaper, subset = duration >= 4)

plot_model(m_lengthWords_loi1, type = "eff", terms = "e3_loi1_m")
plot_model(m_lengthWords_loi2, type = "eff", terms = "e3_loi2_m")
plot_model(m_lengthWords_loi3, type = "eff", terms = "e3_loi3_m")

m_lengthDuration_loi <- lm(log1p(duration) ~ e3_loi3_c + party, data = dataPaper, subset = duration >= 4)
m_lengthDuration_party <- lm(log1p(duration) ~ e3_loi3_c + party + partyPref, data = dataPaper, subset = duration >= 4)
m_lengthDuration_senti <- lm(log1p(duration) ~ e3_loi3_c + party + partyPref + sentiment_norm + I(sentiment_norm^2), data = dataPaper, subset = duration >= 4)
m_lengthDuration_controls <- lm(log1p(duration) ~ e3_loi3_c + party + partyPref + sentiment_norm + I(sentiment_norm^2) + age + female + education + motherTongueGerman, data = dataPaper, subset = duration >= 4)

stargazer(m_lengthDuration_loi, m_lengthDuration_senti, m_lengthDuration_controls, title="Results", align=TRUE, type = "text", report=('vc*p'))

# Multilevel models - Duration
ml_lengthDuration_loi <- lmer(log1p(duration) ~ e3_loi3_c + (1 | lfdn), data = dataPaper, subset = duration >= 4)
ml_lengthDuration_party <- lmer(log1p(duration) ~ e3_loi3_c + party + partyPref + (1 | lfdn), data = dataPaper, subset = duration >= 4)
ml_lengthDuration_senti <- lmer(log1p(duration) ~ e3_loi3_c + party + partyPref + sentiment_norm + I(sentiment_norm^2) + (1 | lfdn), data = dataPaper, subset = duration >= 4)
ml_lengthDuration_controls <- lmer(log1p(duration) ~ e3_loi3_c + party + partyPref + sentiment_norm + I(sentiment_norm^2) + age + female + education + motherTongueGerman + (1 | lfdn), data = dataPaper, subset = duration >= 4)

multilevelR2(ml_lengthDuration_loi)
multilevelR2(ml_lengthDuration_party)
multilevelR2(ml_lengthDuration_senti)
multilevelR2(ml_lengthDuration_controls)

class(ml_lengthDuration_loi) <- "lmerMod"
class(ml_lengthDuration_party) <- "lmerMod"
class(ml_lengthDuration_senti) <- "lmerMod"
class(ml_lengthDuration_controls) <- "lmerMod"

stargazer(ml_lengthDuration_loi, ml_lengthDuration_party, ml_lengthDuration_senti, ml_lengthDuration_controls, report = ('vcsp'),
          omit.table.layout = "n", align = TRUE, no.space = TRUE, out.header = T, out = "models_LengthDuration4SecLOG.html")
