### Author: Konstantin Gavras

###Regression analysis

glimpse(survey_c)

dataPaper <- survey_c %>% 
  mutate(party = as.factor(party),
         partyPreference = as.factor(partyPreference),
         partyPreference = fct_recode(partyPreference, Others = "FDP", Others = "Linke", Others = "NA"),
         partyPreference = relevel(partyPreference, ref = "Others"))


dataPaper <- dataPaper %>% 
  mutate(minsentiment = min(sentiment, na.rm = T),
         maxsentiment = max(sentiment, na.rm = T),
         diffsentiment = maxsentiment - minsentiment,
         sentiment_norm = sentiment - minsentiment,
         sentiment_norm = if_else(diffsentiment != 0, sentiment_norm / diffsentiment, sentiment_norm))


#Distribution of Emotions
m0_distribution <- dataPaper %>% 
  dplyr::select(party, contains("e1")) %>%
  drop_na() %>% 
  group_by(party) %>% 
  mutate(Count = n()) %>% 
  group_by(party, Count) %>% 
  summarize_at(vars(e1_anger_m:e1_sadness_m), funs(mean,sd), na.rm = T)

m0_distribution5sec <- dataPaper %>% 
  filter(duration >=5) %>% 
  dplyr::select(party, contains("e1")) %>%
  drop_na() %>% 
  group_by(party) %>% 
  mutate(Count = n()) %>% 
  group_by(party, Count) %>% 
  summarize_at(vars(e1_anger_m:e1_sadness_m), funs(mean,sd), na.rm = T)


#Mean duration and length of answers

meanDurationLength <- dataPaper %>% 
  filter(duration >=5) %>% 
  group_by(party) %>% 
  drop_na(e1_anger_m) %>% 
  summarize(duration_mean = mean(duration, na.rm = T),
            duration_min = min(duration, na.rm = T),
            duration_max = max(duration, na.rm = T),
            duration_median = median(duration, na.rm = T),
            duration_sd = sd(duration, na.rm = T),
            duration_skew = moments::skewness(duration, na.rm = T))

meanTokenLength <- dataPaper %>% 
  filter(duration >=5) %>% 
  group_by(party) %>% 
  drop_na(e1_anger_m) %>% 
  summarize(NToken_mean = mean(NToken, na.rm = T),
            NToken_min = min(NToken, na.rm = T),
            NToken_max = max(NToken, na.rm = T),
            NToken_median = median(NToken, na.rm = T),
            NToken_sd = sd(NToken, na.rm = T),
            NToken_skew = moments::skewness(NToken, na.rm = T))

meanDurationLengthLog <- dataPaper %>% 
  filter(duration >=5) %>% 
  group_by(party) %>% 
  drop_na(e1_anger_m) %>% 
  summarize(duration_mean = mean(log1p(duration), na.rm = T),
            duration_min = min(log1p(duration), na.rm = T),
            duration_max = max(log1p(duration), na.rm = T),
            duration_median = median(log1p(duration), na.rm = T),
            duration_sd = sd(log1p(duration), na.rm = T),
            duration_skew = moments::skewness(log1p(duration), na.rm = T))

meanTokenLengthLog <- dataPaper %>% 
  filter(duration >=5) %>% 
  group_by(party) %>% 
  drop_na(e1_anger_m) %>% 
  summarize(NToken_mean = mean(log1p(NToken), na.rm = T),
            NToken_min = min(log1p(NToken), na.rm = T),
            NToken_max = max(log1p(NToken), na.rm = T),
            NToken_median = median(log1p(NToken), na.rm = T),
            NToken_sd = sd(log1p(NToken), na.rm = T),
            NToken_skew = moments::skewness(log1p(NToken), na.rm = T))



N_question <- dataPaper %>% 
  filter(duration >=5) %>% 
  group_by(party) %>% 
  drop_na(duration, e1_anger_m) %>% 
  summarize(N = n())

meanDurationLength5sec <- dataPaper %>% 
  filter(duration >=5) %>% 
  summarize(duration_m = mean(duration, na.rm = T),
            NToken_m = mean(NToken, na.rm = T))

skewnessDurationLength5sec <- dataPaper %>% 
  filter(duration >=5) %>% 
  summarize(duration_skew = moments::skewness(duration, na.rm = T),
            NToken_skew = moments::skewness(NToken, na.rm = T))


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

###Remove short answers
# Prediction of Answer Length - Words
m_lengthWords_Emotions <- lm(NToken ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party, data = dataPaper, subset = duration >= 2)

m_lengthWords_Sentiment <- lm(NToken ~ sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 2)

m_lengthWords_Full <- lm(NToken ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 2)

m_lengthWords_Controls <- lm(NToken ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + age + female + education, data = dataPaper, subset = duration >= 2)

stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results", align=TRUE, type = "text")

stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results: Length Words", align=TRUE, type = "html", out = "resultsPaper/models_LengthWordsRobust.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))

plot_model(m_lengthWords_Controls, type = "pred", terms = "sentiment_norm")


# Prediction of Answer Length - Duration
m_lengthDuration_Emotions <- lm(duration ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party, data = dataPaper, subset = duration >= 2)

m_lengthDuration_Sentiment <- lm(duration ~ sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 2)

m_lengthDuration_Full <- lm(duration ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party, data = dataPaper, subset = duration >= 2)

m_lengthDuration_Controls <- lm(duration ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + age + female + education, data = dataPaper, subset = duration >= 2)

stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results", align=TRUE, type = "text")

stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results: Duration", align=TRUE, type = "html", out = "resultsPaper/models_LengthDurationRobust.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))


plot_model(m_lengthDuration_Controls, type = "pred", terms = "sentiment_norm")

###Remove short answers - restrictive
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
m_lengthWords_Emotions <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party + as.factor(e3_class), data = dataPaper, subset = duration >= 5)

m_lengthWords_Sentiment <- lm(log1p(NToken) ~ sentiment_norm + I(sentiment_norm^2) + party + as.factor(e3_class), data = dataPaper, subset = duration >= 5)

m_lengthWords_Full <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + as.factor(e3_class), data = dataPaper, subset = duration >= 5)

m_lengthWords_Controls <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + age + female + education + as.factor(e3_class), data = dataPaper, subset = duration >= 5)

stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))

# stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results: Length Words", align=TRUE, type = "html", out = "resultsPaper/models_LengthWordsRobust5SecLOG.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
# 
# stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results: Length Words", align=TRUE, type = "html", out = "resultsPaper/models_LengthWordsRobust5SecLOG_AAPOR.htm", no.space = T, single.row = T, digits = 2, report = "vcp", star.cutoffs = c(.05, .01, .001))


# plot_model(m_lengthWords_Controls, type = "pred", terms = "sentiment_norm")

# Prediction of Answer Length - Duration
m_lengthDuration_Emotions <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party + as.factor(e3_class), data = dataPaper, subset = duration >= 5)

m_lengthDuration_Sentiment <- lm(log1p(duration) ~ sentiment_norm + I(sentiment_norm^2) + party + as.factor(e3_class), data = dataPaper, subset = duration >= 5)

m_lengthDuration_Full <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + as.factor(e3_class), data = dataPaper, subset = duration >= 5)

m_lengthDuration_Controls <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + party + age + female + education + as.factor(e3_class), data = dataPaper, subset = duration >= 5)

stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))

# stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results: Duration", align=TRUE, type = "html", out = "resultsPaper/models_LengthDurationRobust5SecLOG.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
# 
# stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results: Duration", align=TRUE, type = "html", out = "resultsPaper/models_LengthDurationRobust5SecLOG_AAPOR.htm", no.space = T, single.row = T, digits = 2, report = "vcp", star.cutoffs = c(.05, .01, .001))

# 
# plot_model(m_lengthDuration_Controls, type = "pred", terms = "sentiment_norm")
# 
# 


###AfD


###Log transformation
# Prediction of Answer Length - Words
m_lengthWords_Emotions <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "AfD")

m_lengthWords_Sentiment <- lm(log1p(NToken) ~ sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "AfD")

m_lengthWords_Full <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "AfD")

m_lengthWords_Controls <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2)  + age + female + education + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "AfD")

stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))


m_lengthWords_Emotions <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "CDU")

m_lengthWords_Sentiment <- lm(log1p(NToken) ~ sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "CDU")

m_lengthWords_Full <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "CDU")

m_lengthWords_Controls <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2)  + age + female + education + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "CDU")

stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))


m_lengthWords_Emotions <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "Greens")

m_lengthWords_Sentiment <- lm(log1p(NToken) ~ sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "Greens")

m_lengthWords_Full <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "Greens")

m_lengthWords_Controls <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2)  + age + female + education + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "Greens")

stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))


m_lengthWords_Emotions <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "SPD")

m_lengthWords_Sentiment <- lm(log1p(NToken) ~ sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "SPD")

m_lengthWords_Full <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "SPD")

m_lengthWords_Controls <- lm(log1p(NToken) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2)  + age + female + education + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "SPD")

stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))

# stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results: Length Words", align=TRUE, type = "html", out = "resultsPaper/models_LengthWordsRobust5SecLOG.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
# 
# stargazer(m_lengthWords_Emotions, m_lengthWords_Sentiment, m_lengthWords_Full, m_lengthWords_Controls, title="Results: Length Words", align=TRUE, type = "html", out = "resultsPaper/models_LengthWordsRobust5SecLOG_AAPOR.htm", no.space = T, single.row = T, digits = 2, report = "vcp", star.cutoffs = c(.05, .01, .001))


# plot_model(m_lengthWords_Controls, type = "pred", terms = "sentiment_norm")

# Prediction of Answer Length - Duration
m_lengthDuration_Emotions <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "AfD")

m_lengthDuration_Sentiment <- lm(log1p(duration) ~ sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "AfD")

m_lengthDuration_Full <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "AfD")

m_lengthDuration_Controls <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2)  + age + female + education + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "AfD")

stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))


m_lengthDuration_Emotions <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "CDU")

m_lengthDuration_Sentiment <- lm(log1p(duration) ~ sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "CDU")

m_lengthDuration_Full <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "CDU")

m_lengthDuration_Controls <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2)  + age + female + education + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "CDU")

stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))


m_lengthDuration_Emotions <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "Greens")

m_lengthDuration_Sentiment <- lm(log1p(duration) ~ sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "Greens")

m_lengthDuration_Full <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "Greens")

m_lengthDuration_Controls <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2)  + age + female + education + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "Greens")

stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))


m_lengthDuration_Emotions <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "SPD")

m_lengthDuration_Sentiment <- lm(log1p(duration) ~ sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "SPD")

m_lengthDuration_Full <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2) + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "SPD")

m_lengthDuration_Controls <- lm(log1p(duration) ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + sentiment_norm + I(sentiment_norm^2)  + age + female + education + as.factor(e3_class), data = dataPaper, subset = duration >= 5 & party == "SPD")

stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results", align=TRUE, type = "text", report=('vc*p'))

# stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results: Duration", align=TRUE, type = "html", out = "resultsPaper/models_LengthDurationRobust5SecLOG.htm", no.space = T, single.row = T, digits = 2, star.cutoffs = c(.05, .01, .001))
# 
# stargazer(m_lengthDuration_Emotions, m_lengthDuration_Sentiment, m_lengthDuration_Full, m_lengthDuration_Controls, title="Results: Duration", align=TRUE, type = "html", out = "resultsPaper/models_LengthDurationRobust5SecLOG_AAPOR.htm", no.space = T, single.row = T, digits = 2, report = "vcp", star.cutoffs = c(.05, .01, .001))

# 
# plot_model(m_lengthDuration_Controls, type = "pred", terms = "sentiment_norm")
# 
# 



