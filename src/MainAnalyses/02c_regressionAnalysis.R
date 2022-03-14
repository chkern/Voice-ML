
###Regression analysis

glimpse(survey_c)

dataReg <- survey_c %>% 
  mutate(party = as.factor(party))


# Hierachical model: respondents nested in questions
# DV: Sentiment
# IV: Emotion scores

# First models: e1_Emotions

m1 <- lm(sentiment ~ e1_anger_m + party, data = dataReg)
m2 <- lm(sentiment ~ e1_boredom_m + I(e1_boredom_m^2) + party, data = dataReg)
m3 <- lm(sentiment ~ e1_disgust_m + party, data = dataReg)
m4 <- lm(sentiment ~ e1_fear_m + party, data = dataReg)
m5 <- lm(sentiment ~ e1_happiness_m + party, data = dataReg)
m6 <- lm(sentiment ~ e1_neutral_m + I(e1_neutral_m^2) + party, data = dataReg)
m7 <- lm(sentiment ~ e1_sadness_m + party, data = dataReg)

stargazer(m1,m2,m3,m4,m5,m6,m7, title="Results", align=TRUE, type = "text")

plot_model(m2, type = "pred", terms = "e1_boredom_m")
plot_model(m6, type = "pred", terms = "e1_neutral_m")


m8 <- lm(sentiment ~ e1_anger_m + e1_boredom_m + I(e1_boredom_m^2) + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m + party , data = dataReg)

plot_model(m8, type = "pred", terms = "e1_boredom_m")

stargazer(m8, title="Results", align=TRUE, type = "text")

stargazer(m1,m2,m3,m4,m5,m6,m7,m8, title="Results", align=TRUE, type = "html", out = "models_e1.htm")

m8_1 <- lm(sentiment ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m, data = dataReg, subset = party == "CDU")
m8_2 <- lm(sentiment ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m, data = dataReg, subset = party == "SPD")
m8_3 <- lm(sentiment ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m, data = dataReg, subset = party == "Greens")
m8_4 <- lm(sentiment ~ e1_anger_m + e1_boredom_m + e1_disgust_m + e1_fear_m + e1_happiness_m + e1_sadness_m, data = dataReg, subset = party == "AfD")

stargazer(m8_1, m8_2, m8_3, m8_4, title="Results", align=TRUE, type = "text", column.labels = c("CDU", "SPD", "Greens", "AfD"))

stargazer(m8_1, m8_2, m8_3, m8_4, title="Results", align=TRUE, type = "html", column.labels = c("CDU", "SPD", "Greens", "AfD"), out = "models_e1_byQuestion.htm")


# Regression Emotion Class
m_class <- lm(sentiment ~ e1_class + party, data = dataReg)
summary(m_class)

stargazer(m_class, title="Results", align=TRUE, type = "html", out = "model_e1_class.htm")



# Second models: e2_Emotions

m1 <- lm(sentiment ~ e2_agressiv_m + party, data = dataReg)
m2 <- lm(sentiment ~ e2_cheerful_m + party, data = dataReg)
m3 <- lm(sentiment ~ e2_intoxicated_m + party, data = dataReg)
m4 <- lm(sentiment ~ e2_nervous_m + party, data = dataReg)
m5 <- lm(sentiment ~ e2_neutral_m + I(e2_neutral_m^2) + party, data = dataReg)
m6 <- lm(sentiment ~ e2_tired_m + party, data = dataReg)

stargazer(m1,m2,m3,m4,m5,m6, title="Results", align=TRUE, type = "text")

plot_model(m5, type = "pred", terms = "e2_neutral_m")


m8 <- lm(sentiment ~ e2_agressiv_m + e2_cheerful_m + e2_intoxicated_m + e2_nervous_m + e2_tired_m + party , data = dataReg)

stargazer(m8, title="Results", align=TRUE, type = "text")

stargazer(m1,m2,m3,m4,m5,m6,m8, title="Results", align=TRUE, type = "html", out = "models_e2.htm")

m8_1 <- lm(sentiment ~ e2_agressiv_m + e2_cheerful_m + e2_intoxicated_m + e2_nervous_m + e2_tired_m, data = dataReg, subset = party == "CDU")
m8_2 <- lm(sentiment ~ e2_agressiv_m + e2_cheerful_m + e2_intoxicated_m + e2_nervous_m + e2_tired_m, data = dataReg, subset = party == "SPD")
m8_3 <- lm(sentiment ~ e2_agressiv_m + e2_cheerful_m + e2_intoxicated_m + e2_nervous_m + e2_tired_m, data = dataReg, subset = party == "Greens")
m8_4 <- lm(sentiment ~ e2_agressiv_m + e2_cheerful_m + e2_intoxicated_m + e2_nervous_m + e2_tired_m, data = dataReg, subset = party == "AfD")

stargazer(m8_1, m8_2, m8_3, m8_4, title="Results", align=TRUE, type = "text", column.labels = c("CDU", "SPD", "Greens", "AfD"))

stargazer(m8_1, m8_2, m8_3, m8_4, title="Results", align=TRUE, type = "html", column.labels = c("CDU", "SPD", "Greens", "AfD"), out = "models_e2_byQuestion.htm")


# Regression Emotion Class
m_class <- lm(sentiment ~ e2_class + party, data = dataReg)
summary(m_class)

stargazer(m_class, title="Results", align=TRUE, type = "html", out = "model_e2_class.htm")

