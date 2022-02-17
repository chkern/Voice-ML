### Author: Konstantin Gavras

#02_analysis

glimpse(survey_c)



#Descriptives

#Get mean values by question
df_mean <- survey_c %>%
  dplyr::select(party, sentiment, e1_anger_m, e1_boredom_m, e1_disgust_m, e1_fear_m, e1_happiness_m, e1_neutral_m, e1_sadness_m, e2_agressiv_m, e2_cheerful_m, e2_intoxicated_m, e2_nervous_m, e2_neutral_m, e2_tired_m, e3_loi1_m, e3_loi2_m, e3_loi3_m) %>% 
  group_by(party) %>%
  summarise_all(mean, na.rm = T)


#Distribution of sentiment scores in transcribed answers
emotionPlot <- function(data, dataMean, variable, title){
  variable <- enquo(variable)
  p <- ggplot(data, aes(x = !!variable)) + 
    geom_histogram() + 
    facet_wrap(~party) + 
    geom_vline(data = dataMean, aes(xintercept = !!variable),col='red') + 
    labs(title = title) +
    theme_bw()
  
  return(p)
}


pdf(file = "EmotionPlots.pdf")
emotionPlot(survey_c, df_mean, sentiment, "Sentiment Scores")

#Distribution of emotions in voice answers (e1)
emotionPlot(survey_c, df_mean, e1_anger_m, "Emotion: Anger")
emotionPlot(survey_c, df_mean, e1_boredom_m, "Emotion: Boredom")
emotionPlot(survey_c, df_mean, e1_disgust_m, "Emotion: Disgust")
emotionPlot(survey_c, df_mean, e1_fear_m, "Emotion: Fear")
emotionPlot(survey_c, df_mean, e1_happiness_m, "Emotion: Happiness")
emotionPlot(survey_c, df_mean, e1_neutral_m, "Emotion: Neutral")
emotionPlot(survey_c, df_mean, e1_sadness_m, "Emotion: Sadness")

#Emotions in Voice Answers (e2)
emotionPlot(survey_c, df_mean, e2_agressiv_m, "Emotion: Agressiv")
emotionPlot(survey_c, df_mean, e2_cheerful_m, "Emotion: Cheerful")
emotionPlot(survey_c, df_mean, e2_intoxicated_m, "Emotion: Intoxicated")
emotionPlot(survey_c, df_mean, e2_nervous_m, "Emotion: Nervous")
emotionPlot(survey_c, df_mean, e2_neutral_m, "Emotion: Neutral")
emotionPlot(survey_c, df_mean, e2_tired_m, "Emotion: Tired")

#Emotions in Voice Answers (e3)
emotionPlot(survey_c, df_mean, e3_loi1_m, "Emotion: Disinterest")
emotionPlot(survey_c, df_mean, e3_loi2_m, "Emotion: Normal")
emotionPlot(survey_c, df_mean, e3_loi3_m, "Emotion: High interest")
dev.off()

#Emotions when evaluating own party
survey_c_ownParty <- survey_c %>% 
  filter(party == partyPreference)

df_mean_ownParty <- survey_c_ownParty %>%
  dplyr::select(party, sentiment, e1_anger_m, e1_boredom_m, e1_disgust_m, e1_fear_m, e1_happiness_m, e1_neutral_m, e1_sadness_m, e2_agressiv_m, e2_cheerful_m, e2_intoxicated_m, e2_nervous_m, e2_neutral_m, e2_tired_m, e3_loi1_m, e3_loi2_m, e3_loi3_m) %>% 
  group_by(party) %>%
  summarise_all(mean, na.rm = T)

pdf(file = "EmotionPlotsOwnParty.pdf")
emotionPlot(survey_c_ownParty, df_mean_ownParty, sentiment, "Own party: Sentiment Scores")

#Distribution of emotions in voice answers (e1)
emotionPlot(survey_c_ownParty, df_mean_ownParty, e1_anger_m, "Own party: Emotion: Anger")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e1_boredom_m, "Own party: Emotion: Boredom")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e1_disgust_m, "Own party: Emotion: Disgust")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e1_fear_m, "Own party: Emotion: Fear")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e1_happiness_m, "Own party: Emotion: Happiness")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e1_neutral_m, "Own party: Emotion: Neutral")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e1_sadness_m, "Own party: Emotion: Sadness")

#Emotions in Voice Answers (e2)
emotionPlot(survey_c_ownParty, df_mean_ownParty, e2_agressiv_m, "Own party: Emotion: Agressiv")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e2_cheerful_m, "Own party: Emotion: Cheerful")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e2_intoxicated_m, "Own party: Emotion: Intoxicated")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e2_nervous_m, "Own party: Emotion: Nervous")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e2_neutral_m, "Own party: Emotion: Neutral")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e2_tired_m, "Own party: Emotion: Tired")

#Emotions in Voice Answers (e3)
emotionPlot(survey_c_ownParty, df_mean_ownParty, e3_loi1_m, "Own party: Emotion: Disinterest")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e3_loi2_m, "Own party: Emotion: Normal")
emotionPlot(survey_c_ownParty, df_mean_ownParty, e3_loi3_m, "Own party: Emotion: High interest")
dev.off()


#Difference between General Emotion and Emotion when evaluating own party
common <- intersect(names(df_mean), names(df_mean_ownParty))[-(1)]
df_diff <- df_mean
df_diff[common] <- df_mean_ownParty[common] - df_mean[common]

print(xtable::xtable(df_diff), type="html", file="DiffTable_Ownparty-Mean.html")
