
#Survey Data Cleaning

library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(psych)
library(sjPlot)
library(tuneR)

load("E:/dataFull_ohnePara.RData")
load("E:/dataFull_Para.RData")

dataIdentifyComplete <- dataFull_Para %>% 
  dplyr::select(Goodbye_1_Trans_Time, c_0007, lfdn, tic)

dataFull <- dataFull %>% 
  left_join(dataIdentifyComplete, by = "lfdn") %>% 
  mutate(completed = if_else(is.na(Goodbye_1_Trans_Time) == T, 0, 1) ) %>% 
  dplyr::select(-Goodbye_1_Trans_Time)

dataComplete <- dataFull %>% 
  dplyr::select(lfdn, completed)

dataCompleteDuplicated <- dataFull %>% 
  dplyr::select(lfdn, completed, c_0007) %>% 
  filter(completed == 1) %>% 
  group_by(c_0007) %>% 
  filter(n()>1 & is.na(c_0007)==F)

dataDuplicated <- dataFull %>% 
  dplyr::select(lfdn, c_0007) %>% 
  distinct(c_0007, .keep_all = T)


dataDesignRecruitment <- dataFull %>% 
  dplyr::select(lfdn, c_0007, c_0001) %>%
  mutate(TextCondition = if_else(c_0001 <=3, 1, 0)) %>% 
  semi_join(dataDuplicated, by = c("lfdn")) %>% 
  group_by(TextCondition) %>% 
  summarize(N = n())

rm(dataFull_Para)

dataSurvey <- dataFull %>% 
  anti_join(dataCompleteDuplicated, by = c("c_0007")) %>% 
  filter(completed == 1) %>% 
  unite("CDUCSU_TextCondition", c(CDU_CSU_Text1_1_Answ_1, CDU_CSU_Text2_1_Answ_1, CDU_CSU_Text3_1_Answ_1, CDU_CSU_Text3_1_FU_Answ_1), remove = FALSE) %>% 
  unite("CDUCSU_VoiceCondition", c(transcript_QA_1_CDUCSU, transcript_QA_2_CDUCSU, transcript_QA_3_CDUCSU, transcript_QA_3_CDUCSU_FU), remove = FALSE) %>% 
  mutate(CDUCSU_TextCondition = str_replace_all(CDUCSU_TextCondition, pattern = "\\_", replacement = " "),
         CDUCSU_TextCondition = str_remove_all(CDUCSU_TextCondition, pattern = "NA"),
         CDUCSU_TextCondition = str_squish(CDUCSU_TextCondition)) %>% 
  mutate(CDUCSU_VoiceCondition = str_replace_all(CDUCSU_VoiceCondition, pattern = "\\_", replacement = " "),
         CDUCSU_VoiceCondition = str_remove_all(CDUCSU_VoiceCondition, pattern = "NA"),
         CDUCSU_VoiceCondition = str_squish(CDUCSU_VoiceCondition)) %>% 
  unite("SPD_TextCondition", c(SPD_Text1_1_Answ_1, SPD_Text2_1_Answ_1, SPD_Text3_1_Answ_1, SPD_Text3_1_FU_Answ_1), remove = FALSE) %>% 
  unite("SPD_VoiceCondition", c(transcript_QA_1_SPD, transcript_QA_2_SPD, transcript_QA_3_SPD, transcript_QA_3_SPD_FU), remove = FALSE) %>% 
  mutate(SPD_TextCondition = str_replace_all(SPD_TextCondition, pattern = "\\_", replacement = " "),
         SPD_TextCondition = str_remove_all(SPD_TextCondition, pattern = "NA"),
         SPD_TextCondition = str_squish(SPD_TextCondition)) %>% 
  mutate(SPD_VoiceCondition = str_replace_all(SPD_VoiceCondition, pattern = "\\_", replacement = " "),
         SPD_VoiceCondition = str_remove_all(SPD_VoiceCondition, pattern = "NA"),
         SPD_VoiceCondition = str_squish(SPD_VoiceCondition)) %>% 
  unite("Greens_TextCondition", c(Greens_Text1_1_Answ_1, Greens_Text2_1_Answ_1, Greens_Text3_1_Answ_1, Greens_Text3_1_FU_Answ_1), remove = FALSE) %>% 
  unite("Greens_VoiceCondition", c(transcript_QA_1_Greens, transcript_QA_2_Greens, transcript_QA_3_Greens, transcript_QA_3_Greens_FU), remove = FALSE) %>% 
  mutate(Greens_TextCondition = str_replace_all(Greens_TextCondition, pattern = "\\_", replacement = " "),
         Greens_TextCondition = str_remove_all(Greens_TextCondition, pattern = "NA"),
         Greens_TextCondition = str_squish(Greens_TextCondition)) %>% 
  mutate(Greens_VoiceCondition = str_replace_all(Greens_VoiceCondition, pattern = "\\_", replacement = " "),
         Greens_VoiceCondition = str_remove_all(Greens_VoiceCondition, pattern = "NA"),
         Greens_VoiceCondition = str_squish(Greens_VoiceCondition)) %>% 
  unite("AfD_TextCondition", c(AfD_Text1_1_Answ_1, AfD_Text2_1_Answ_1, AfD_Text3_1_Answ_1, AfD_Text3_1_FU_Answ_1), remove = FALSE) %>% 
  unite("AfD_VoiceCondition", c(transcript_QA_1_AfD, transcript_QA_2_AfD, transcript_QA_3_AfD, transcript_QA_3_AfD_FU), remove = FALSE) %>% 
  mutate(AfD_TextCondition = str_replace_all(AfD_TextCondition, pattern = "\\_", replacement = " "),
         AfD_TextCondition = str_remove_all(AfD_TextCondition, pattern = "NA"),
         AfD_TextCondition = str_squish(AfD_TextCondition)) %>% 
  mutate(AfD_VoiceCondition = str_replace_all(AfD_VoiceCondition, pattern = "\\_", replacement = " "),
         AfD_VoiceCondition = str_remove_all(AfD_VoiceCondition, pattern = "NA"),
         AfD_VoiceCondition = str_squish(AfD_VoiceCondition)) %>% 
  mutate(female = case_when(Gender_1_Answ_1 == 2 ~ 1,
                            Gender_1_Answ_1 == 1 ~ 0),
         age = if_else(Birthyear_1_Answ_1 > 0, Birthyear_1_Answ_1, NA_integer_),
         age = 12 - age,
         education = if_else(Education_1_Answ_1 > 0, Education_1_Answ_1, NA_integer_),
         education = case_when(education <= 2 ~ 3,
                               education == 5 | education == 7 ~ 6,
                               TRUE ~ as.numeric(education)),
         education = as.factor(education),
         education = fct_recode(education, low = "3", medium = "4", high = "6"),
         motherTongueGerman = case_when(Mother_Tongue_1_Answ_1 == 1 ~ 1,
                                        Mother_Tongue_1_Answ_1 == 2 ~ 0),
         smartphoneSkills = if_else(Smartphone_Skills_1_Answ_1 > 0, Smartphone_Skills_1_Answ_1, NA_integer_),
         smartphoneSkills = 7 - smartphoneSkills,
         internetUsage = if_else(Internet_Usage_1_Answ_1 > 0, Internet_Usage_1_Answ_1, NA_integer_),
         internetUsage = 5 - internetUsage,
         voiceMessage = if_else(Voice_Message_1_Answ_1 > 0, Voice_Message_1_Answ_1, NA_integer_),
         voiceMessage = case_when(voiceMessage <= 2 ~ 1,
                                  voiceMessage > 2 ~ 0),
         voteIntention = if_else(Voting_Intention_1_Answ_1 > 0, Voting_Intention_1_Answ_1, NA_integer_),
         voteIntention = 5 - voteIntention,
         polKnowledge = case_when(Five_Percent_Threshold_1_Answ_1 == 7 ~ 0,
                                  Five_Percent_Threshold_1_Answ_2 == "5" ~ 1,
                                  Five_Percent_Threshold_1_Answ_2 == "5,0" ~ 1,
                                  Five_Percent_Threshold_1_Answ_2 == "5%" ~ 1,
                                  TRUE ~ 0),
         polKnowledge = if_else(Five_Percent_Threshold_1_Answ_1 == 0, NA_real_, polKnowledge)
  ) %>% 
  mutate(
    partyPreference = case_when(Party_Preference_1_Answ_1 == 1 ~ "CDU",
                                Party_Preference_1_Answ_1 == 2 ~ "SPD",
                                Party_Preference_1_Answ_1 == 3 ~ "Greens",
                                Party_Preference_1_Answ_1 == 4 ~ "AfD",
                                Party_Preference_1_Answ_1 == 5 ~ "FDP",
                                Party_Preference_1_Answ_1 == 6 ~ "Linke",
                                Party_Preference_1_Answ_1 == 7 ~ "Others",
                                TRUE ~ "NA")
  ) %>% 
  dplyr::select(lfdn, tic, CDUCSU_VoiceCondition, SPD_VoiceCondition, Greens_VoiceCondition, AfD_VoiceCondition, TextCondition = c_0001, female, age, education, motherTongueGerman, smartphoneSkills, internetUsage, voiceMessage, voteIntention, polKnowledge, partyPreference) %>% 
  mutate(TextCondition = if_else(TextCondition <=3, 1, 0))

rm(dataComplete, dataCompleteDuplicated, dataFull, dataIdentifyComplete)
