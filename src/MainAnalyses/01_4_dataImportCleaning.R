
#Data Import Cleaning

library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(psych)
library(sjPlot)
library(tuneR)

afd15 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q1_afd_15s.rds")
afd16 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q1_afd_16s.rds")

afd15_2 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q2_afd_15s.rds")
afd16_2 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q2_afd_16s.rds")

afd15_3 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q3_afd_15s.rds")
afd16_3 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q3_afd_16s.rds")

afd <- rbind(afd15, afd16, afd15_2, afd16_2, afd15_3, afd16_3) %>% 
  mutate(party = "AfD")

cdu15 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q1_cdu_15s.rds")
cdu16 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q1_cdu_16s.rds")

cdu15_2 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q2_cdu_15s.rds")
cdu16_2 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q2_cdu_16s.rds")

cdu15_3 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q3_cdu_15s.rds")
cdu16_3 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q3_cdu_16s.rds")

cdu <- rbind(cdu15, cdu16, cdu15_2, cdu16_2, cdu15_3, cdu16_3) %>% 
  mutate(party = "CDU")

spd15 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q1_spd_15s.rds")
spd16 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q1_spd_16s.rds")

spd15_2 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q2_spd_15s.rds")
spd16_2 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q2_spd_16s.rds")

spd15_3 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q3_spd_15s.rds")
spd16_3 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q3_spd_16s.rds")

spd <- rbind(spd15, spd16, spd15_2, spd16_2, spd15_3, spd16_3) %>% 
  mutate(party = "SPD")

greens15 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q1_greens_15s.rds")
greens16 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q1_greens_16s.rds")

greens15_2 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q2_greens_15s.rds")
greens16_2 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q2_greens_16s.rds")

greens15_3 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q3_greens_15s.rds")
greens16_3 <- readRDS("C:/Users/kgavras/Google Drive/Sound of Respondents/voice_q3_greens_16s.rds")

greens <- rbind(greens15, greens16, greens15_2, greens16_2, greens15_3, greens16_3) %>%
  mutate(party = "Greens")

rm(afd15, afd16, afd15_2, afd16_2, afd15_3, afd16_3, cdu15, cdu16,  cdu15_2, cdu16_2, cdu15_3, cdu16_3, spd15, spd16, spd15_2, spd16_2, spd15_3, spd16_3, greens15, greens16, greens15_2, greens16_2, greens15_3, greens16_3)

#Load survey data
source("01_1_surveyDataCleaning.R")
source("01_2_sentimentScores.R")

dataSurveyVoice <- dataSurvey %>% 
  filter(TextCondition == 0)

dataDistribution <- dataSurveyVoice %>%
  sjmisc::to_dummy(education, var.name = "education", suffix = "numeric") %>% 
  bind_cols(dataSurveyVoice) %>% 
  dplyr::rename(lowerEducation = education_1, medEducation = education_2, highEducation = education_3) %>% 
  summarize(female = mean(female, na.rm = T),
            ageMean = median(age, na.rm = T),
            lowerEducation = mean(lowerEducation, na.rm = T),
            medEducation = mean(medEducation, na.rm = T),
            highEducation = mean(highEducation, na.rm = T),
            motherTongueGerman = mean(motherTongueGerman, na.rm = T))


voice <- rbind(afd, cdu, spd, greens)

rm(afd, cdu, spd, greens)

dataSurveyFinal <- dataSurvey %>% 
    filter(TextCondition == 0) %>% 
    dplyr::select(lfdn, tic, female, age, education, motherTongueGerman, smartphoneSkills, internetUsage, voiceMessage, voteIntention, polKnowledge, partyPreference, sentimentRatio_CDU = ratio_CDUVoice, sentimentRatio_SPD = ratio_SPDVoice, sentimentRatio_Greens = ratio_GreensVoice, sentimentRatio_AfD = ratio_AfDVoice) %>% 
    pivot_longer(cols = c(sentimentRatio_SPD, sentimentRatio_Greens, sentimentRatio_AfD, sentimentRatio_CDU), names_to = "party", values_to = "sentiment") %>% 
    mutate(party = str_extract(party, "_.*"),
           party = str_replace(party, "_", ""))

#Extract information from the text answers
dataSurveyText <- dataSurvey %>% 
  filter(TextCondition == 0) %>% 
  dplyr::select(lfdn, tic, CDU_Text = CDUCSU_VoiceCondition, SPD_Text = SPD_VoiceCondition, Greens_Text = Greens_VoiceCondition, AfD_Text = AfD_VoiceCondition) %>% 
  pivot_longer(cols = c(CDU_Text, SPD_Text, Greens_Text, AfD_Text), names_to = "party", values_to = "voiceAnswer") %>% 
  mutate(party = str_extract(party, ".*_"),
         party = str_replace(party, "_", "")) %>% 
  mutate(NToken = ntoken(voiceAnswer))

#Get duration of .wav files
source("01_3_durationWAV.R")

survey_c <- dataSurveyFinal %>%
  left_join(voice, by = c("tic" = "id", "party")) %>% 
  left_join(dataSurveyText, by = c("lfdn", "tic", "party")) %>% 
  left_join(dataDuration, by = c("tic", "party"))

survey_c <- survey_c %>%
  mutate_at(c("e1_class", "e2_class", "e3_class"), as.factor) %>%
  mutate(e1_class = fct_recode(e1_class, 
                               Neutral = "e1_neutral_m",
                               Anger = "e1_anger_m", 
                               Boredom = "e1_boredom_m",
                               Disgust = "e1_disgust_m",
                               Fear = "e1_fear_m",
                               Happiness = "e1_happiness_m",
                               Sadness = "e1_sadness_m"),
         e2_class = fct_recode(e2_class, 
                               Neutral = "e2_neutral_m",
                               Agressive = "e2_agressiv_m", 
                               Cheerful = "e2_cheerful_m",
                               Intoxicated = "e2_intoxicated_m",
                               Nervous = "e2_nervous_m",
                               Tired = "e2_tired_m"),
         e3_class = fct_recode(e3_class, 
                               disinterest = "e3_loi1_m", 
                               normal = "e3_loi2_m",
                               high_interest = "e3_loi3_m"),
         no_voice = ifelse(is.na(e1_class), 1, 0))

save(survey_c, file = "voiceData.Rdata")
