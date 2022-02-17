### Author: Konstantin Gavras

#Get Sentiment Scores

dataSurvey <- dataSurvey %>% 
  mutate_at(.vars = c("CDUCSU_VoiceCondition",
                      "SPD_VoiceCondition",
                      "Greens_VoiceCondition",
                      "AfD_VoiceCondition"),funs(str_replace_all(., "[^[:alnum:] ]", "")))

getPreparedData <- function(data = dataSurvey, var){
  
  VarString <- deparse(substitute(data$var))
  VarString <- str_remove(VarString, pattern = ".*\\$")
  
  selectVar = data[,c(VarString, "lfdn")]
  
  combinedData = unnest_tokens(selectVar, words, !! rlang::sym(VarString), to_lower = F, drop = F)
  
  return(combinedData)
  
}

dataListStructure = list()

dataListStructure[[1]] <- getPreparedData(var = CDUCSU_VoiceCondition)
dataListStructure[[2]] <- getPreparedData(var = SPD_VoiceCondition)
dataListStructure[[3]] <- getPreparedData(var = Greens_VoiceCondition)
dataListStructure[[4]] <- getPreparedData(var = AfD_VoiceCondition)

#Get sentiment scores
getSentimentScores <- function(data, newVar){
  newVar <- enquo(newVar)
  newVarName <- quo_name(newVar)
  
  sentiData = data %>% 
    left_join(senti_ws, by = c("words" = "word"))
  
  ratioDF <- sentiData %>% 
    mutate(type = if_else(score > 0, 1, if_else(score < 0, -1, 0))) %>% 
    group_by(lfdn, type) %>% 
    dplyr::summarize(score = sum(score, na.rm = T)) %>% 
    filter(!is.na(type)) %>% 
    pivot_wider(names_from = "type", values_from = "score") %>% 
    rename(positive = `1`, negative = `-1`) %>% 
    tidyr::replace_na(list(positive = 0.001, negative = 0.001)) %>% 
    mutate(!!newVarName := log( positive / abs(negative)) ) %>% 
    select(-c(positive, negative))
  
  return(ratioDF)
}

sentiData_CDUVoice <- getSentimentScores(dataListStructure[[1]], "ratio_CDUVoice")
sentiData_SPDVoice <- getSentimentScores(dataListStructure[[2]], "ratio_SPDVoice")
sentiData_GreensVoice <- getSentimentScores(dataListStructure[[3]], "ratio_GreensVoice")
sentiData_AfDVoice <- getSentimentScores(dataListStructure[[4]], "ratio_AfDVoice")

dataSurvey <- dataSurvey %>% 
  left_join(sentiData_CDUVoice, by = "lfdn") %>% 
  left_join(sentiData_SPDVoice, by = "lfdn") %>% 
  left_join(sentiData_GreensVoice, by = "lfdn") %>% 
  left_join(sentiData_AfDVoice, by = "lfdn")

rm(dataListStructure, sentiData_AfDVoice, sentiData_GreensVoice, sentiData_SPDVoice, sentiData_CDUVoice)

dataSurvey <- dataSurvey %>% 
  mutate(ratio_CDUVoice = if_else((nchar(CDUCSU_VoiceCondition) != 0 | nchar(SPD_VoiceCondition) != 0 | nchar(Greens_VoiceCondition) != 0 |  nchar(AfD_VoiceCondition)) & is.na(ratio_CDUVoice) == T, 0, ratio_CDUVoice),
         ratio_SPDVoice = if_else((nchar(CDUCSU_VoiceCondition) != 0 | nchar(SPD_VoiceCondition) != 0 | nchar(Greens_VoiceCondition) != 0 | nchar(AfD_VoiceCondition)) & is.na(ratio_SPDVoice) == T, 0, ratio_SPDVoice),
         ratio_GreensVoice = if_else((nchar(CDUCSU_VoiceCondition) != 0 | nchar(SPD_VoiceCondition) != 0 | nchar(Greens_VoiceCondition) != 0 | nchar(AfD_VoiceCondition)) & is.na(ratio_GreensVoice) == T, 0, ratio_GreensVoice),
         ratio_AfDVoice = if_else((nchar(CDUCSU_VoiceCondition) != 0 | nchar(SPD_VoiceCondition) != 0 | nchar(Greens_VoiceCondition) != 0 | nchar(AfD_VoiceCondition)) & is.na(ratio_AfDVoice) == T, 0, ratio_AfDVoice))