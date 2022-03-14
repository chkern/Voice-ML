
#Get duration of .wav files

library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(psych)
library(sjPlot)
library(tuneR)

wavFiles <- list.files("E:/final")
wavFiles <- wavFiles[wavFiles != "data"]
wavFiles <- wavFiles[wavFiles != "transcripts"]
wavFiles <- wavFiles[!str_detect(wavFiles, "Chancellor")]
wavFiles <- wavFiles[!str_detect(wavFiles, "Issue")]
wavFiles <- wavFiles[!str_detect(wavFiles, "Vignette")]
wavFiles <- wavFiles[!str_detect(wavFiles, "DENIED")]

dataList <- list()

for(i in 1:length(wavFiles)){
  condition = str_extract(wavFiles[i], ".*\\_")
  tic = str_extract(wavFiles[i], "[^_]+$")
  tic = str_remove(tic, "\\..*")
  
  audio <- readWave(paste0("E:/final/", wavFiles[i]), header = T)
  duration <- round(audio$samples / audio$sample.rate, 2)
  
  data <- tibble(condition, tic, duration)
  
  dataList[[i]] <- data
  
  rm(condition, tic, audio, duration, data)
}

dataDuration <- bind_rows(dataList, .id = "column_label")

dataDuration <- dataDuration %>% 
  mutate(followUp = if_else(str_detect(condition, "FU"), 1, 0),
         condition = str_remove(condition, "FU\\_"),
         condition = str_sub(condition, end = -2),
         party = str_extract(condition, "[^_]+$"),
         condition = str_remove(condition, "QA\\_"),
         condition = str_remove(condition, "\\_.*"),
         party = str_remove(party, "CSU")
         ) %>% 
  group_by(tic, condition, party) %>% 
  summarize(duration = sum(duration)) %>% 
  ungroup() %>% 
  dplyr::select(tic, party, duration)
