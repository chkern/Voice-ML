library(tidyverse)
library(stringr)
library(ggmosaic)
library(caret)
library(stargazer)

## Change path
wdpath <- "~/Uni/Forschung/Article/2020 - VoiceML/data"
setwd(wdpath)

afd1_s <- readRDS("voice_q1_afd_15s.rds")
afd1_l <- readRDS("voice_q1_afd_16s.rds")
cdu1_s <- readRDS("voice_q1_cdu_15s.rds")
cdu1_l <- readRDS("voice_q1_cdu_16s.rds")
greens1_s <- readRDS("voice_q1_greens_15s.rds")
greens1_l <- readRDS("voice_q1_greens_16s.rds")
spd1_s <- readRDS("voice_q1_spd_15s.rds")
spd1_l <- readRDS("voice_q1_spd_16s.rds")

load("survey_voice_experiment_2020_07_09_updated.RData")

afd <- afd1_s %>% 
  add_row(afd1_l) %>%
  rename_at(vars(-id), function(x) paste0(x, "_afd"))
cdu <- cdu1_s %>% 
  add_row(cdu1_l) %>%
  rename_at(vars(-id), function(x) paste0(x, "_cdu"))
greens <- greens1_s %>% 
  add_row(greens1_l) %>%
  rename_at(vars(-id), function(x) paste0(x, "_greens"))
spd <- spd1_s %>% 
  add_row(spd1_l) %>%
  rename_at(vars(-id), function(x) paste0(x, "_spd"))

survey_c <- data_voice_experiment %>%
  left_join(afd, by = c("tic" = "id")) %>%
  left_join(cdu, by = c("tic" = "id")) %>%
  left_join(greens, by = c("tic" = "id")) %>%
  left_join(spd, by = c("tic" = "id"))
