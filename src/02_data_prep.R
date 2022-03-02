library(tidyverse)
library(plyr)
library(data.table)
library(matrixStats)

## Change path
wdpath <- "~/Uni/Forschung/Article/2020 - VoiceML/data/Voice QA_3_Greens_15s+"
setwd(wdpath)

tbl_full <- data.frame()
files <- list.files(pattern = "*wav.log")
drop <- "QA_3_Greens_"

for(i in 1:length(files)){
   temp <- read_delim(files[i], delim = " ")     # Read log file
   if (nrow(temp) > 25) {                        # Check if log is empty
     temp <- temp[temp[, 2] == "class", c(3, 5)] # Clean up
     t_temp <- transpose(temp[,2])
     id <- gsub(drop, "", files[i])    # Get and clean ID from file
     id <- gsub(".wav.log", "", id)
     t_temp$id <- id
     } else {t_temp <- data.frame()}
   tbl_full <- rbind.fill(tbl_full, t_temp)      # Combine
   print(i)
}

# Column names
classes <- c("e1_anger", "e1_boredom", "e1_disgust", "e1_fear", "e1_happiness", "e1_neutral", "e1_sadness", 
             "e2_agressiv", "e2_cheerful", "e2_intoxicated", "e2_nervous", "e2_neutral", "e2_tired",
             "e3_loi1", "e3_loi2", "e3_loi3")
n_classes <- length(classes)
n_cols <- ncol(tbl_full) - 1
reps <- rep(classes, n_cols / n_classes)
nums <- rep(1:(n_cols / n_classes), each = n_classes)
cols <- paste0(classes, "_", nums)
cols <- c("id", cols)

tbl_full <- select(tbl_full, id, everything())
names(tbl_full) <- cols

# Number of predictions over time
tbl_clean <- tbl_full %>% 
  mutate(n_preds = (rowSums(!is.na(.)) - 1)/ n_classes)

# Aggregate probabilities for classes over time
tbl_clean <- tbl_clean %>% 
  mutate_at(vars(-id), as.numeric) %>% 
  mutate(e1_anger_m = rowMeans(select(., contains("e1_anger")), na.rm = T)) %>%
  mutate(e1_boredom_m = rowMeans(select(., contains("e1_boredom")), na.rm = T)) %>%
  mutate(e1_disgust_m = rowMeans(select(., contains("e1_disgust")), na.rm = T)) %>%
  mutate(e1_fear_m = rowMeans(select(., contains("e1_fear")), na.rm = T)) %>%
  mutate(e1_happiness_m = rowMeans(select(., contains("e1_happiness")), na.rm = T)) %>%
  mutate(e1_neutral_m = rowMeans(select(., contains("e1_neutral")), na.rm = T)) %>%
  mutate(e1_sadness_m = rowMeans(select(., contains("e1_sadness")), na.rm = T)) %>%
  mutate(e2_agressiv_m = rowMeans(select(., contains("e2_agressiv")), na.rm = T)) %>%
  mutate(e2_cheerful_m = rowMeans(select(., contains("e2_cheerful")), na.rm = T)) %>%
  mutate(e2_intoxicated_m = rowMeans(select(., contains("e2_intoxicated")), na.rm = T)) %>%
  mutate(e2_nervous_m = rowMeans(select(., contains("e2_nervous")), na.rm = T)) %>%
  mutate(e2_neutral_m = rowMeans(select(., contains("e2_neutral")), na.rm = T)) %>%
  mutate(e2_tired_m = rowMeans(select(., contains("e2_tired")), na.rm = T)) %>%
  mutate(e3_loi1_m = rowMeans(select(., contains("e3_loi1")), na.rm = T)) %>%
  mutate(e3_loi2_m = rowMeans(select(., contains("e3_loi2")), na.rm = T)) %>%
  mutate(e3_loi3_m = rowMeans(select(., contains("e3_loi3")), na.rm = T)) %>%
  mutate(e3_loi1_med = rowMedians(as.matrix(select(., contains("e3_loi1"))), na.rm = T)) %>%
  mutate(e3_loi2_med = rowMedians(as.matrix(select(., contains("e3_loi2"))), na.rm = T)) %>%
  mutate(e3_loi3_med = rowMedians(as.matrix(select(., contains("e3_loi3"))), na.rm = T)) %>%
  mutate(e3_loi1_v = rowVars(as.matrix(select(., contains("e3_loi1"))), na.rm = T)) %>%
  mutate(e3_loi2_v = rowVars(as.matrix(select(., contains("e3_loi2"))), na.rm = T)) %>%
  mutate(e3_loi3_v = rowVars(as.matrix(select(., contains("e3_loi3"))), na.rm = T)) %>%
  mutate(e3_loi1_iqr = rowIQRs(as.matrix(select(., contains("e3_loi1"))), na.rm = T)) %>%
  mutate(e3_loi2_iqr = rowIQRs(as.matrix(select(., contains("e3_loi2"))), na.rm = T)) %>%
  mutate(e3_loi3_iqr = rowIQRs(as.matrix(select(., contains("e3_loi3"))), na.rm = T))
  
e1_f <- ncol(tbl_clean) - 24
e1_l <- ncol(tbl_clean) - 18
e2_f <- ncol(tbl_clean) - 17
e2_l <- ncol(tbl_clean) - 12
e3_f <- ncol(tbl_clean) - 11
e3_l <- ncol(tbl_clean) - 9
e3_fm <- ncol(tbl_clean) - 8
e3_lm <- ncol(tbl_clean) - 6

# Assign classes based on highest mean probability
e1_class <- colnames(tbl_clean[,e1_f:e1_l])[max.col(tbl_clean[,e1_f:e1_l], ties.method="first")]
e2_class <- colnames(tbl_clean[,e2_f:e2_l])[max.col(tbl_clean[,e2_f:e2_l], ties.method="first")]
e3_class <- colnames(tbl_clean[,e3_f:e3_l])[max.col(tbl_clean[,e3_f:e3_l], ties.method="first")]
e3_class_med <- colnames(tbl_clean[,e3_fm:e3_lm])[max.col(tbl_clean[,e3_fm:e3_lm], ties.method="first")]

voice_q3_greens <- tbl_clean %>% 
  mutate(e1_class = e1_class,
         e2_class = e2_class,
         e3_class = e3_class,
         e3_class_med = e3_class_med) %>% 
  select(id, contains(c("_m", "_v", "_iqr")), n_preds, e1_class, e2_class, e3_class, e3_class_med)

# Save RDS file
saveRDS(voice_q3_greens, 'voice_q3_greens_16s.rds')
