### Author: Konstantin Gavras

###00_dataDescription.R

load("E:/dataFull_ohnePara.RData")
load("E:/dataFull_Para.RData")

dataIdentifyComplete <- dataFull_Para %>% 
  dplyr::select(Goodbye_1_Trans_Time, c_0007, lfdn)

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