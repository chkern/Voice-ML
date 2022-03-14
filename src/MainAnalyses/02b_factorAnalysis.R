
#03_factorAnalysis

surveyFactor <- survey_c %>% 
  drop_na() %>% 
  dplyr::select(contains("e1_"), contains("e2_"), contains("e3_")) %>% 
  dplyr::select(!c(e1_class, e2_class, e3_class))

surveyFactor_e1 <- surveyFactor %>% 
  dplyr::select(contains("e1_"))

surveyFactor_e2 <- surveyFactor %>% 
  dplyr::select(contains("e2_"))


###E1 Emotions
#Correlations
cor(surveyFactor_e1)

print(xtable::xtable(cor(surveyFactor_e1)), type="html", file="Correlation_Emotions_e1.html")

items.parallel <- fa.parallel(surveyFactor_e1, fa="fa")

#Factor loadings
factors_e1_2fac <- fa(surveyFactor_e1, 
                 nfactors=2,
                 n.obs = nrow(surveyFactor_e1),
                 SMC=TRUE,
                 fm="ml",
                 rotate="promax",
                 max.iter=100
)

print(factors_e1_2fac$loadings, cutoff=.2)

print(xtable::xtable(print(factors_e1_2fac$loadings[1:7, 1:2], cutoff=.2)), type="html", file="EFA_2fac_Emotions_e1.html")

factors_e1_1fac <- fa(surveyFactor_e1, 
                 nfactors=1,
                 n.obs = nrow(surveyFactor_e1),
                 SMC=TRUE,
                 fm="ml",
                 rotate="promax",
                 max.iter=100
)

print(factors_e1_1fac$loadings, cutoff=.2)

e1_efa1 <- data.frame(round(factors_e1_1fac$loadings[1:7,],2))
colnames(e1_efa1) <- "ML1"

print(xtable::xtable(print(e1_efa1)), type="html", file="EFA_1fac_Emotions_e1.html")



###E2 Emotions
#Correlations
cor(surveyFactor_e2)

print(xtable::xtable(cor(surveyFactor_e2)), type="html", file="Correlation_Emotions_e2.html")

items.parallel <- fa.parallel(surveyFactor_e2, fa="fa")

#Factor loadings
factors_e2_2fac <- fa(surveyFactor_e2, 
                      nfactors=2,
                      n.obs = nrow(surveyFactor_e2),
                      SMC=TRUE,
                      fm="ml",
                      rotate="promax",
                      max.iter=100
)

print(factors_e2_2fac$loadings, cutoff=.2)

print(xtable::xtable(print(factors_e2_2fac$loadings[1:6, 1:2], cutoff=.2)), type="html", file="EFA_2fac_Emotions_e2.html")

factors_e2_1fac <- fa(surveyFactor_e2, 
                      nfactors=1,
                      n.obs = nrow(surveyFactor_e2),
                      SMC=TRUE,
                      fm="ml",
                      rotate="promax",
                      max.iter=100
)

print(factors_e2_1fac$loadings, cutoff=.2)

e2_efa1 <- data.frame(round(factors_e2_1fac$loadings[1:6,],2))
colnames(e2_efa1) <- "ML1"

print(xtable::xtable(print(e2_efa1)), type="html", file="EFA_1fac_Emotions_e2.html")