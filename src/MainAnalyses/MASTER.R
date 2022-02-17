###MASTER

library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
# require(devtools)
# install_version("rzeit2", version = "0.2.3", repos = "http://cran.us.r-project.org")
# library(rzeit2)
library(psych)
library(stargazer)
library(sjPlot)
library(tuneR)

load("senti_ws.RData")

###Data description survey
source("00_dataDescription.R")

#Data import and cleaning
source("01_dataImportCleaning.R")

#Analysis descriptive
source("02_analysisDesc.R", echo = T)

#Factor structure
source("03_factorAnalysis.R", echo = T)

#Regression analysis
source("04_regressionAnalysis.R", echo = T)

#Analyses for paper
source("05_paperAnalysis.R", echo = T)