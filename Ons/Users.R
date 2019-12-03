#rm(list=ls())

# text analysis
# leave

getwd()
setwd("D:/rstudio/ons")

library(dplyr)

leave <- read.csv("Users.csv", encoding = "UTF-8")
