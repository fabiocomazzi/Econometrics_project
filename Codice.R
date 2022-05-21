# Qui scriviamo il nostro codice raga

setwd("~/GitHub/Econometrics_project")
rm(list = ls())

library("readxl")
library("lubridate")

df = read_excel("dataset.xlsx", header = T)


