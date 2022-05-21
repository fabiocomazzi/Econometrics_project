# Qui scriviamo il nostro codice raga

setwd("~/GitHub/Econometrics_project")
rm(list = ls())

library("readxl")
library("lubridate")
library("ggplot2")
library("patchwork")

# Import the dataset

df = read_excel("dataset.xlsx")



# Plot of all the time series

p1 <- ggplot(df, aes(x=Data, y=EURUSD)) +
  geom_line() + 
  xlab("")
p2 <- ggplot(df, aes(x=Data, y=M3)) +
  geom_line() + 
  xlab("")
p3 <- ggplot(df, aes(x=Data, y=HICP)) +
  geom_line() + 
  xlab("")
p4 <- ggplot(df, aes(x=Data, y=MRO)) +
  geom_line() + 
  xlab("")
p5 <- ggplot(df, aes(x=Data, y=yieldEU_1y)) +
  geom_line() + 
  xlab("")
p6 <- ggplot(df, aes(x=Data, y=ExtRes)) +
  geom_line() + 
  xlab("")
p7 <- ggplot(df, aes(x=Data, y=RefOp)) +
  geom_line() + 
  xlab("")
p8 <- ggplot(df, aes(x=Data, y=EURUSD_vol)) +
  geom_line() + 
  xlab("")

p1+p2+p3+p4+p5+p6+p7+p8


# Linear regression with all the variables

reg <- lm(EURUSD_vol ~ EURUSD + M3 + HICP + yieldEU_1y + MRO + ExtRes + RefOp, df)
summary(reg)
