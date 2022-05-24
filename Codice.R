# Qui scriviamo il nostro codice raga

setwd("~/GitHub/Econometrics_project")
rm(list = ls())

library("readxl")
library("lubridate")
library("ggplot2")
library("patchwork")
library("olsrr")

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


################################################################################
# LINEAR REGRESSION
################################################################################


# Linear regression with all the variables

reg <- lm(EURUSD_vol ~ EURUSD + M3 + HICP + yieldEU_1y + MRO + ExtRes + RefOp, df)
summary(reg)


# La regressione con tutte le time series dà scarsi risultati, sicuramente perché ci sono time series
# con alta collinearità

ols_coll_diag(reg)

#Il VIF sopra 5/10 implica un'alta collinearità: yieldEU_1y e MRO sono altissimi, ExtRes e RefOp tra 5 e 10



# Proviamo a togliere MRO
reg <- lm(EURUSD_vol ~ EURUSD + M3 + HICP + yieldEU_1y + ExtRes + RefOp, df)
summary(reg)


ols_coll_diag(reg)

#Tutti i VIF sono minori di 10, ma l'R^2 fa schifo.

##########################
#TO DO:
# DIVIDERE I DUE MODELLI COME FA NEL PAPER (FARANNO SCHIFO DI SICURO LO STESSO)
# DISTRIBUTED LAG MODELS
##########################



####### MODELLO 1
reg <- lm(EURUSD_vol ~ M3 + MRO, df)
summary(reg)

# MODELLO 1 CON EURUSD, HICP E RefOp in più
reg <- lm(EURUSD_vol ~ M3 + MRO + EURUSD + HICP + RefOp, df)
summary(reg)

#R^2 uguale, R^2 adjusted più piccolo -> il secondo modello non aggiunge nulla
# (PER VERIFICARLO SI POTREBBE FARE IL CLASSICO TEST STATISTICO ALLA SECCHI)


####### MODELLO 2
reg <- lm(EURUSD_vol ~ yieldEU_1y + ExtRes, df)
summary(reg)

reg <- lm(EURUSD_vol ~ yieldEU_1y + ExtRes + EURUSD + HICP + RefOp, df)
summary(reg)

#Qui il modello migliora abbastanza - yield e HCIP non sono significative

ols_coll_diag(reg)

#Nessun VIF sopra 8







################################################################################
# UNIT ROOT TEST
################################################################################

############### ERS TEST

# H0: Non-stationary
# H1: stationary


# Se la statistica è maggiore di un certo valore critico => non posso rifiutare H0, quindi 
# non posso rifiutare la non stazionarità

library(urca)

summary(ur.ers(df$EURUSD, type="P-test", model="trend")) #non stat
summary(ur.ers(df$M3, type="P-test", model="trend")) #non stat
summary(ur.ers(df$HICP, type="P-test", model="trend")) #non stat
summary(ur.ers(df$MRO, type="P-test", model="trend")) #non stat
summary(ur.ers(df$yieldEU_1y, type="P-test", model="trend")) #non stat

summary(ur.ers(df$ExtRes, type="P-test", model="trend")) #al 5% e al 10% è stat, non lo è all'1%
summary(ur.ers(df$RefOp, type="P-test", model="trend")) #al 5% e al 10% è stat, non lo è all'1%
summary(ur.ers(df$EURUSD_vol, type="P-test", model="trend")) #sempre stat


# Per i processi non stazionari consideriamo le time series delle differenze

dEURUSD = diff(df$EURUSD)
dM3 = diff(df$M3)
dHICP = diff(df$HICP)
dMRO = diff(df$MRO)
dyield = diff(df$yieldEU_1y)
dExtRes = diff(df$ExtRes)
dRefOp = diff(df$RefOp)


# Inserisco le differenze nel dataset

df$dEURUSD = c(0, dEURUSD)
df$dM3 = c(0, dM3)
df$dHICP = c(0, dHICP)
df$dMRO = c(0, dMRO)
df$dyield = c(0, dyield)
df$dExtRes = c(0, dExtRes)
df$dRefOp = c(0, dRefOp)


# Plottiamo le differenze

p1 <- ggplot(df, aes(x=Data, y=dEURUSD)) +
  geom_line() + 
  xlab("")
p2 <- ggplot(df, aes(x=Data, y=dM3)) +
  geom_line() + 
  xlab("")
p3 <- ggplot(df, aes(x=Data, y=dHICP)) +
  geom_line() + 
  xlab("")
p4 <- ggplot(df, aes(x=Data, y=dMRO)) +
  geom_line() + 
  xlab("")
p5 <- ggplot(df, aes(x=Data, y=dyield)) +
  geom_line() + 
  xlab("")
p6 <- ggplot(df, aes(x=Data, y=dExtRes)) +
  geom_line() + 
  xlab("")
p7 <- ggplot(df, aes(x=Data, y=dRefOp)) +
  geom_line() + 
  xlab("")
p8 <- ggplot(df, aes(x=Data, y=EURUSD_vol)) +
  geom_line() + 
  xlab("")

p1+p2+p3+p4+p5+p6+p7+p8


# Calcolo l'ERS test

summary(ur.ers(df$dEURUSD, type="P-test", model="trend"))
summary(ur.ers(df$dM3, type="P-test", model="trend")) 
summary(ur.ers(df$dHICP, type="P-test", model="trend")) 
summary(ur.ers(df$dMRO, type="P-test", model="trend")) 
summary(ur.ers(df$dyield, type="P-test", model="trend")) 
summary(ur.ers(df$dExtRes, type="P-test", model="trend")) 
summary(ur.ers(df$dRefOp, type="P-test", model="trend")) 

# Sono tutti stazionari all'1%



############### ADF TEST

# H0: non stationary
# H1: stationary

# pvalue alto => non possiamo rifiutare H0

library(aTSA)


# Prima bisogna scegliere che tipo di test effettuare in base a come sono strutturati i dati
# Se no intercept e no trend, intercept e no trend o intercept e trend

adf.test(as.matrix(df$EURUSD))
adf.test(as.matrix(df$M3))
adf.test(as.matrix(df$HICP))
adf.test(as.matrix(df$MRO))
adf.test(as.matrix(df$yieldEU_1y))
adf.test(as.matrix(df$ExtRes)) # Con drift e trend con qualche lag rifiuto H0
adf.test(as.matrix(df$RefOp)) # Con drift e trend rifiuto sempre H0
adf.test(as.matrix(df$EURUSD_vol)) # Quasi sempre stat


# Applichiamo il test alle first differences

adf.test(as.matrix(df$dEURUSD))
adf.test(as.matrix(df$dM3))
adf.test(as.matrix(df$dHICP))
adf.test(as.matrix(df$dMRO))
adf.test(as.matrix(df$dyield))
adf.test(as.matrix(df$dExtRes))
adf.test(as.matrix(df$dRefOp)) 

# Vengono tutti stazionari, ovviamente


##### I processi vengono segnati stazionari ma le varianze non sembrano molto costanti
# Further analysis?






################################################################################
# COINTEGRATION
################################################################################


#PRIMO MODELLO 

library(vars)

y.VAR.IC <- VARselect(df[c("EURUSD_vol", "M3", "MRO")], type="const")
nlags <- y.VAR.IC$selection["SC(n)"]
nlags

#Dice di usare un lag (non ho capito bene questa cosa)


y.CA <- ca.jo(df[c("EURUSD_vol", "M3", "MRO")], type="trace", K=2)
summary(y.CA)

# Statistica più grande dei valori cruciali => rifiutiamo H0 (quindi la cosa che c'è scritta a sinistra)

# Accettiamo cointegrazione con r = 1 => un'equazione di cointegrazione


#SECONDO MODELLO

y.VAR.IC <- VARselect(df[c("EURUSD_vol", "yieldEU_1y", "ExtRes", "EURUSD", "HICP", "RefOp")], type="const")
nlags <- y.VAR.IC$selection["SC(n)"]
nlags

#Dice di usare un lag (non ho capito bene questa cosa)


y.CA <- ca.jo(df[c("EURUSD_vol", "M3", "MRO")], type="trace", K=2)
summary(y.CA)

# Accettiamo r = 1 => Un'equazione di cointegrazione




