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

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")

# La regressione con tutte le time series dà scarsi risultati, sicuramente perché ci sono time series
# con alta collinearità

# In più, HCIP non è significativa (nè con il t-test nè con Bonferroni (?))


# Test H0: beta3 = 0 vs H1: beta3 != 0 (HICP)
linearHypothesis(reg, c(0,0,0,1,0,0,0,0), c(0)) #infatti viene lo stesso risultato del t-test di prima

# pvalue alto => togliamo HICP

# Calcoliamo i VIF

ols_coll_diag(reg)

#Il VIF sopra 5/10 implica un'alta collinearità: yieldEU_1y e MRO sono altissimi (sono super collineari), 
# ExtRes e RefOp tra 5 e 10


# Test H0: beta4 = 0 e beta5 = 0 vs H1: at least one of beta4 and beta5 != 0 (yield e MRO)
linearHypothesis(reg, rbind(c(0,0,0,0,1,0,0,0), c(0,0,0,0,0,1,0,0)), c(0,0))

# => MRO e yield sono molto collineari però almeno uno dei due è significativo => proviamo a togliere yield





# Proviamo a togliere yieldEU_1y e HCIP
reg <- lm(EURUSD_vol ~ EURUSD + M3 + MRO + ExtRes + RefOp, df)
summary(reg)

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")

ols_coll_diag(reg)

#Tutti i VIF sono minori di 10, ma l'R^2 fa schifo (e anche il plot)


##########################
#TO DO:
# DIVIDERE I DUE MODELLI COME FA NEL PAPER (FARANNO SCHIFO DI SICURO LO STESSO)
# DISTRIBUTED LAG MODELS
##########################



####### MODELLO 1
reg <- lm(EURUSD_vol ~ M3 + MRO, df)
summary(reg)

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")

# MODELLO 1 CON EURUSD, HICP E RefOp in più
reg <- lm(EURUSD_vol ~ M3 + MRO + EURUSD + HICP + RefOp, df)
summary(reg)

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")

#R^2 uguale, R^2 adjusted più piccolo -> il secondo modello forse non aggiunge nulla

# Facciamo il test

# Test beta3 = 0, beta4 = 0, beta5 = 0 vs at least one != 0

linearHypothesis(reg, rbind(c(0,0,0,1,0,0), c(0,0,0,0,1,0), c(0,0,0,0,0,1)), c(0,0,0))

# p-value alto => non posso rifiutare H0 => (beta3, beta4, beta5) = (0,0,0)





####### MODELLO 2

# Modello del paper
reg <- lm(EURUSD_vol ~ yieldEU_1y + ExtRes, df) #come paper
summary(reg)

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")


# Modello del paper più nostre features
reg <- lm(EURUSD_vol ~ yieldEU_1y + ExtRes + EURUSD + HICP + RefOp, df) #paper più nostre variabili
summary(reg)

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")

#Qui il modello migliora abbastanza -> per vedere se almeno una delle features che abbiamo inserito sono significative

linearHypothesis(reg, rbind(c(0,0,0,1,0,0), c(0,0,0,0,1,0), c(0,0,0,0,0,1)), c(0,0,0))

# => p-value basso => almeno una di quelle è significativa



# yield e HCIP sembrano non significative singolarmente -> facciamo il test congiunto

# Test H0: (beta1, beta4) = 0 vs (beta1, beta4) != 0
linearHypothesis(reg, rbind(c(0,1,0,0,0,0), c(0,0,0,0,1,0)), c(0,0))

# pvalue alto => tolgo entrambe dal modello


reg <- lm(EURUSD_vol ~ ExtRes + EURUSD + RefOp, df) #paper più nostre variabili
summary(reg)


# ora tutte le variabili sono significative

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")


# Controlliamo il VIF

ols_coll_diag(reg)

#Nessun VIF sopra 8


# In generale, dai plot si vede che la regressione fa schifo






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
summary(ur.ers(df$EURUSD_vol, type="P-test", model="trend")) #sempre stat -> male


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


# PROBLEMA: ExtRes, RefOp e EURUSD_vol sono stazionari: come possiamo imporre la cointegrazione?
# SECONDO PROBLEMA: Stazionarietà dovrebbe implicare std costante, ma non mi sembra


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

#Dice di usare due lag (non ho capito bene questa cosa)


y.CA <- ca.jo(df[c("EURUSD_vol", "M3", "MRO")], type="trace", K=2)
summary(y.CA)

# Statistica più grande dei valori cruciali => rifiutiamo H0 (quindi la cosa che c'è scritta a sinistra)

# Accettiamo cointegrazione con r = 1 => un'equazione di cointegrazione





#SECONDO MODELLO

y.VAR.IC <- VARselect(df[c("EURUSD_vol", "ExtRes", "EURUSD", "RefOp")], type="const")
nlags <- y.VAR.IC$selection["SC(n)"]
nlags

#Dice di usare due lag (non ho capito bene questa cosa)


y.CA <- ca.jo(df[c("EURUSD_vol", "ExtRes", "EURUSD", "RefOp")], type="trace", K=2)
summary(y.CA)

# Accettiamo r = 2 => Due equazioni di cointegrazione




# Provo a mettere tutte le features:

y.VAR.IC <- VARselect(df[c("EURUSD_vol", "M3", "MRO", "yieldEU_1y", "HICP", "ExtRes", "EURUSD", "RefOp")], type="const")
nlags <- y.VAR.IC$selection["SC(n)"]
nlags

#Dice di usare due lag (non ho capito bene questa cosa)


y.CA <- ca.jo(df[c("EURUSD_vol", "M3", "MRO", "yieldEU_1y", "HICP", "ExtRes", "EURUSD", "RefOp")], type="trace", K=2)
summary(y.CA)

# Accettiamo r = 3 => Tre equazioni di cointegrazione





