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

dev.new()
p8

dev.off()

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



###########################################
# DIVIDIAMO I DUE MODELLI COME FA NEL PAPER
###########################################



####### MODELLO 1
reg <- lm(EURUSD_vol ~ M3 + MRO, df)
summary(reg)

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")

# MODELLO 1 CON ExtRes in più
reg <- lm(EURUSD_vol ~ M3 + MRO + ExtRes, df)
summary(reg)

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")

# Il modello migliora un po' -> si vede dal t-test che ExtRes è significativo




####### MODELLO 2

# Modello del paper
reg <- lm(EURUSD_vol ~ yieldEU_1y + EURUSD, df) #come paper
summary(reg)

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")


# Modello del paper più nostre features
reg <- lm(EURUSD_vol ~ yieldEU_1y + EURUSD + HICP + RefOp, df) #paper più nostre variabili
summary(reg)

plot(df$Data, df$EURUSD_vol, type = "l")
lines(df$Data, fitted(reg), col = "green")

#Qui il modello migliora abbastanza -> per vedere se almeno una delle features che abbiamo inserito sono significative

linearHypothesis(reg, rbind(c(0,0,0,1,0), c(0,0,0,0,1)), c(0,0))

# => p-value alto => al 5% accetto H0 => le rifiuto entrambe






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


########## PRIMO MODELLO #######################################################

library(vars)

y.VAR.IC <- VARselect(df[c("EURUSD_vol", "M3", "MRO")], type="const")
nlags <- y.VAR.IC$selection["SC(n)"]
nlags

#Dice di usare due lag


y.CA <- ca.jo(df[c("EURUSD_vol", "M3", "MRO")], type="trace", ecdet = "const", spec="longrun", K=2)
summary(y.CA)


# Accettiamo cointegrazione con r = 1 => un'equazione di cointegrazione


plot(df$EURUSD_vol, type = "l")
lines(1.000*df$EURUSD_vol + 0.0006483369*df$M3 -0.0024*df$MRO, col = "green")

# merda, stiamo cannando tutto


# Statistica più grande dei valori cruciali => rifiutiamo H0 (quindi la cosa che c'è scritta a sinistra)


# r DICE CHE LA COMBINAZIONE LINEARE DI r TIME SERIES Dà UNA SERIE STAZIONARIA
# => SE r = 1, BASTA SOLO EURUSD_vol => NON STIAMO TROVANDO UN EMERITO CAZZO




########### PRIMO MODELLO + ExtRes #############################################

y.VAR.IC <- VARselect(df[c("EURUSD_vol", "M3", "MRO", "ExtRes")], type="const")
nlags <- y.VAR.IC$selection
nlags

#Dice di usare un lag -> ne usiamo due lo stesso e sta anche un po' zitto


y.CA <- ca.jo(df[c("EURUSD_vol", "M3", "MRO", "ExtRes")], type="trace", K=3, spec="longrun", ecdet = "const")
summary(y.CA)

# Una relazione di cointegrazione => ExtRes non aggiunge relazioni di cointegrazione

vecm<-cajorls(y.CA, r = 1)
vecm


################################################################################

############ SECONDO MODELLO ###################################################

y.VAR.IC <- VARselect(df[c("EURUSD_vol", "yieldEU_1y", "EURUSD")], type="const")
nlags <- y.VAR.IC$selection["SC(n)"]
nlags

#Dice di usare due lag


y.CA <- ca.jo(df[c("EURUSD_vol", "yieldEU_1y", "EURUSD")], type="trace", K=2, spec="longrun")
summary(y.CA)

# Una relazione di cointegrazione




############# SECONDO MODELLO + RefOp e HICP ###################################

y.VAR.IC <- VARselect(df[c("EURUSD_vol", "yieldEU_1y", "EURUSD", "RefOp", "HICP")], type="const")
nlags <- y.VAR.IC$selection
nlags

#Dice di usare un lag -> ne usiamo due e ce ne sbattiamo

y.CA <- ca.jo(df[c("EURUSD_vol", "yieldEU_1y", "EURUSD", "RefOp", "HICP")], type="trace", K=3,spec="longrun")
summary(y.CA)

vecm<-cajorls(y.CA, r = 1)
vecm
# Al 5% e 1% accettiamo una relazione di cointegrazione




############### MODELLO MISCHIONE

y.VAR.IC <- VARselect(df[c("M3", "MRO", "HICP", "ExtRes", "EURUSD")], type="const")
nlags <- y.VAR.IC$selection["SC(n)"]
nlags

#Dice di usare un lag


y.CA <- ca.jo(df[c( "M3", "MRO", "HICP", "ExtRes", "EURUSD")], type="trace", K=2, spec="longrun")
summary(y.CA)

# Accettiamo r = 3 => Tre equazioni di cointegrazione
















#### MODELLO 1 CRISI

volatilità_crisi = df$EURUSD_vol[30:51]
plot(volatilità_crisi, type = "l")

adf.test(as.matrix(volatilità_crisi))

M3 = df$M3[30:51]
MRO = df$MRO[30:51]
ExtRes = df$ExtRes[30:51]


y.VAR.IC <- VARselect(cbind(volatilità_crisi, M3, MRO, ExtRes), type="const")
nlags <- y.VAR.IC$selection
nlags


y.CA <- ca.jo(cbind(volatilità_crisi, M3, MRO, ExtRes), type="trace", K=3, spec="longrun", ecdet = "const")
summary(y.CA)

vecm<-cajorls(y.CA, r = 2)
vecm

plot(5.401592e-02 * MRO -2.010420e-06*ExtRes -1.656763e-01, type = "l")
lines(5.401592e-02 * MRO -2.010420e-06*ExtRes -1.656763e-01 + volatilità_crisi, col = "green")



#### MODELLO 2 CRISI

#"yieldEU_1y", "ExtRes", "EURUSD", "RefOp", "HICP"


yieldEU_1y = df$yieldEU_1y[30:51]
EURUSD = df$EURUSD[30:51]
RefOp = df$RefOp[30:51]
HICP = df$HICP[30:51]

y.VAR.IC <- VARselect(cbind(volatilità_crisi, EURUSD, yieldEU_1y, HICP), type="const")
nlags <- y.VAR.IC$selection
nlags

y.CA <- ca.jo(cbind(volatilità_crisi, EURUSD, yieldEU_1y, HICP), type="trace", K=3, spec="longrun", ecdet = "const")
summary(y.CA)

vecm<-cajorls(y.CA, r = 3)
vecm





