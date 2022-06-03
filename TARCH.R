
setwd("~/GitHub/Econometrics_project")

rm(list = ls())

library("readxl")
library("lubridate")
library("ggplot2")
library("patchwork")
library("olsrr")
library("dynlm")
library("car")

# Import pf the dataset

df = read_excel("dataset.xlsx")
df_1 = read.csv("MABMM301USM189S.csv")
df$M3_USA = df_1$MABMM301USM189S
df_1 = read.csv("DGS1.csv")
df$yieldUSA_1y = df_1$DGS1
df_1 = read.csv("DCOILWTICO.csv")
df_1 <- head(df_1, -1) 
df$Petrol_USA = df_1$DCOILWTICO
inf_df = read.table("inf_rate.txt", sep = "")

inf_rate = inf_df[2:(length(inf_df)-1)]

d = c()
for (i in 1: 19)
{
  d = c(d, inf_rate[20-i,])
}

d = d[10:(length(d)-9)]
d

d = as.numeric(d)
df$inf_USA = d


# Plot of all our time series

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
p7 <- ggplot(df, aes(x=Data, y=yieldUSA_1y)) +
  geom_line() + 
  xlab("")
p8 <- ggplot(df, aes(x=Data, y=M3_USA)) +
  geom_line() + 
  xlab("")
p9 <- ggplot(df, aes(x=Data, y=Petrol_USA)) +
  geom_line() + 
  xlab("")
p10 <- ggplot(df, aes(x=Data, y=inf_USA)) +
  geom_line() + 
  xlab("")


p1+p2+p3+p4+p5+p6+p7+p8+p9+p10



################################################################################
################################################################################
#
# FIRST MODEL: Regression on I(0) variables
#
################################################################################
################################################################################


# We want to see if we can find a relationship between our I(0) variables and
# the differences in exchange rate EUR/USD


################################################################################
# UNIT ROOT TEST
################################################################################

# First, we test whether our time series are I(1)


# Before doing so, we introduce another time series:
# Difference between interest rates in USA w.r.t. the
# ones in EU

df$yield_diff = df$yieldUSA_1y - df$yieldEU_1y


p11 <- ggplot(df, aes(x=Data, y=yield_diff)) +
  geom_line() + 
  xlab("")

p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11



############### ERS TEST

# H0: Non-stationary
# H1: stationary


# Se la statistica è maggiore di un certo valore critico => non posso rifiutare H0, quindi 
# non posso rifiutare la non stazionarità

library(urca)

summary(ur.ers(df$EURUSD, type="P-test", model="trend"))
summary(ur.ers(df$M3, type="P-test", model="trend")) 
summary(ur.ers(df$HICP, type="P-test", model="trend")) 
summary(ur.ers(df$MRO, type="P-test", model="trend")) 
summary(ur.ers(df$yieldEU_1y, type="P-test", model="trend")) 
summary(ur.ers(df$ExtRes, type="P-test", model="trend")) #al 5% è stat
summary(ur.ers(df$M3_USA, type="P-test", model="trend"))
summary(ur.ers(df$inf_USA, type="P-test", model="trend"))
summary(ur.ers(df$Petrol_USA, type="P-test", model="trend")) #al 5% è stat
summary(ur.ers(df$yieldUSA_1y, type="P-test", model="trend"))
summary(ur.ers(df$yield_diff, type="P-test", model="trend"))


# We now create the first differences of our time series

dEURUSD = diff(df$EURUSD)
dM3 = diff(df$M3)
dHICP = diff(df$HICP)
dMRO = diff(df$MRO)
dyieldEU = diff(df$yieldEU_1y)
dExtRes = diff(df$ExtRes)
dM3_USA = diff(df$M3_USA)
dyieldUSA = diff(df$yieldUSA_1y)
dinf_USA = diff(df$inf_USA)
dPetrol_USA = diff(df$Petrol_USA)
dyield_diff = diff(df$yield_diff)


# We insert these new time series in the dataset

df$dEURUSD = c(NA, dEURUSD)
df$dM3 = c(NA, dM3)
df$dHICP = c(NA, dHICP)
df$dMRO = c(NA, dMRO)
df$dyieldEU = c(NA, dyieldEU)
df$dExtRes = c(NA, dExtRes)
df$dM3_USA = c(NA, dM3_USA)
df$dyieldUSA = c(NA, dyieldUSA)
df$dinf_USA = c(NA, dinf_USA)
df$dPetrol_USA = c(NA, dPetrol_USA)
df$dyield_diff = c(NA, dyield_diff)


# Plot of the differences

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
p5 <- ggplot(df, aes(x=Data, y=dyieldEU)) +
  geom_line() + 
  xlab("")
p6 <- ggplot(df, aes(x=Data, y=dExtRes)) +
  geom_line() + 
  xlab("")
p7 <- ggplot(df, aes(x=Data, y=dyieldUSA)) +
  geom_line() + 
  xlab("")
p8 <- ggplot(df, aes(x=Data, y=dM3_USA)) +
  geom_line() + 
  xlab("")
p9 <- ggplot(df, aes(x=Data, y=dPetrol_USA)) +
  geom_line() + 
  xlab("")
p10 <- ggplot(df, aes(x=Data, y=dinf_USA)) +
  geom_line() + 
  xlab("")
p11 <- ggplot(df, aes(x=Data, y=dyield_diff)) +
  geom_line() + 
  xlab("")


p1+p2+p3+p4+p5+p6+p8+p9+p10+p11



# We calculate the ERS test on the differences

summary(ur.ers(df$dEURUSD, type="P-test", model="trend"))
summary(ur.ers(df$dM3, type="P-test", model="trend")) 
summary(ur.ers(df$dHICP, type="P-test", model="trend")) 
summary(ur.ers(df$dMRO, type="P-test", model="trend")) 
summary(ur.ers(df$dyieldEU, type="P-test", model="trend")) 
summary(ur.ers(df$dExtRes, type="P-test", model="trend"))
summary(ur.ers(df$dM3_USA, type="P-test", model="trend"))
summary(ur.ers(df$dinf_USA, type="P-test", model="trend"))
summary(ur.ers(df$dPetrol_USA, type="P-test", model="trend"))
summary(ur.ers(df$dyieldUSA, type="P-test", model="trend"))
summary(ur.ers(df$dyield_diff, type="P-test", model="trend"))

# They are all stationary at 5% (and also at 1%)





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
adf.test(as.matrix(df$ExtRes)) # Borderline: with drift and trend for 5% it depends
                               # on the lags I consider (at 5%)
adf.test(as.matrix(df$yieldUSA_1y))
adf.test(as.matrix(df$M3_USA))
adf.test(as.matrix(df$Petrol_USA)) # with 0, 4 and 5 lags it is stationary, with 1, 2, 3
                                   # instead it is stationary (at 5%)
adf.test(as.matrix(df$inf_USA))
adf.test(as.matrix(df$yield_diff))



# We apply the test to the first differences

adf.test(as.matrix(df$dEURUSD))
adf.test(as.matrix(df$dM3))
adf.test(as.matrix(df$dHICP))
adf.test(as.matrix(df$dMRO))
adf.test(as.matrix(df$dyieldEU))
adf.test(as.matrix(df$dExtRes)) 
adf.test(as.matrix(df$dyieldUSA))
adf.test(as.matrix(df$dM3_USA))
adf.test(as.matrix(df$dPetrol_USA))
adf.test(as.matrix(df$dinf_USA))
adf.test(as.matrix(df$dyield_diff))

# They are all stationary => All the time series are I(1) 
# (except, maybe, for ExtRes and Petrol)



###############################################################################
#Siccome tutte le variabili sono I(1) la regressione sulle variabili originarie
#non ha senso a meno che non siano cointegrate (relazioni spurie) -> regressione
#serie differenziate



reg = dynlm(dEURUSD ~ L(dExtRes, 1) + L(dM3,1) + L(dMRO,1), data=df)
summary(reg)


# Test on dM3 and dMRO

linearHypothesis(reg, rbind(c(0,0,1,0), c(0,0,0,1)), c(0,0))

#p-value 0.8739: reject H0


reg = dynlm(dEURUSD ~ L(dExtRes, 1), data=df)
summary(reg)

plot(reg)

acf(reg$residuals)
pacf(reg$residuals)
shapiro.test(reg$residuals)


# Try regression with all our I(0) time series

reg = dynlm(dEURUSD ~ L(dM3,1) + L(dHICP,1) + L(dMRO,1) + L(dinf_USA,1) 
                    + L(dyield_diff,1) + L(dPetrol_USA,1) + L(dExtRes, 1), data=df)
summary(reg)


plot(reg)

acf(reg$residuals)
pacf(reg$residuals)
shapiro.test(reg$residuals)


#la regressione non è malaccio, R^2 del 28%, i residui sono normali, e non sembra esserci 
# autocorrelazione quindi c'è consistenza
# MRO e M3 non viene significativo quindi li scartiamo -> facciamo un test congiunto


# Test H0: beta2 = 0 e beta4 = 0 vs H1: at least one of beta2 and beta4 != 0 (dM3 e dMRO)

linearHypothesis(reg, rbind(c(0,1,0,0,0,0,0,0), c(0,0,0,1,0,0,0,0)), c(0,0))

# p-value: 0.3419 => we remove dM3 and dMRO


reg = dynlm(dEURUSD ~ L(dHICP,1) + L(dinf_USA,1) + L(dyield_diff,1) 
                    + L(dPetrol_USA,1) + L(dExtRes), data=df)
summary(reg)

acf(reg$residuals)
pacf(reg$residuals)
shapiro.test(reg$residuals)


#Plot of dEURUSD and our regression

plot(df$Data, c(0, dEURUSD), type = "l")
lines(df$Data, c(0, fitted(reg)), col = "green")




################################################################################
################################################################################
#
# SECOND MODEL: VECM
#
################################################################################
################################################################################


# We want to find a long term relationship between exchange rate EUR/USD and our
# time series.
# Then, we exploit the residuals of the deriving VECM in order to estimate the 
# exchange rate volatility through a GARCH (O QUALCOSA DI DIVERSO?) model.


################################################################################

library(vars)

y.VAR.IC <- VARselect(df[c("EURUSD", "M3", "yieldEU_1y", "HICP", "ExtRes", 
                           "yieldUSA_1y", "Petrol_USA", "inf_USA")], type="const")
nlags <- y.VAR.IC$selection
nlags

# We select two lags


y.CA <- ca.jo(df[c("EURUSD", "M3", "yieldEU_1y", "HICP", "ExtRes", 
                   "yieldUSA_1y", "Petrol_USA", "inf_USA")], 
              type="trace", ecdet = "const", spec="longrun", K=2)
summary(y.CA)


# We have evidence for two cointegration relationships

vecm<-cajorls(y.CA, r = 2)


# We see the two cointegrating relationships

vecm


# We see our VECM

summary(vecm$rlm)


res = vecm$rlm$residuals[,1]

plot(res^2, type = "l")

plot(res, type = "l")



################ Now we consider the difference between EU and USD interest rates

y.VAR.IC <- VARselect(df[c("EURUSD", "M3", "yield_diff", "HICP", 
                           "ExtRes", "Petrol_USA", "inf_USA")], type="const")
nlags <- y.VAR.IC$selection
nlags

#Dice di usare due lag


y.CA <- ca.jo(df[c("EURUSD", "M3", "yield_diff", "HICP", 
                   "ExtRes", "Petrol_USA", "inf_USA")], 
              type="trace", ecdet = "const", spec="longrun", K=2)

summary(y.CA)

# There is only one cointegration relationship

vecm<-cajorls(y.CA, r = 1)

# Let's see the cointegration relationship
vecm


summary(vecm$rlm)


res = vecm$rlm$residuals[,1]


plot(res^2, type='l')

shapiro.test(res) 

hist(res, breaks = 20)

# non sono per niente normali -> leptokurtic -> vediamo se la varianza
# varia nel tempo

plot(res, type = "l")

# It varies, so we could think about a ARCH/GARCH model to describe it

acf(res^2)
pacf(res^2)

acf(res)
pacf(res)

# le relazioni di cointegrazione sono 1 ed è super significativa per spiegare andamento tasso di cambio (perché è supersignificativa?)
# le relazioni di short term significative sono invece riguardanti external reserve, prezzo petrolio, e inflazione usa
# R^2 è uguale a 28.75% ma è paragonabile ai risultati di semplice regressione
# inflazione EU invece non sembra particolarmente significativa

#i residui di questo modello sono ben centrati, no autocorrelazione etc. (NON MI PARE)
#usiamo la volatilità di questi residui per studiare la volatilità del tasso di cambio
#è la nostra alternativa alla semplice volatilità del sliding windows


# Plot of empirical volatility and our regressors

plot(df$EURUSD_vol, type='l')
lines(res^2, col = "red")

df$EURUSD_vol_cointegration = c(NA,NA, res^2)


################################################################################
# ACF E PACF
################################################################################

plot(acf(df$EURUSD, 12, xlim=c(1,12)))
plot(acf(df$EURUSD, 12, type = "partial", xlim = c(1,12)))

plot(acf(res, 12, xlim=c(1,12)))
plot(acf(res, 12, type = "partial", xlim = c(1,12)))

hist(res, breaks = 20) #leptokurtic

shapiro.test(res) # rifiutiamo H0 al 5% => non è normale


################################################################################
################################################################################
# THIRD MODEL: VOLATILITY
################################################################################
################################################################################


# Modellizziamo la volatilità con 3 approcci:
# - volatilità intesa come volatilità mensile
# - volatilità come i residui del nostro modello di cointegrazione
# - modelliamo il tasso di interesse direttamente con un ARIMA in media e GARCH in varianza
# l'idea è di vedere se politiche di aumento o diminuzione dei tassi possono influenzare la volatilità


################################################################################




# Creation of dummy variable

dummy = as.numeric(as.logical(diff(df$MRO)))
dummy = c(NA, dummy)
plot(df$MRO, type = "l")
lines(dummy, col = "green")


df$ECB_MROaction = as.data.frame(dummy)



################################################################################



# Facciamo un test per vedere se i nostri residui hanno un ARCH effect (LM test)

library(FinTS)
ArchTest(res, lag = 2, demean = TRUE) # con un lag non viene significativo, boh


# Altro modo per fare lo stesso test -> decido io il numero di lags

res_adj = c(0,0,res)
length(res_adj)

res_adj_sq = ts(res_adj^2)

mod_arch = dynlm(res_adj_sq ~ L(res_adj_sq) + L(res_adj_sq, 2))
summary(mod_arch)




#########################################################
# APPROCCIO 1: vediamo se a decisioni della banca centrale 
# per abbassare o alzare tassi corrisponde volatilità maggiore
# nel nostro modello di cointegrazione
# Interpretazione: abbiamo un modello che spiega abbastanza bene il cambio 
# ----> vediamo se decisioni di politica monetaria danno scossoni non previsti dal modello
# modellizziamo i residui come se fossero una serie temporale a media nulla
# e ammettiamo un modello GARCH con regressori esterni

library(rugarch)
ecb_action = c(as.numeric( as.logical(diff(df$MRO))))

#external_data = cbind(ecb_action, ecb_action.l1, ecb_action.l2, ecb_action.l3)
spec = ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1,1), external.regressors = as.matrix(ecb_action)), 
                  distribution.model="std", mean.model=list(armaOrder=c(0,0), include.mean = FALSE))

fit = ugarchfit(spec=spec, data=res[1:length(res)])
fit


fit@fit$coef

fit@fit$fitted.values

# gli effetti del cambio dei tassi non si vedono minimamente nella varianza



# #tGARCH
# 
# garchMod <- ugarchspec(variance.model=list(model="fGARCH",
#                                            garchOrder=c(2,2),
#                                            submodel="TGARCH",
#                                            external.regressors = as.matrix(dummy)),
#                        mean.model=list(armaOrder=c(0,0)), 
#                        distribution.model="std")
# garchFit <- ugarchfit(spec=garchMod, data=res_adj)
# coef(garchFit)
# 
# garchFit
# 
# 
# ############### Altra libreria
# 
# library(fGarch)
# arch.fit <- garchFit(~garch(1,1), data = res_adj)
# summary(arch.fit)
# 
# plot(arch.fit@fitted, type = "l")



####################################################################
#APPROCCIO 2: MODELLIAMO TUTTO TRAMITE ARIMA E GARCH

library(forecast)
EURUSD = ts(df$EURUSD, frequency = 12, start=c(2004,10), end=c(2022,3))
plot(diff(log(EURUSD)), type = "l")

acf(diff(EURUSD))
pacf(diff(EURUSD))

acf(diff(log(EURUSD)))
pacf(diff(log(EURUSD)))

mod = auto.arima(log(EURUSD), d=1) #modello selezionato è arima (1,1,0)
mod
summary(lm(diff(log(EURUSD)) ~ c(tail(diff(log(EURUSD)),-1),0)))#la media non è significativa quindi la toglieremo del modello

plot(EURUSD, type='l')
lines(exp(mod$fitted), col='green')
pacf(mod$residuals)

# ecb_decisions = cbind(diff(df$M3), diff(df$MRO), diff(df$ExtRes))
# ecb_decisions.l1 = cbind(c(tail(diff(df$M3), -1), 0), c(tail(diff(df$MRO), -1), 0), c(tail(diff(df$ExtRes), -1), 0))
# ecb_decisions.l2 = cbind(c(tail(diff(df$M3), -2),0,0), c(tail(diff(df$MRO), -2), 0, 0), c(tail(diff(df$ExtRes), -2), 0, 0))
# dummy = as.numeric(as.logical(diff(df$MRO)))
# dummy.a1 = c(0, dummy[1:(length(dummy)-1)])
# dummy.l1 = c(tail(dummy, -1), 0)

spec = ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1,1), external.regressors = as.matrix(tail(ecb_action, -1))), 
                  distribution.model="std", mean.model=list(armaOrder=c(1,0), include.mean = FALSE))

fit = ugarchfit(spec=spec, data=diff(log(EURUSD))[2:209])

fit

####le decisioni di politica monetaria non sembrano avere un significativa influenza sulla volatilità della politica monetaria
####unico effetto apprezzabile riguarda se c'è stato o meno un ritocco dei tassi MRO nel mese corrente ma c'è poca significatività

########################################################################
# APPROCCIO 3: Modellizziamo direttamente la volatilità mensile tramite modelli ARMA

plot(df$EURUSD_vol, type='l')
acf(df$EURUSD_vol)
pacf(df$EURUSD_vol)
mod0 = auto.arima(df$EURUSD_vol[2:length(df$EURUSD_vol)])
mod0
1-mod0$sigma2/var(df$EURUSD_vol) #che cos'è?

#external regressor MRO
mod1 = auto.arima(df$EURUSD_vol[2:length(df$EURUSD_vol)], xreg = as.matrix(diff(df$MRO)))
mod1
1-mod1$sigma2/var(df$EURUSD_vol)
#external regressor dummy MRO
mod2 = auto.arima(df$EURUSD_vol[2:length(df$EURUSD_vol)], xreg = as.matrix(ecb_action))
mod2
1-mod2$sigma2/var(df$EURUSD_vol)
#external regressor

acf(mod1$residuals)
pacf(mod1$residuals)


reg = dynlm(diff(EURUSD) ~ L(diff(df$EURUSD_vol),1) )
summary(reg)
plot(reg)

acf(reg$residuals)
pacf(reg$residuals)
