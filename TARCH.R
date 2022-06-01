
rm(list = ls())

library("readxl")
library("lubridate")
library("ggplot2")
library("patchwork")
library("olsrr")

# Import the dataset

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

p10
p1+p2+p3+p4+p5+p6+p7+p8+p9+p10


plot(df$inf_USA, type = "l")

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


# Per i processi non stazionari consideriamo le time series delle differenze

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
yield_diff = df$yieldUSA_1y - df$yieldEU_1y

# Inserisco le differenze nel dataset

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

df$yield_diff = yield_diff
df$dyield_diff = c(NA, diff(df$yield_diff))


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
p5 <- ggplot(df, aes(x=Data, y=dyieldEU)) +
  geom_line() + 
  xlab("")
p6 <- ggplot(df, aes(x=Data, y=dExtRes)) +
  geom_line() + 
  xlab("")
#p7 <- ggplot(df, aes(x=Data, y=dRefOp)) +
#  geom_line() + 
#  xlab("")
p8 <- ggplot(df, aes(x=Data, y=EURUSD_vol)) +
  geom_line() + 
  xlab("")

p9 <- ggplot(df, aes(x=Data, y=yield_diff)) + 
  geom_line() +
  xlab("")

p10 <- ggplot(df, aes(x=Data, y=dyield_diff)) + 
  geom_line() +
  xlab("")

p11 <- ggplot(df, aes(x=Data, y=dinf_USA)) + 
  geom_line() +
  xlab("")


p1+p2+p3+p4+p5+p6+p8+p9+p10+p11


# Calcolo l'ERS test

summary(ur.ers(df$dEURUSD, type="P-test", model="trend"))
summary(ur.ers(df$dM3, type="P-test", model="trend")) 
summary(ur.ers(df$dHICP, type="P-test", model="trend")) 
summary(ur.ers(df$dMRO, type="P-test", model="trend")) 
summary(ur.ers(df$dyieldEU, type="P-test", model="trend")) 
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
adf.test(as.matrix(df$M3_USA))
adf.test(as.matrix(df$yieldUSA_1y))
adf.test(as.matrix(df$Petrol_USA))

# PROBLEMA: ExtRes, RefOp e EURUSD_vol sono stazionari: come possiamo imporre la cointegrazione?
# SECONDO PROBLEMA: Stazionarietà dovrebbe implicare std costante, ma non mi sembra


# Applichiamo il test alle first differences

adf.test(as.matrix(df$dEURUSD))
adf.test(as.matrix(df$dM3))
adf.test(as.matrix(df$dHICP))
adf.test(as.matrix(df$dMRO))
adf.test(as.matrix(df$dyieldEU))
adf.test(as.matrix(df$dExtRes))
adf.test(as.matrix(df$dM3_USA)) 
adf.test(as.matrix(df$dyieldUSA)) 

# Vengono tutti stazionari, ovviamente

###############################################################################
#Siccome tutte le variabili sono I(1) la regressione sulle variabili originarie
#non ha senso a meno che non siano cointegrate (relazioni spurie) -> regressione
#serie differenziate


reg = dynlm(dEURUSD ~ L(dM3,1) + L(dHICP,1) + L(dMRO,1) + L(dinf_USA,1) + L(dyield_diff,1) + L(dPetrol_USA,1), data=df)
summary(reg)

acf(reg$residuals)
pacf(reg$residuals)
shapiro.test(reg$residuals)
#la regressione non è malaccio, R^2 del 28%, i residui sono normali, e non sembra esserci autocorrelazione quindi c'è consistenza
# MRO e M3 non viene significativo quindi li scartiamo

reg = dynlm(dEURUSD ~ L(dHICP,1) + L(dinf_USA,1) + L(dyield_diff,1) + L(dPetrol_USA,1), data=df)
summary(reg)

acf(reg$residuals)
pacf(reg$residuals)
shapiro.test(reg$residuals)

################################################################################
#Analizziamo ora le correlazioni di lungo termine

library(vars)

y.VAR.IC <- VARselect(df[c("EURUSD", "M3", "yieldEU_1y", "HICP", "ExtRes", "yieldUSA_1y", "Petrol_USA", "inf_USA")], type="const")
nlags <- y.VAR.IC$selection
nlags

#Dice di usare due lag


y.CA <- ca.jo(df[c("EURUSD", "M3", "yieldEU_1y", "HICP", "ExtRes", "yieldUSA_1y", "Petrol_USA", "inf_USA")], type="trace", ecdet = "const", spec="longrun", K=2)
summary(y.CA)

vecm<-cajorls(y.CA, r = 2)
vecm


summary(vecm$rlm)


res = vecm$rlm$residuals[,1]
res

plot(res^2, type = "l")

plot(res, type = "l")

################consideriamo la differenza dei tassi tra europa e america

y.VAR.IC <- VARselect(df[c("EURUSD", "M3", "yield_diff", "HICP", "ExtRes", "Petrol_USA", "inf_USA")], type="const")
nlags <- y.VAR.IC$selection
nlags

#Dice di usare due lag


y.CA <- ca.jo(df[c("EURUSD", "M3", "yield_diff", "HICP", "ExtRes", "Petrol_USA", "inf_USA")], type="trace", ecdet = "const", spec="longrun", K=2)
summary(y.CA)

vecm<-cajorls(y.CA, r = 1)
vecm


summary(vecm$rlm)


res = vecm$rlm$residuals[,1]


plot(res^2, type='l')

shapiro.test(res)
acf(res^2)
pacf(res^2)

# le relazioni di cointegrazione sono 1 ed è super significativa per spiegare andamento tasso di cambio
# le relazioni di short term significative sono invece riguardanti external reserve, prezzo petrolio, è inflazione usa
# R^2 è uguale a 28.75% ma è paragonabile ai risultati di semplice regressione
# inflazione EU invece non sembra particolarmente significativa

#i residui di questo modello sono ben centrati, no autocorrelazione etc.
#usiamo la volatilità di questi residui per studiare la volatilità del tasso di cambio
#è la nostra alternativa alla semplice volatilità del sliding windows

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
# Modelliamo la volatilità con 3 approcci:
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



library(dynlm)


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
# approccio 1: vediamo se a decisioni della banca centrale 
# per abbassare o alzare tassi corrisponde volatilità maggiore
# nel nostro modello di cointegrazione
# Interpretazione: abbiamo un modello che spiega abbastanza bene il cambio 
# ----> vediamo se decisioni di politica monetaria danno scossoni non previsti dal modello
# modelliamo i residui come se fossere una serie temporale a media nulla
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
#approccio 2: MODELLIAMO TUTTO TRAMITE ARIMA E GARCH

library(forecast)
EURUSD = ts(df$EURUSD, frequency = 12, start=c(2004,10), end=c(2022,3))
plot(diff(log(EURUSD)), type = "l")
acf(diff(EURUSD))
pacf(diff(EURUSD))
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
# approccio 3: Modelliamo direttamente la volatilità mensile tramite modelli ARMA

plot(df$EURUSD_vol, type='l')
acf(df$EURUSD_vol)
pacf(df$EURUSD_vol)
mod0 = auto.arima(df$EURUSD_vol[2:length(df$EURUSD_vol)])
mod0
1-mod0$sigma2/var(df$EURUSD_vol)
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


reg = dynlm(diff(EURUSD) ~ L(diff(EURUSD_vol),1) )
summary(reg)
plot(reg)

acf(reg$residuals)
pacf(reg$residuals)
