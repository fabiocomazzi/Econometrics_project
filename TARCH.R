
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
dyield = diff(df$yieldEU_1y)
dExtRes = diff(df$ExtRes)
dM3_USA = diff(df$M3_USA)
dyieldUSA = diff(df$yieldUSA_1y)

# Inserisco le differenze nel dataset

df$dEURUSD = c(0, dEURUSD)
df$dM3 = c(0, dM3)
df$dHICP = c(0, dHICP)
df$dMRO = c(0, dMRO)
df$dyield = c(0, dyield)
df$dExtRes = c(0, dExtRes)
df$dM3_USA = c(0, dM3_USA)
df$dyieldUSA = c(0, dyieldUSA)


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
adf.test(as.matrix(df$dyield))
adf.test(as.matrix(df$dExtRes))
adf.test(as.matrix(df$dM3_USA)) 
adf.test(as.matrix(df$dyieldUSA)) 

# Vengono tutti stazionari, ovviamente








################################################################################

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
# TGARCH
################################################################################




# Creation of dummy variable
dummy = c(0)
for(i in 2:nrow(df)) 
{
  if (df$MRO[i]-df$MRO[i-1]) {
  d = 1 
  } else {
  d = 0
  }
  dummy = c(dummy,d)
}


plot(df$MRO, type = "l")
lines(dummy, col = "green")


df$ECB_action = as.data.frame(dummy)



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




############################### APPLICHIAMO IL MODELLO

library(rugarch)

spec = ugarchspec(variance.model=list(model = "eGARCH", garchOrder = c(0,1), external.regressors = as.matrix(dummy)), 
                  distribution.model="std", mean.model=list(armaOrder=c(0,0), include.mean = TRUE))

fit = ugarchfit(spec=spec, data=res)
fit


fit@fit$coef

fit@fit$fitted.values

mean(res)


#tGARCH

garchMod <- ugarchspec(variance.model=list(model="fGARCH",
                                           garchOrder=c(2,2),
                                           submodel="TGARCH",
                                           external.regressors = as.matrix(dummy)),
                       mean.model=list(armaOrder=c(0,0)), 
                       distribution.model="std")
garchFit <- ugarchfit(spec=garchMod, data=res_adj)
coef(garchFit)

garchFit


############### Altra libreria

library(fGarch)
arch.fit <- garchFit(~garch(1,1), data = res_adj)
summary(arch.fit)

plot(arch.fit@fitted, type = "l")

