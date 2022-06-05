
setwd("~/GitHub/Econometrics_project")

rm(list = ls())

library("readxl")
library("lubridate")
library("ggplot2")
library("patchwork")
library("olsrr")
library("dynlm")
library("car")
library(forecast)
library(urca)
library(aTSA)
library(vars)
library(dynlm)
library(rugarch)


# Import pf the dataset


df = read_excel("dataset.xlsx")
df_1 = read.csv("MABMM301USM189S.csv")
df$M3_USA = df_1$MABMM301USM189S
df_1 = read.csv("DGS1.csv")
df$yieldUSA_1y = df_1$DGS1
df_1 = read.csv("DCOILWTICO.csv")
df_1 <- head(df_1, -1) 
df$Oil_price = df_1$DCOILWTICO
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

p1 <- ggplot(df, aes(x=Data, y=EURUSD)) + ggtitle("EURUSD") +
  geom_line() + 
  ylab("Exchange Rate") + xlab("Date")
p2 <- ggplot(df, aes(x=Data, y=M3)) + ggtitle("M3") +
  geom_line() + 
  ylab("Trillion Euros") + xlab("Date")
p3 <- ggplot(df, aes(x=Data, y=HICP)) + ggtitle("HICP") +
  geom_line() + 
  ylab("%") + xlab("Date")
p4 <- ggplot(df, aes(x=Data, y=MRO)) + ggtitle("MRO") +
  geom_line() + 
  ylab("%") + xlab("Date")
p5 <- ggplot(df, aes(x=Data, y=yieldEU_1y)) + ggtitle("yieldEU_1y") +
  geom_line() + 
  ylab("%") + xlab("Date")
p6 <- ggplot(df, aes(x=Data, y=ExtRes)) + ggtitle("ExtRes") +
  geom_line() + 
  ylab("Billion Dollars") + xlab("Date")
p7 <- ggplot(df, aes(x=Data, y=yieldUSA_1y)) + ggtitle("yieldUSA_1y") +
  geom_line() + 
  ylab("%") + xlab("Date")

p9 <- ggplot(df, aes(x=Data, y=Oil_price)) + ggtitle("Oil_price") +
  geom_line() + 
  ylab("Dollars per barrel") + xlab("Date")
p10 <- ggplot(df, aes(x=Data, y=inf_USA)) + ggtitle("inf_USA") +
  geom_line() + 
  ylab("%") + xlab("Date")


p1+p2+p3+p4+p5+p6+p7+p9+p10



################################################################################
################################################################################
#
# First Regressions on all the variables
#
################################################################################
################################################################################


# We want to see if we can find a relationship between variables and
# the differences in EUR/USD exchange rate
# Let's begin by testing for stationarity


################################################################################
# UNIT ROOT TEST
################################################################################

# First, we test whether our time series are I(1)


# Before doing so, we introduce another time series:
# Difference between interest rates in USA w.r.t. the
# ones in EU

df$yield_diff = df$yieldUSA_1y - df$yieldEU_1y


p11 <- ggplot(df, aes(x=Data, y=yield_diff)) + ggtitle("yield_diff") +
  geom_line() + 
  ylab("%") + xlab("Date")

x11()
p1+p2+p3+p4+p5+p6+p7+p9+p10+p11


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
dOil_price = diff(df$Oil_price)
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
df$dOil_price = c(NA, dOil_price)
df$dyield_diff = c(NA, dyield_diff)


# Plot of the differences

p1 <- ggplot(df, aes(x=Data, y=dEURUSD)) + ggtitle("dEURUSD") +
  geom_line() + 
  ylab("Exchange Rate") + xlab("Date")
p2 <- ggplot(df, aes(x=Data, y=dM3)) + ggtitle("dM3") +
  geom_line() + 
  ylab("Trillion Euros") + xlab("Date")
p3 <- ggplot(df, aes(x=Data, y=dHICP)) + ggtitle("dHICP") +
  geom_line() + 
  ylab("%") + xlab("Date")
p4 <- ggplot(df, aes(x=Data, y=dMRO)) + ggtitle("dMRO") +
  geom_line() + 
  ylab("%") + xlab("Date")
p5 <- ggplot(df, aes(x=Data, y=dyieldEU)) + ggtitle("dyieldEU_1y") +
  geom_line() + 
  ylab("%") + xlab("Date")
p6 <- ggplot(df, aes(x=Data, y=dExtRes)) + ggtitle("dExtRes") +
  geom_line() + 
  ylab("Billion Dollars") + xlab("Date")
p7 <- ggplot(df, aes(x=Data, y=dyieldUSA)) + ggtitle("yieldUSA_1y") +
  geom_line() + 
  ylab("%") + xlab("Date")
p9 <- ggplot(df, aes(x=Data, y=dOil_price)) + ggtitle("dOil_price") +
  geom_line() + 
  ylab("Dollars per barrel") + xlab("Date")
p10 <- ggplot(df, aes(x=Data, y=dinf_USA)) + ggtitle("dinf_USA") +
  geom_line() + 
  ylab("%") + xlab("Date")
p11 <- ggplot(df, aes(x=Data, y=dyield_diff)) + ggtitle("dyield_diff") +
  geom_line() + 
  ylab("%") + xlab("Date")


x11()
p1+p2+p3+p4+p5+p6+p7+p9+p10+p11



############### ADF TEST

# H0: non stationary
# H1: stationary


adf.test(as.matrix(df$EURUSD), nlag = 5)
adf.test(as.matrix(df$M3), nlag = 5)
adf.test(as.matrix(df$HICP), nlag = 5)
adf.test(as.matrix(df$MRO), nlag = 5)
adf.test(as.matrix(df$yieldEU_1y), nlag = 5)
adf.test(as.matrix(df$ExtRes), nlag = 5) 
adf.test(as.matrix(df$yieldUSA_1y), nlag = 5)
adf.test(as.matrix(df$M3_USA), nlag = 5)
adf.test(as.matrix(df$Oil_price), nlag = 5) 
adf.test(as.matrix(df$inf_USA), nlag = 5)
adf.test(as.matrix(df$yield_diff), nlag = 5)



# We apply the test to the first differences

adf.test(as.matrix(df$dEURUSD), nlag = 5)
adf.test(as.matrix(df$dM3), nlag = 5)
adf.test(as.matrix(df$dHICP), nlag = 5)
adf.test(as.matrix(df$dMRO), nlag = 5)
adf.test(as.matrix(df$dyieldEU), nlag = 5)
adf.test(as.matrix(df$dExtRes), nlag = 5) 
adf.test(as.matrix(df$dyieldUSA), nlag = 5)
adf.test(as.matrix(df$dM3_USA), nlag = 5)
adf.test(as.matrix(df$dOil_price), nlag = 5)
adf.test(as.matrix(df$dinf_USA), nlag = 5)
adf.test(as.matrix(df$dyield_diff), nlag = 5)

# They are all stationary => All the time series are I(1) 


###############################################################################
# Better to perform regression on I(0) variables in order to avoid spurious correlation


################################################################################
# Regression without lag -> assess correlations on the same month (not useful for prediction)


reg = lm(dEURUSD ~ dM3 + dHICP + dMRO + dinf_USA + dyield_diff 
         + dOil_price + dExtRes, data=df)
summary(reg)

# Wald Test to assess if the dM3 and dMRO coefficients are together zero or not 
linearHypothesis(reg, rbind(c(0,1,0,0,0,0,0,0), c(0,0,0,1,0,0,0,0)), c(0,0))


reg = lm(dEURUSD ~ dHICP + dinf_USA + dyield_diff 
         + dOil_price + dExtRes, data=df)
summary(reg)

x11()
plot(df$Data, c(0, dEURUSD), type = "l", ylab = "", xlab = "Date")
lines(df$Data, c(0, fitted(reg)), col = "red")
legend(x = "topright", legend=c("dEURUSD", "Fitted Values"),
       col=c("black", "red"), lty = c(1, 1), cex=1.3)
################################################################################
# Now we want a predictive model, so we consider lagged variables

# Try regression with all our I(0) time series

reg = dynlm(dEURUSD ~ L(ts(dM3),1) + L(ts(dHICP),1) + L(ts(dMRO),1) + L(ts(dinf_USA),1) 
                    + L(ts(dyield_diff),1) + L(ts(dOil_price),1) + L(ts(dExtRes), 1), data=df)
summary(reg)


plot(reg)

acf(reg$residuals)
pacf(reg$residuals)
shapiro.test(reg$residuals)


# Test H0: beta2 = 0 e beta4 = 0 vs H1: at least one of beta2 and beta4 != 0 (dM3 e dMRO)

linearHypothesis(reg, rbind(c(0,1,0,0,0,0,0,0), c(0,0,0,1,0,0,0,0)), c(0,0))

# p-value: 0.6714 => we remove dM3 and dMRO


reg = dynlm(ts(dEURUSD) ~ L(ts(dHICP), 1) + L(ts(dinf_USA),1) + L(ts(dyield_diff),1) 
                    + L(ts(dOil_price),1) + L(ts(dExtRes), 1), data=df)
summary(reg)


x11()
plot(df$Data, c(0,dEURUSD), type = "l", ylab = "", xlab = "Date")
lines(df$Data, c(0, 0, reg$fitted.values), col = "red")
legend(x = "topright", legend=c("dEURUSD", "Fitted Values"),
       col=c("black", "red"), lty = c(1, 1), cex=1.3)

acf(reg$residuals)
pacf(reg$residuals)
# acf and pacf do not evidence problematic behaviour

shapiro.test(reg$residuals)

################################################################################
################################################################################
#
# SECOND MODEL: VECM
#
################################################################################
################################################################################


# We want to find a long term relationship between exchange rate EUR/USD and our
# time series.
# Let's see if this approach yields a better fit

################################################################################

# we consider the difference between EU and USD interest rates instead of the two rates

y.VAR.IC <- VARselect(df[c("EURUSD", "M3", "yield_diff", "HICP", 
                           "ExtRes", "Oil_price", "inf_USA")], type="const")
nlags <- y.VAR.IC$selection
nlags

#Dice di usare due lag


y.CA <- ca.jo(df[c("EURUSD", "M3", "yield_diff", "HICP", 
                   "ExtRes", "Oil_price", "inf_USA")], 
              type="trace", ecdet = "const", spec="longrun", K=2)

summary(y.CA)

# There is only one cointegration relationship

vecm<-cajorls(y.CA, r = 1)

# Let's see the cointegration relationship
vecm

summary(vecm$rlm)

x11()
plot(x=df$Data, y=df$EURUSD, type='l', xlab = "Date", ylab = "Exchange Rate")
lines(x=df$Data[3:210], y=df$EURUSD[2]+cumsum(vecm$rlm$fitted.values[,1]), col='red')
legend(x = "topright", legend=c("EURUSD", "Fitted Values"),
       col=c("black", "red"), lty = c(1, 1), cex=1.3)

res = vecm$rlm$residuals[,1]

x11()
plot(df$Data[3:210], res^2, xlab="Date", ylab=" ", main="Squared VECM Residuals", type='l')


shapiro.test(res) #p-value 0.006957 reject Normality assumption
library(moments)

kurtosis(res) #4.26 => leptokurtic

x11()
plot(x=df$Data, y=df$EURUSD, type='l', xlab = "Date", ylab = "Exchange Rate")
lines(x=df$Data[3:210], y=df$EURUSD[2]+cumsum(vecm$rlm$fitted.values[,1]), col='red')
legend(x = "topright", legend=c("EURUSD", "Fitted Values"),
       col=c("black", "red"), lty = c(1, 1), cex=1.3)


res = vecm$rlm$residuals[,1]


x11()
plot(df$Data[1:208], res, type='l', xlab = "Date", ylab = "")
title(main="residui")

acf(res^2)
pacf(res^2)

acf(res)
pacf(res)

x11()
hist(res, breaks = 40, xlab = "Residual Values", col = "lightblue")
title(main = "")


#R^2 increase a lot from 20% to 35% -> considering the long run reletionship 
#proves to be beneficial



df$EURUSD_vol_cointegration = c(NA,NA, res^2)


################################################################################
################################################################################
#
# THIRD PART: VOLATILITY ANALYSIS
#
################################################################################
################################################################################

#We model volatility with 3 approaches:
# - volatility of the residuals of our cointegration model
# - we model the interest rate returns directly with an ARIMA-GARCH model
# - monthly volatility: variance of the EURUSD exchange rate in a given month

################################################################################




# Creation of dummy variable

dummy = as.numeric(as.logical(diff(df$MRO)))
dummy = c(NA, dummy)
plot(df$MRO, type = "l")
lines(dummy, col = "green")


df$ECB_MROaction = as.data.frame(dummy)



################################################################################
# Test for ARCH effect (LM test)

library(FinTS)
ArchTest(res, lag = 2, demean = TRUE) # usiamo due lag
x11()
plot(df$Data[3:210],res^2, type='l', xlab="date", ylab=" ", main="squared VECM residuals")

#########################################################
# Let's if ecb action influence the magnitude of the VECM residuals 

spec = ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1,1), external.regressors = as.matrix(df[2:209, c("dExtRes", "dMRO", "dM3")])), 
                  distribution.model="norm", mean.model=list(armaOrder=c(0,0), include.mean = FALSE))

fit = ugarchfit(spec=spec, data=res)
fit

fit@fit$coef

#No significance of the regressor -> final model for the conditional variance
spec = ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1,1)), 
                  distribution.model="norm", mean.model=list(armaOrder=c(0,0), include.mean = FALSE))

fit = ugarchfit(spec=spec, data=res)
fit
x11()
plot(df$Data[3:210], res^2, type="l", xlab="Date", ylab=" ", main="Square Residuals and Fitted Variance of GARCH(1,1) Model")
lines(df$Data[3:210], fit@fit$var, col='red')
res_std = res/sqrt(fit@fit$var)
hist(res_std)
shapiro.test(res_std)
x11()
plot(df$Data[3:210], res_std, xlab="Date", ylab = " ", main="Standardized Residuals with Conditional Standard Deviation")
abline(h=0, col="red")
abline(h=qnorm(0.975), col="blue")
abline(h=-qnorm(0.975), col="blue")


####################################################################
#Second Approach: EURUSD returns via ARIMA-GARCH model


EURUSD = ts(df$EURUSD, frequency = 12, start=c(2004,10), end=c(2022,3))
plot(diff(log(EURUSD)), type = "l")

x11()
plot(acf(diff(log(df$EURUSD))), main="ACF Returns", xlab="Lag", ylab=" ")

x11()
plot(pacf(diff(log(df$EURUSD))), main="PACF Returns", xlab="Lag", ylab=" ")

mod = auto.arima(log(EURUSD), d=1) #model selected is arima (1,1,0)
mod
summary(lm(diff(log(EURUSD)) ~ c(tail(diff(log(EURUSD)),-1),0)))#intercept not significant -> take it out of GARCH

ecb_decisions = cbind(diff(df$M3), diff(df$MRO), diff(df$ExtRes))
ecb_decisions.l1 = cbind(diff(df$M3)[1:208], diff(df$MRO)[1:208], diff(df$ExtRes)[1:208])
ecb_decisions.l2 = cbind(c(tail(diff(df$M3), -2),0,0), c(tail(diff(df$MRO), -2), 0, 0), c(tail(diff(df$ExtRes), -2), 0, 0))
dummy = as.numeric(as.logical(diff(df$MRO)))


spec = ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1,1), external.regressors = as.matrix(ecb_decisions[,1])), 
                  distribution.model="norm", mean.model=list(armaOrder=c(1,0), include.mean = FALSE))

fit = ugarchfit(spec=spec, data=diff(log(EURUSD)))
fit

spec = ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1,1), external.regressors = as.matrix(dummy)), 
                  distribution.model="norm", mean.model=list(armaOrder=c(1,0), include.mean = FALSE))

fit = ugarchfit(spec=spec, data=diff(log(EURUSD)))
fit

1 - var(fit@fit$residuals)/var(diff(log(EURUSD)))# R^2 adjusted

x11()
plot(df$Data[2:210], fit@fit$var, type='l', xlab='date', ylab=' ', main="Conditional Variance")
abline(v=df$Data[as.logical(dummy)], col='red')

########################################################################
# Third Approach: Modelleing empirical monthly volatility with ARMA model
library(lmtest)
#una semplice regressione mostra che differenze nel tasso MRO sono correlate con aumento di volatilità 
reg = dynlm( EURUSD_vol[2:210] ~ diff(MRO) + diff(ExtRes) + diff(M3), data=df)
summary(reg)
plot(reg)

acf(reg$residuals)
pacf(reg$residuals)

#lot of auto correlation -> use ARMA
df$dummy_MRO = c(NA,as.numeric(as.logical(diff(df$MRO))))

plot(df$EURUSD_vol, type='l')
acf(df$EURUSD_vol)
pacf(df$EURUSD_vol)

mod0 = auto.arima(df$EURUSD_vol[2:length(df$EURUSD_vol)], d=0)
mod0
1-mod0$sigma2/var(df$EURUSD_vol) #R^2

#external regressor MRO
mod1 = auto.arima(df$EURUSD_vol[2:length(df$EURUSD_vol)], xreg = as.matrix(diff(df$MRO)), d=0)
mod1
1-mod1$sigma2/var(df$EURUSD_vol) #R^2
#external regressor dummy MRO
mod2 = auto.arima(df$EURUSD_vol[2:length(df$EURUSD_vol)], xreg = as.matrix(df[2:210,c("dummy_MRO")]), d=0)
mod2
1-mod2$sigma2/var(df$EURUSD_vol) #R^2

coeftest(mod2)
acf(mod2$residuals)
pacf(mod2$residuals)

# What happens if we take into account VIX index?

library(tidyverse)
vix = read.csv("VIX_History.csv")
vix$DATE = as.Date(vix$DATE, "%m/%d/%Y")
vix_monthly = vix %>% 
  mutate(month = floor_date(DATE, "month")) %>%
  group_by(month) %>%
  summarize(avg = mean(CLOSE))
vix_monthly = vix_monthly%>%filter(between(month, as.Date("2004-10-1"), as.Date("2022-3-1")))

df$vix = vix_monthly$avg
x11()
plot(df$Data, (df$EURUSD_vol-mean(df$EURUSD_vol))/sd(df$EURUSD_vol), xlab="Date", ylab="Standardized Volatility", type="l")
lines(df$Data, (df$vix-mean(df$vix))/sd(df$vix), col="red")
lines(df$Data, df$MRO, col="green")
legend(x="topright", col=c("black", "red", "green"), lty=c(1,1,1), cex=1 ,legend=c("EURUSD monthly volatility (std)", "VIX (std)", "MRO"))



reg = dynlm( EURUSD_vol[2:210] ~ vix[2:210] + diff(MRO) + diff(ExtRes) + diff(M3), data=df)
summary(reg)

acf(reg$residuals)
pacf(reg$residuals)
# a simple regression shows that VIX and MRO are both significant
# Let's build a more complex ARMA model 
k=1
reg = dynlm( EURUSD_vol[k:210] ~ vix[k:210] + dummy_MRO[k:210] , data=df)
summary(reg)


x11()
acf(reg$residuals)
x11()
plot(pacf(reg$residuals), main="PACF residuals", ylab=" ", xlab = "Lag" )

k=2
mod3 = Arima(df$EURUSD_vol[k:210], xreg= as.matrix(df[k:210, c( "vix", "dummy_MRO")]), order=c(5,0,0))
#mod3 = auto.arima(df$EURUSD_vol[k:210], xreg = as.matrix(df[k:210, c( "vix", "dMRO")]))
mod3
coeftest(mod3)
1 - mod3$sigma2/var(df$EURUSD_vol)
acf(mod3$residuals)
pacf(mod3$residuals)
plot(mod3)
