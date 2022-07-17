# Chapter 1 ====================================================================

## Preparation =================================================================
library(data.table)
library(tseries)
library(forecast)
library(modelr)
## textbook content ============================================================

value_weighted_index.monthly_simple_returns <- fread("chapter 2/m-vw2697.txt")

value_weighted_index.monthly_simple_returns[,plot(.I, V1, type='l')]
plot.ts(value_weighted_index.monthly_simple_returns, main='V1')


prod(value_weighted_index.monthly_simple_returns + 1) - 1

acf(value_weighted_index.monthly_simple_returns)
pacf(value_weighted_index.monthly_simple_returns)
TSA:eacf(value_weighted_index.monthly_simple_returns)

model_ar_3 <- tseries::arma(x = value_weighted_index.monthly_simple_returns,
                            order = c(3, 0), include.intercept = TRUE)

summary(model_ar_3)

value_weighted_index.monthly_simple_returns[,exp(12 * mean(log(V1 + 1))) - 1]

acf(model_ar_3 $ residuals, na.action = na.exclude)

value_weighted_index.monthly_simple_returns[, Box.test(V1,
                                                    lag = log(.N),
                                                     type = "Ljung-Box")]

Box.test(model_ar_3 $ residuals, lag = 12, type='Ljung-Box', fitdf = 2)

# p-value is 0.04972, thus the null hypothesis of no residual serial correlation
# in the first 12 lags is barely not rejected at the 5% level

# However, sice the lag-2 AR coefficient is not significant at the 5% level,
# one can refine the model as

model_ar_3.without_lag_2 <- tseries::arma(x = value_weighted_index.monthly_simple_returns,
              lag = list(ar=c(1,3)),
              include.intercept = TRUE)
Box.test(model_ar_3.without_lag_2 $ residuals,
         lag = 12,
         type='Ljung-Box',
         fitdf = 3)

# Unit root

n <- 10000
random.walk <- cumsum(sample(c(-1, 1), n, TRUE))

plot(random.walk, type='l')
acf(random.walk)

# Regression models withs time series errors
#tcmr = treasury constant maturity rate
tcmr.1year <-  fread('chapter 2/m-gs1.txt')
tcmr.3year <-  fread('chapter 2/m-gs3.txt')

tcmr <- cbind(r1=tcmr.1year, r3=tcmr.3year)[,
  .(date=lubridate::make_date(r1.V1, r1.V2, r1.V3),
    r1=r1.V4, r3=r3.V4)]

tcmr[,plot(date, r1,
           type='l',
           main = 'treasury constant maturity rate',
           ylab='')]
tcmr[,lines(date, r3, lty='dashed')]

tcmr.lm <- lm(r3~r1, data=tcmr)
summary(tcmr.lm)

tcmr.residuals <- residuals(tcmr.lm)

Box.test(tcmr.residuals)
acf(tcmr.residuals, demean=FALSE)

tcmr.diff <- tcmr[,.(date=date[-1], r1=diff(r1), r3=diff(r3))]

tcmr.diff[,plot(date, r1,
           type='l',
           main = 'treasury constant maturity rate',
           ylab='')]
tcmr.diff[,lines(date, r3, lty='dashed')]


tcmr.diff.lm <- lm(r3~r1, data=tcmr.diff)
summary(tcmr.diff.lm)
tcmr.diff.residuals <- residuals(tcmr.diff.lm)
Box.test(tcmr.diff.residuals)
acf(tcmr.diff.residuals)
pacf(tcmr.diff.residuals)

tcmr.diff.residuals.ma <- tseries::arma(tcmr.diff.residuals, 
                                        lag = list(ar=NULL,ma=c(1,2)))
summary(tcmr.diff.residuals.ma)

tcmr.diff.residuals.ma.residuals <- residuals(tcmr.diff.residuals.ma)

acf(tcmr.diff.residuals.ma.residuals, na.action = na.exclude)
Box.test(tcmr.diff.residuals.ma.residuals)

## questions ===================================================================

### 2.1 ========================================================================
# In markdown

### 2.2 ========================================================================
# In markdown

### 2.3 ========================================================================

Ljung.test <- function(x, fitdf=0) {
  Box.test(x, type='Ljung-Box', lag=log(length(x)), fitdf = fitdf)
}

unemhelp <- fread("chapter 2/m-unemhelp.txt",
                  header = FALSE,
                  col.names = c('unemployment_rate',
                                'help_wanted_ads',
                                'year',
                                'month'))

unemployment_rate <- unemhelp[,.(date=lubridate::make_date(year, month),
                                 value=unemployment_rate)]

setkey(unemployment_rate, 'date')

unemployment_rate[,plot(date, value, type='l', main='unemployment_rate')]
unemployment_rate[,acf(value)]
unemployment_rate[,pacf(value)]

# long-memory
unemployment_rate[,adf.test(value)]
# p-value = 0.06468 for H_0 = have unit-root 

unemployment_rate.diff = unemployment_rate[,diff(value)]
plot(unemployment_rate.diff, type='l')
# the time plot show heteroscedasticity

Ljung.test(unemployment_rate.diff)
# p-value < 2.2e^-16, by far has serial correlation

acf(unemployment_rate.diff)
pacf(unemployment_rate.diff)
# both have no cut

TSA::eacf(unemployment_rate.diff)
# (1,2) (2,2) (1,3) (2,3)

unemployment_rate.diff.get_AIC <- function (ar, ma) {
  model <- arma(unemployment_rate.diff, order=c(ar,ma))
  summary(model) $ aic
}

unemployment_rate.diff.get_AIC(1,2)
#-322.4137
unemployment_rate.diff.get_AIC(2,2)
#-320.4182
unemployment_rate.diff.get_AIC(1,3)
#-320.3858
unemployment_rate.diff.get_AIC(2,3)
#-319.2205

# Go to (1,2), the minimum AIC

unemployment_rate.diff.arma <- arma(unemployment_rate.diff, order=c(1,2))

summary(unemployment_rate.diff.arma)
# all coefficients are significant at any reasonable significance level

unemployment_rate.diff.ar.residuals <- residuals(unemployment_rate.diff.arma)
Ljung.test(unemployment_rate.diff.ar.residuals, fitdf = 2)
# p-value = 0.6598, by far we can consider no serial correlation

unemployment_rate[,max(date)]
# r_{h+1} -> March of 2004 
# r_{h+2} -> April of 2004 
# r_{h+3} -> May of 2004 

# to forecast, we need use stats:arima (we has used tseries:arma)
model <- stats::arima(unemployment_rate $ value, order=c(1,1,2))
forecast(model, 3)
# Forecast to March 2004 = 5.54119
# Forecast to April 2004 = 5.5072
# Forecast to May 2004 = 5.4807

### 2.4 ========================================================================
DT <- fread("chapter 2/m-decile1510.txt",
                  header = FALSE,
                  col.names = c('DATE',
                                'DECILE1',
                                'DECILE5',
                                'DECILE10'))
DT[,DATE:=lubridate::ymd(DATE)]

DT[,Box.test(DECILE1, lag=12, type="Ljung-Box")]
# p-value = 7.045e-12 by far has serial correlation
# only biggest companies

DT[,Box.test(DECILE5, lag=12, type="Ljung-Box")]
# p-value = 0.0002005 by \alpha = 0.001 has serial correlation

DT[,Box.test(DECILE10, lag=12, type="Ljung-Box")]
# p-value = 0.8564, we can consider that have no serial correlation

DT[,acf(DECILE5)]
# for AR(p), p = 1

DECILE5.ar <- DT[,arma(DECILE5, order=c(1,0))]

summary(DECILE5.ar)
# all coeficients are signficant

Box.test(residuals(DECILE5.ar),
         lag=12, type="Ljung-Box")
# p-value 0.1275 (no serial correlation)

DT[,pacf(DECILE5)]

DECILE5.ma <- DT[,arma(DECILE5, lag=list(
  ar=NULL,
  ma=c(8)
))]

summary(DECILE5.ma)
# all coeficients are signficant

Box.test(residuals(DECILE5.ma),
         lag=12, type="Ljung-Box")

predict(DECILE5.ar, n.ahead=1)
forecast::is.Arima(DECILE5.ar)


model <- stats::arima(DT $ DECILE5, order=c(1,0,0),
                      fixed=c(
                        DECILE5.ar $ coef [["ar1"]],
                        DECILE5.ar $ coef [["intercept"]]
                      ))

forecast(model, 3)

model <- stats::arima(DT $ DECILE5, order=c(0,0,8),
                      fixed=c(
                        rep(0, 7),
                        DECILE5.ma $ coef [["ma8"]],
                        DECILE5.ma $ coef [["intercept"]]
                      ))

forecast(model, 3)

# 2.5 ==========================================================================
DT <- fread("chapter 2/d-ibmvwewsp6202.txt",
            header = FALSE,
            col.names = c('DATE', 'IBM', 'VW', 'EW', 'SP'))

DT[,.(IBM=suppressWarnings(as.double(IBM)))][
  ,.(IBM=zoo::na.locf(IBM, na.rm=FALSE))][
    ,acf(IBM, lag.max = 100)]

# no long-range dependence, because have no significant ACF to l > 0

# 2.6 ==========================================================================
demand <- fread("chapter 2/power6.txt") $ V1

plot.ts(demand)

acf(demand)
pacf(demand)
# annual seasonality

demand.diff12 <- diff(demand, lag=12)
plot.ts(demand.diff12)

acf(demand.diff12)
# exponential decay start at lag = 2; ma=1

pacf(demand.diff12)
# big at 1, 11, 12 and 13 

demand.diff12.model <- arma(demand.diff12, lag = list(
  ar=c(1, 11, 12, 13),
  ma=c(1)
))

summary(demand.diff12.model)
# ar11 is not significant

demand.diff12.model <- arma(demand.diff12, lag = list(
  ar=c(1, 12, 13),
  ma=c(1)
))

summary(demand.diff12.model)

plot.ts(demand.diff12.model $ residuals)

acf(demand.diff12.model $ residuals, na.action = na.remove)
Box.test(demand.diff12.model $ residuals, lag = 5)
# p-value = 0.6405 (because 5 is near to log(N))

cor(
  demand.diff12,
  demand.diff12.model $ fitted.values,
  use="pairwise.complete.obs"
)
# R**2 = 0.7895, sooo good :)

plot(demand.diff12, type='l')
lines(demand.diff12.model $ fitted.values, col='red', lty='dashed')

demand.diff12.forecast.model <- stats::arima(
  demand.diff12,
  order = c(3,0,1),
  fixed = demand.diff12.model $ coef
)
demand.diff12.predicted_value <- forecast(demand.diff12.forecast.model, 24)

demand.diff12.predicted_value <- diffinv(demand.diff12.predicted_value $ x,
                                         lag=12, xi=tail(demand, 12))

# 2.7 ==========================================================================
crsp <- fread("chapter 2/d-ew8099.txt", col.names = c('DATE',
                                                      'INDEX'))
crsp[,DATE:=lubridate::ymd(DATE)]

crsp[,plot.ts(INDEX)]
lubridate::wday()
crsp[,WEEKDAY:=lubridate::wday(DATE, label = TRUE, locale = 'en_US.UTF-8')]
crsp[,WEEKDAY:=as.character(WEEKDAY)]

crsp.model <- lm(INDEX ~ WEEKDAY, crsp)

coefs <- crsp.model $ coefficients
coefs.intercept <- coefs[[1]]
coefs <- coefs[-1] + coefs.intercept
coefs <- append(coefs, 0)
names(coefs) <- c("Mon", "Tue", 'Wed', 'Thu', "Fri")
barplot(coefs)
# Mondays are worst days
# Friday are best days

summary(crsp.model)

# significant in 5% level

crsp.model.vcovHAC <- sandwich::vcovHAC(crsp.model)

crsp.test_with_hac <- data.table(
  coef = crsp.model $ coefficients,
  var = diag(crsp.model.vcovHAC),
)

crsp.test_with_hac[,sd:=sqrt(var)]
crsp.test_with_hac[,t:=coef/sd]
crsp.test_with_hac[,pvalue:=(2 * pt(-abs(t), crsp[,.N] - 2))]
# ever pvalue is less that 0.0004

crsp.residuals <-  crsp.model $ residuals
names(crsp.residuals) <- c()

plot.ts(crsp.residuals)

acf(crsp.residuals)
pacf(crsp.residuals)

crsp.residuals.model <- arma(crsp.residuals)

Box.test(crsp.residuals.model $ residuals, lag = 5)
# p-value = 1.221e-05

plot.ts(crsp.residuals.model $ residuals)

# 2.8 ==========================================================================
return_values <- fread('chapter 2/d-dell3dx0003.txt',
                 col.names = c("DATE",
                               "DELL",
                               "VW",
                               "EW",
                               "SP"))

sp_return <- return_values[,.(
  DATE=lubridate::ymd(DATE),
  SP
)][,.(
  DATE,
  INDEX=SP,
  WEEKDAY=lubridate::wday(DATE, label = TRUE, locale = 'en_US.UTF-8')
)][,.(
  DATE, INDEX,WEEKDAY,
  IS_FRIDAY = WEEKDAY=='Fri'
)]

sp_return.model <- lm(INDEX ~ IS_FRIDAY, sp_return)

summary(sp_return.model)
# p-value = 0.244 (no significant)

sp_return.vcovHAC <- sandwich::vcovHAC(sp_return.model)

sp_return.test_with_hac <- data.table(
  coef = sp_return.model $ coefficients,
  var = diag(sp_return.vcovHAC)
)

sp_return.test_with_hac[,sd:=sqrt(var)]
sp_return.test_with_hac[,t:=coef/sd]
sp_return.test_with_hac[,pvalue:=(2 * pt(-abs(t), crsp[,.N] - 2))]
# p-value = 0.2387 (no significant)

Box.test(sp_return.model $ residuals, lag = 12, type = "Ljung-Box")
# p-value = 0.2779 (no significant serial correlation)

# 2.9 ==========================================================================
return_values <- fread('chapter 2/d-dell3dx0003.txt',
                       col.names = c("DATE",
                                     "DELL",
                                     "VW",
                                     "EW",
                                     "SP"))

dell_return <- return_values[,.(
  DATE=lubridate::ymd(DATE),
  DELL
)][,.(
  DATE,
  INDEX=DELL,
  WEEKDAY=lubridate::wday(DATE, label = TRUE, locale = 'en_US.UTF-8')
)][,.(
  DATE, INDEX,WEEKDAY,
  IS_FRIDAY = WEEKDAY=='Fri'
)]

dell_return.model <- lm(INDEX ~ IS_FRIDAY, sp_return)

summary(dell_return.model)
# p-value = 0.251 (no significant)

dell_return.vcovHAC <- sandwich::vcovHAC(dell_return.model)

dell_return.test_with_hac <- data.table(
  coef = dell_return.model $ coefficients,
  var = diag(dell_return.vcovHAC)
)

dell_return.test_with_hac[,sd:=sqrt(var)]
dell_return.test_with_hac[,t:=coef/sd]
dell_return.test_with_hac[,pvalue:=(2 * pt(-abs(t), crsp[,.N] - 2))]
dell_return.test_with_hac
# p-value = 0.2785118 (no significant)

Box.test(dell_return.model $ residuals, lag = 12, type = "Ljung-Box")
# p-value = 0.02193 (significant serial correlation)

plot.ts(dell_return $ INDEX)

acf(dell_return $ INDEX)
# no cut off
# 2, 13

pacf(dell_return $ INDEX)
# 1 and 13

TSA::eacf(dell_return $ INDEX)
# (2,0) 

dell_return.arma <- arma(dell_return $ INDEX, lag = list(
  ar = c(2, 13),
  ma = c(1, 13)
))

summary(dell_return.arma) 
# only ar2 coefficient are significant, AIC = -3938.32

dell_return.arma <- arma(dell_return $ INDEX, lag = list(
  ar = c(2),
  ma = c(1)
))

summary(dell_return.arma)
# only ar2 coefficient are significant, AIC = -3926.05

dell_return.arma <- arma(dell_return $ INDEX, lag = list(
  ar = c(2)
))

summary(dell_return.arma)
# only ar2 coefficient are significant, AIC = -3927.94

acf(dell_return.arma $ residuals, na.action = na.remove)
pacf(dell_return.arma $ residuals, na.action = na.remove)

dell_return.arma <- arma(dell_return $ INDEX, lag = list(
  ar = c(2)
), include.intercept = FALSE)

acf(dell_return.arma $ residuals, na.action = na.remove)
pacf(dell_return.arma $ residuals, na.action = na.remove)

summary(dell_return.arma)
# all coefs are significant, AIC = -3929.94

Box.test(dell_return.arma $ residuals, lag = 12, type = "Ljung-Box")
# p-value = 0.6835

dell_return.arma.residuals <-  data.table(
  residual = dell_return.arma $ residuals,
  IS_FRIDAY = dell_return $ IS_FRIDAY
)[!is.null(residual)]

dell_return.arma.residuals.model <- lm(residual ~ IS_FRIDAY, dell_return.arma.residuals)

summary(dell_return.arma.residuals.model)
# p-value 0.225, no significant

# 2.10 =========================================================================
maaa <- fread('chapter 2/m-aaa.txt',
              col.names = c('year', 'month', 'day', 'aaa'))
mbaa <- fread('chapter 2/m-baa.txt',
              col.names = c('year', 'month', 'day', 'baa'))

moody <- merge(maaa, mbaa)

moody[,date:=lubridate::make_date(year, month, day)]
moody[,c('year', 'month', 'day') := NULL]

advanced_summary <- function(x) {
  x.excess_kurtosis = moments::kurtosis(x)
  x.kurtosis.var = sqrt(24 / length(x))
  x.kurtosis.statistic <- x.excess_kurtosis / x.kurtosis.var
  x.kurtosis.statistic.abs <- abs(x.kurtosis.statistic)
  x.kurtosis.pvalue <- 2 * pnorm(-x.kurtosis.statistic.abs)

  x.skewness <- moments::skewness(x)
  x.skewness.var <-  sqrt(6 / length(x))
  x.skewness.statistic <- x.skewness / x.skewness.var
  x.skewness.statistic.abs <- abs(x.skewness.statistic)
  x.skewness.pvalue <- 2 * pnorm(-x.skewness.statistic.abs)
  
  list(
    mean=mean(x),
    sd = sd(x),
    skewness = moments::skewness(x),
    skewness_pvalue = x.skewness.pvalue,
    excess_kurtosis = x.excess_kurtosis,
    kurtosis_pvalue = x.kurtosis.pvalue,
    min = min(x),
    max = max(x)
  )
}

moody[,advanced_summary(aaa)]
moody[,advanced_summary(baa)]
# both have heavy tail

aaa <- moody[,aaa]
acf(aaa)
# unitroot

adf.test(aaa)
# p-value = 0.6431

aaa.diff <- diff(aaa)

plot.ts(aaa.diff)
# heteroscedasticity

aaa.diff.offsetted <- aaa.diff - min(aaa.diff) + 0.01

aaa.diff.offsetted.log = log(aaa.diff.offsetted)

plot.ts(aaa.diff.offsetted.log)

aaa.diff.offsetted.log.scaled <- scale(aaa.diff.offsetted.log)

plot.ts(aaa.diff.offsetted.log.scaled, ylim=c(-3,3))

aaa.diff.offsetted.log.scaled.abs = abs(aaa.diff.offsetted.log.scaled)

aaa.diff.offsetted.log.scaled.outlier_removed <- aaa.diff.offsetted.log.scaled[aaa.diff.offsetted.log.scaled.abs < 2]

plot.ts(aaa.diff.offsetted.log.scaled.outlier_removed)

aaa.treated <- aaa.diff.offsetted.log.scaled.outlier_removed

acf(aaa.treated)
# cutoff at 1

pacf(aaa.treated)
# cutoff at 1
# significant at 6 and 11

aaa.treated.arma <- arma(aaa.treated, lag=list(
  ar=c(6, 11),
  ma=c(1)
))

summary(aaa.treated.arma)

acf(aaa.treated.arma $ residuals, na.action = na.remove)
pacf(aaa.treated.arma $ residuals, na.action = na.remove)

Box.test(aaa.treated.arma $ residuals,
         lag = 12,
         type='Ljung-Box',
         fitdf = 3)
# 0.6425

# 2.12 =========================================================================
model <- lm(baa ~ aaa, moody)

summary(model)

# 2.13 =========================================================================
crsp <- fread('chapter 2/m-ew6299.txt') $ V2

plot.ts(crsp)

acf(crsp)
pacf(crsp)

# To AR, p=12
# To MA, p=1

crsp.ar <- arma(crsp, lag=list(
    ar=c(12),
    ma=NULL
  )
)

summary(crsp.ar)

Box.test(crsp.ar $ residuals,
         lag = 12,
         type='Ljung-Box',
         fitdf = 1)
# p-value = 0.001438 (sooo bad)

crsp.ma <- arma(crsp, lag=list(
  ar=NULL,
  ma=c(1)
)
)
Box.test(crsp.ma $ residuals,
         lag = 12,
         type='Ljung-Box',
         fitdf = 1)
# p-value 0.2879

# 2.14 =========================================================================
sp500index <- fread("chapter 2/sp5may.dat",
                    col.names = c("log_future_price",
                                  "log_spot_price",
                                  "cost_of_carry"))
sp500index.diff <- sp500index[,.(
  y = diff(log_future_price),
  x = diff(log_spot_price)
)]

model <- lm(x ~ y, sp500index.diff)

summary(model)

# 2.15 =========================================================================
gdpdef <- fread("chapter 2/q-gdpdef.txt") $ V3

plot.ts(gdpdef)

acf(gdpdef)
# unitroot

adf.test(gdpdef)
# p-value = 0.3672

gdpdef.diff <- diff(gdpdef)

plot.ts(gdpdef.diff)
acf(gdpdef.diff)
# unitroot

adf.test(gdpdef.diff)
# p-value = 0.3593

gdpdef.diff.diff <- diff(gdpdef.diff)

plot.ts(gdpdef.diff.diff)
acf(gdpdef.diff.diff)
# 1, 4, 11, 12, 15, 16
pacf(gdpdef.diff.diff)
# cutoff at 2

gdpdef.diff.diff.arma <- arma(gdpdef.diff.diff, lag=list(
  ar = c(1, 2, 3),
  ma = NULL
), include.intercept = FALSE)

summary(gdpdef.diff.diff.arma)


Box.test(gdpdef.diff.diff.arma $ residuals,
         lag = 12,
         type='Ljung-Box')
# p-value = 0.3829
