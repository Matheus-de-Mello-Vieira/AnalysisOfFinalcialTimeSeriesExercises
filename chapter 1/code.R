library(here)
library(data.table)

# 1 ============================================================================
## preparation =================================================================

# Consider the daily stock returns of American Express (axp), Caterpillar (cat),
# and Starbucks (sbux) from January 1994 to December 2003. The data are
# simple returns given in the ﬁle d-3stock.txt (date, axp, cat, sbux) .

daily_stock_returns <- fread("chapter 1/d-3stock.txt") [,
                                                        .(date=lubridate::ymd(V1, quiet=TRUE),
                                                          axp=V2,
                                                          cat=V3,
                                                          sbux=V4)][!is.na(date)]

setkey(daily_stock_returns, date)

daily_stock_returns[,plot(axp, type='l')]
daily_stock_returns[,plot(cat, type='l')]
daily_stock_returns[,plot(sbux, type='l')]

## A ===========================================================================

# Express the simple returns in percentages. Compute the sample mean,
# standard deviation, skewness, excess kurtosis, minimum, and maximum
# of the percentage simple returns.

summarize <- function(x) {
  sample_normalization = 1 / (length(x)  - 1)
  x.mean = mean(x)
  x.demean = x - x.mean
  x.var = sample_normalization * sum(x.demean ** 2)
  x.sd = sqrt(x.var)
  x.skewness = sample_normalization / (x.sd ** 3) * sum(x.demean ** 3)
  x.kurtosis = sample_normalization / (x.sd ** 4) * sum(x.demean ** 4)
  x.min = min(x)
  x.max = max(x)
  
  c(x.mean, x.sd, x.skewness, x.kurtosis, x.min, x.max)
}

daily_stock_percentage <- daily_stock_returns[,.(axp = axp * 100,
                                                 cat = cat * 100,
                                                 sbux = sbux * 100)]

data.frame(axp = summarize(daily_stock_percentage $ axp),
           cat = summarize(daily_stock_percentage $ cat),
           sbux = summarize(daily_stock_percentage $ sbux),
           row.names = c('mean', 'sd', 'skewness', 'kurtosis', 'min', 'max'))

## B ===========================================================================
# Transform the simple returns to log returns.
daily_stock_log_returns <- daily_stock_returns[,
                                               lapply(.SD, function(x) log(1 + x)),
                                               .SDcols = c('axp', 'cat', 'sbux')]

## C ===========================================================================
# Express the log returns in percentages. Compute the sample mean, stan-
# dard deviation, skewness, excess kurtosis, minimum, and maximum of the
# percentage log returns.
daily_stock_percentages_log_returns <- daily_stock_log_returns * 100

data.frame(axp = summarize(daily_stock_percentages_log_returns $ axp),
           cat = summarize(daily_stock_percentages_log_returns $ cat),
           sbux = summarize(daily_stock_percentages_log_returns $ sbux),
           row.names = c('mean', 'sd', 'skewness', 'kurtosis', 'min', 'max'))

## D ===========================================================================
# Test the null hypothesis that the mean of the log returns of each stock
# is zero. (Perform three separate tests.) Use 5% signiﬁcance level to draw
# your conclusion.

skewness_test <- function(x){
  sample_normalization = 1 / (length(x)  - 1)
  x.mean = mean(x)
  x.demean = x - x.mean
  x.var = sample_normalization * sum(x.demean ** 2)
  x.sd = sqrt(x.var)
  x.skewness = sample_normalization / (x.sd ** 3) * sum(x.demean ** 3)
  x.skewness.var = sqrt(6 / length(x))
  
  statistic <- x.skewness / x.skewness.var
  statistic.abs <- abs(statistic)
  
  pvalue.half <- 1 - pnorm(statistic.abs)
  pvalue <- pvalue.half * 2
  
  c(statistic =  statistic, pvalue = pvalue)
}

kurtosis_test <- function(x){
  sample_normalization = 1 / (length(x) - 1)
  x.mean = mean(x)
  x.demean = x - x.mean
  x.var = sample_normalization * sum(x.demean ** 2)
  x.sd = sqrt(x.var)
  x.kurtosis = sample_normalization / (x.sd ** 4) * sum(x.demean ** 4)
  x.kurtosis.var = sqrt(24 / length(x))
  
  statistic <- (x.kurtosis - 3) / x.kurtosis.var
  statistic.abs <- abs(statistic)
  
  pvalue.half <- 1 - pnorm(statistic.abs)
  pvalue <- pvalue.half * 2
  
  c(kurtosis = x.kurtosis, statistic =  statistic, pvalue = pvalue)
}

hist(daily_stock_percentages_log_returns $ axp)

shapiro.test(daily_stock_percentages_log_returns $ axp)
skewness_test(daily_stock_percentages_log_returns $ axp)
# I suppose that axp is:
# non-normal (p-value < 2.2e-16)
# symmetric (p-value = 0.8288)
wilcox.test(daily_stock_percentages_log_returns $ axp)
# the mean is 0 (p-value = 0.3167)

shapiro.test(daily_stock_percentages_log_returns $ cat)
skewness_test(daily_stock_percentages_log_returns $ cat)
# I suppose that axp is:
# non-normal (p-value < 2.2e-16)
# symmetric (p-value = 0.261724)
wilcox.test(daily_stock_percentages_log_returns $ cat)
# the mean is 0 (p-value = 0.4925)

shapiro.test(daily_stock_percentages_log_returns $ sbux)
skewness_test(daily_stock_percentages_log_returns $ sbux)
# I suppose that axp is:
# non-normal (p-value < 2.2e-16)
# non-symmetric (p-value = 0.0005247316)
# I can't apply the normal test or wilcox
hist(daily_stock_percentages_log_returns $ sbux)

# 2 ============================================================================
## preparation =================================================================

# Answer the same questions as Exercise 1.1 but using monthly stock returns for
# IBM, CRSP value-weighted index (VW), CRSP equal-weighted index (EW),
# and S&P composite index from January 1975 to December 2003. The returns
# of the indexes include dividend distributions. Data ﬁle is m-ibm3dx7503.txt .

monthly_stock_returns <- fread("chapter 1/m-ibm3dx7503.txt")[,
  .(date=lubridate::ymd(V1, quiet=TRUE),
   IBM=V2,
   CRSP.VW=V3,
   CRSP.EW=V4,
   INDEXTSI=V5)][!is.na(date)]

setkey(monthly_stock_returns, date)

monthly_stock_returns[,plot(IBM, type='l')]
monthly_stock_returns[,plot(CRSP.VW, type='l')]
monthly_stock_returns[,plot(CRSP.EW, type='l')]
monthly_stock_returns[,plot(INDEXTSI, type='l')]

## A ===========================================================================

# Express the simple returns in percentages. Compute the sample mean,
# standard deviation, skewness, excess kurtosis, minimum, and maximum
# of the percentage simple returns.

monthly_stock_returns_percentage <- monthly_stock_returns[,.(
  IBM = IBM * 100,
  CRSP.VW = CRSP.VW * 100,
  CRSP.EW = CRSP.EW * 100,
  INDEXTSI = INDEXTSI * 100)]

data.frame(IBM = summarize(monthly_stock_returns_percentage $ IBM),
           CRSP.VW = summarize(monthly_stock_returns_percentage $ CRSP.VW),
           CRSP.EW = summarize(monthly_stock_returns_percentage $ CRSP.EW),
           INDEXTSI = summarize(monthly_stock_returns_percentage $ INDEXTSI),
           row.names = c('mean', 'sd', 'skewness', 'kurtosis', 'min', 'max'))

## B ===========================================================================
# Transform the simple returns to log returns.
monthly_stock_log_returns <- monthly_stock_returns[,
  lapply(.SD, function(x) log(1 + x)),
  .SDcols = c('IBM', 'CRSP.VW', 'CRSP.EW', 'INDEXTSI'),
  by = date]

## C ===========================================================================
# Express the log returns in percentages. Compute the sample mean, stan-
# dard deviation, skewness, excess kurtosis, minimum, and maximum of the
# percentage log returns.
monthly_stock_percentages_log_returns <- monthly_stock_log_returns * 100

data.frame(IBM = summarize(monthly_stock_percentages_log_returns $ IBM),
           CRSP.VW = summarize(monthly_stock_percentages_log_returns $ CRSP.VW),
           CRSP.EW = summarize(monthly_stock_percentages_log_returns $ CRSP.EW),
           INDEXTSI = summarize(monthly_stock_percentages_log_returns $ INDEXTSI),
           row.names = c('mean', 'sd', 'skewness', 'kurtosis', 'min', 'max'))

## D ===========================================================================
# Test the null hypothesis that the mean of the log returns of each stock
# is zero. (Perform three separate tests.) Use 5% signiﬁcance level to draw
# your conclusion.
shapiro.test(monthly_stock_percentages_log_returns $ IBM)
skewness_test(monthly_stock_percentages_log_returns $ IBM)
# I suppose that axp is:
# non-normal (p-value = 0.0008087)
# symmetric (p-value = 0.5857078)
wilcox.test(monthly_stock_percentages_log_returns $ IBM)
# the mean is 0 (p-value = 0.03156)

# Test the null hypothesis that the mean of the log returns of each stock
# is zero. (Perform three separate tests.) Use 5% signiﬁcance level to draw
# your conclusion.
shapiro.test(monthly_stock_percentages_log_returns $ CRSP.VW)
skewness_test(monthly_stock_percentages_log_returns $ CRSP.VW)
# I suppose that axp is:
# non-normal (p-value = 2.16e-08)
# no symmetric (p-value = 2.206679e-12)
# I can't apply the normal test or wilcox
hist(monthly_stock_percentages_log_returns $ CRSP.VW)
# not evidences the mean is 0

# Test the null hypothesis that the mean of the log returns of each stock
# is zero. (Perform three separate tests.) Use 5% signiﬁcance level to draw
# your conclusion.
shapiro.test(monthly_stock_percentages_log_returns $ CRSP.EW)
skewness_test(monthly_stock_percentages_log_returns $ CRSP.EW)
# I suppose that axp is:
# non-normal (p-value = 6.421e-11)
# no symmetric (p-value = 2.558423e-08)
hist(monthly_stock_percentages_log_returns $ CRSP.EW)

# Test the null hypothesis that the mean of the log returns of each stock
# is zero. (Perform three separate tests.) Use 5% signiﬁcance level to draw
# your conclusion.
shapiro.test(monthly_stock_percentages_log_returns $ INDEXTSI)
skewness_test(monthly_stock_percentages_log_returns $ INDEXTSI)
# I suppose that axp is:
# non-normal (p-value = 7.757e-07)
# no symmetric (p-value = 1.411325e-08)
hist(monthly_stock_percentages_log_returns $ INDEXTSI)

# 3 ============================================================================

# Consider the monthly stock returns of S&P composite index from January
# 1975 to December 2003 in Exercise 1.2. Answer the following questions:

monthly_stock_returns <- fread("chapter 1/m-ibm3dx7503.txt")[,
   .(date=lubridate::ymd(V1, quiet=TRUE),
     IBM=V2,
     CRSP.VW=V3,
     CRSP.EW=V4,
     INDEXTSI=V5)][!is.na(date)]

## A ===========================================================================
# What is the average annual log return over the data span?

monthly_stock_log_returns[,.(
  IBM=sum(IBM),
  CRSP.VW=sum(CRSP.VW),
  CRSP.EW=sum(CRSP.EW),
  INDEXTSI=sum(INDEXTSI)
  ),by=format(date, "%Y")]

## B ===========================================================================
# Assume that there were no transaction costs. If one invested $1.00 on the
# S&P composite index at the beginning of 1975, what was the value of the
# investment at the end of 2003?
monthly_stock_log_returns[,sum(INDEXTSI)]

# 4 ============================================================================
# Consider the daily log returns of American Express stock from January 1994
# to December 2003 as in Exercise 1.1. Use the 5% signiﬁcance level to perform
# the following tests. (a) Test the null hypothesis that the skewness measure of
# the returns is zero. (b) Test the null hypothesis that the excess kurtosis of
#the returns is zero.

skewness_test(daily_stock_percentages_log_returns $ axp)
kurtosis_test(daily_stock_percentages_log_returns $ axp)
timeDate::kurtosis

hist(daily_stock_percentages_log_returns $ axp)

x <- seq(min(daily_stock_percentages_log_returns $ axp),
         max(daily_stock_percentages_log_returns $ axp),
         by = .1)
y <- dnorm(x,
           mean = mean(daily_stock_percentages_log_returns $ axp),
           sd = sd(daily_stock_percentages_log_returns $ axp))

plot(density(daily_stock_percentages_log_returns $ axp))
lines(x,y)

# 5 ============================================================================
# Daily foreign exchange rates (spot rates) can be obtained from the Federal
# Reserve Bank in Chicago. The data are the noon buying rates in New York City
# certiﬁed by the Federal Reserve Bank of New York. Consider the exchange
# rates between the U.S. dollar and the Canadian dollar, euro, U.K. pound, and
#Japanese yen from January 2000 to March 2004. (a) Compute the daily log
# return of each exchange rate. (b) Compute the sample mean, standard
# devia-tion, skewness, excess kurtosis, minimum, and maximum of the log returns
# of each exchange rate. (c) Discuss the empirical characteristics of the log
# returns of exchange rates.

daily_exchange_rates <- fread("chapter 1/d-fxjp00.txt")[,
    .(date=lubridate::ymd(V1, quiet=TRUE),
      fx=V2)][!is.na(date)]
setkey(daily_exchange_rates, date)

daily_exchange_log_return <- daily_exchange_rates[,.(
  date = date + lubridate::days(1),
  fx=fx / shift(fx, type='lead')
)][!is.na(fx)][,.(date=date,fx=log(1 + fx))]

data.frame(fx = summarize(daily_exchange_log_return $ fx),
           row.names = c('mean', 'sd', 'skewness', 'kurtosis', 'min', 'max'))

daily_exchange_log_return[,plot(fx, type='line')]
# stationary

daily_exchange_log_return[,boxplot(fx, horizontal=TRUE)]
# beetween 0.6 and 0.8

daily_exchange_log_return[,hist(fx)]
