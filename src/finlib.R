## this file contains some functions that may be helpful
## for finance-related tasks

## future value of a series of payments
## x is the amount payed each period
## r is the interest rate per period, which is the yearly rate
##   divided by the number of periods per year. for a yearly
##   rate of 5% compounded monthly r = .05/12
## n is the number of periods.
## example:
## future value of making $1200 payments monthly for 20 years
## FV(240, 1200, .05/12)
FV <- function(n, x, r) x * ( (1+r)^n - 1 )/r

## compute value of x dollars after k periods of compound interest
## r is the annual interest rate
## m is the number of periods per year for which interest is applied
## t is the number of years into the future
## for example,
## we have x = $1000 in an account today
## annual interest rate is r = 5%
## interest is componded monthly
## how much will the account hold in 8 years?
## compound(8, 1000, .05, 12)
## 
compound <- function(t, x, r, m) {
    k = m*t  # total number of periods
    x * (1 + r/m)^k
}

reverse <- function(x) x[length(x):1]

invlogit <- function(x) 1/(1+exp(-x))

## functions for computing the accuracy of a model
## hit ratio is for categorical response variables
hit <- function(pred, actual) sum(as.character(pred)==as.character(actual))/length(pred)
ape <- function(pred, actual) mean(abs(pred-actual), na.rm=T) # absolute prediction error

r.squared <- function(pred, actual) 
  1 - (sum((actual-pred)^2) / sum((actual-mean(actual, na.rm=T))^2))

r.absolute <- function(pred, actual) 
  1 - (sum(abs(actual-pred)) / sum(abs(actual-median(actual,na.rm=T))))

## compute exponentially weighted moving average of a time series x.
## lambda is the weight to place on the most recent observation.
ewma <- function(x, lambda=.2) {
    n <- length(x)
    z <- numeric(n)
    z[1] <- x[1]   # to get started

    for (i in 2:length(x)) {
        z[i] <- lambda*x[i] + (1-lambda)*z[i-1]
    }
    z
}

