datadir <- "~/Downloads/data/train/"
exchange <- "kraken"   # "bittrex" or "kraken"
granularity <- 10
#source("prep-data.R")
load("prep-data-training-xbt-usd.rda")
range(Obook$ts)  # check the date range


## scale the response and the predictor variables
X <- subset(Obook, select=c("delta.bid","net.order.flow","delta.buy.dist","arr.rate","spread"))
X <- as.data.frame(scale(X))

## pay attention to the time step! the response variable is lead(.).
## remember that the delta.bid is a rate of $ per second
fm.lm <- lm(lead(delta.bid) ~ delta.bid + net.order.flow + delta.buy.dist + arr.rate + spread, data=X)
summary(fm.lm)

# diagnostics for the regression model
layout(matrix(1:4, nrow=2))
plot(fm.lm)

save(fm.lm, file="fm.lm.rda")  # save the fitted model

