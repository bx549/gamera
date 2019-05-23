datadir <- "~/Downloads/data/train/"
exchange <- "kraken"   # "bittrex" or "kraken"
granularity <- 10
source("prep-data.R")

## fit a logistic regression model
## pay attention to the time step! the response variable is lead(.)
fm.bid <- lm(lead(delta.bid) ~ delta.bid + net.order.flow + delta.buy.dist, data=Obook)
summary(fm.bid)

# diagnostics for the regression model
layout(matrix(1:4, nrow=2))
plot(fm.bid)

save(fm.bid, file="fm.rda")  # save the fitted model
