## script to read testing data and execute the trading strategy
##
datadir <- "~/Downloads/data/test/"
exchange <- "kraken"   # "bittrex" or "kraken"
granularity <- 10
source("prep-data.R")

range(Obook$ts)  # check the date range

## make predictions on out-of-sample data.
## the model should be saved from training.R
load("fm.rda")
pred.bid <- predict(fm.bid, newdata=Obook, type="response")
Obook$pred.bid <- pred.bid
actual <- Obook$delta.bid

ggplot(Obook) + geom_histogram(aes(x=pred.bid-actual))

## test the trading strategy
mkt <- "XBT-USD"
usdbal <- 500
btcbal <- 0
qty <- 0.0005000  # quantity of BTC to buy/sell = 5000 satoshis
position <- 0   # -1, 0, or +1
profit <- 0
basis <- c()
n <- nrow(Obook)

for (i in 1:n) {
    if (is.na(Obook$pred.bid[i])) {
        next
    }
    
    if (usdbal < 50) {
        message("stop trading due to low balance")
        break
    }
    
    bid <- Obook$bid[i]
    ask <- Obook$ask[i]
    pred <- pred.bid[i]
    spread <- Obook$spread[i]

    if (pred > 0 && pred > spread && position <= 0) {
        ## buy qty btc at ask price
        btcbal <- btcbal + qty
        usdbal <- usdbal - ask * qty
        position <- position + 1
        message("purchased ", qty, " btc at ", ask)
        basis <- c(ask, basis)  # lifo
    }

    if (pred < 0 && abs(pred) > spread && position >= 0 && btcbal >= qty) {
        ## sell qty btc at bid price
        btcbal <- btcbal - qty
        usdbal <- usdbal + bid * qty
        position <- position - 1
        message("sold ", qty, " btc at ", bid)
        stopifnot(length(basis) >= 1)
        profit  <- profit + (bid - basis[1])*qty # lifo
        basis <- basis[-1]
    }
    
}

rng <- range(Obook$ts)
t <- as.numeric(rng[2] - rng[1])
message("profit is $", round(profit, 2), " over ", round(t, 1), " days.")
