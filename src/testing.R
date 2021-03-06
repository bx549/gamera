## script to read testing data and execute the trading strategy
##
datadir <- "~/Downloads/data/test/"
exchange <- "kraken"   # "bittrex" or "kraken"
granularity <- 10      # seconds
#source("prep-data.R")
load("prep-data-testing-xbt-usd.rda")
range(Obook$ts)  # check the date range

## make predictions on out-of-sample data.
## the model should be saved from training.R
load("fm.lm.rda")

## scale the response and the predictor variables
X <- subset(Obook, select=c("delta.bid","net.order.flow","delta.buy.dist",
                            "buy.rate","sell.rate","spread"))
X <- as.data.frame(scale(X))

pred <- predict(fm.lm, newdata=X)

## test the trading strategy

mkt <- "XBT-USD"
usdbal <- 500
btcbal <- 0
qty <- 0.005000 # quantity of BTC to buy/sell = 50000 satoshis
position <- 0   # -1, 0, or +1
profit <- 0
basis <- c()
n <- nrow(Obook)
stopifnot(nrow(X) == nrow(Obook))
n.buys <- 0
n.sells <- 0
losses <- 0
sigma <- sd(pred, na.rm=TRUE)
mu <- mean(pred, na.rm=TRUE)

for (i in 1:n) {
    if (is.na(pred[i])) {
        next
    }
    
    bid <- Obook$bid[i]
    ask <- Obook$ask[i]
    pred.delta <- ((pred[i]*sigma)+mu)*granularity
    ## put the prediction back on the scale of the data.
    ## prediction is rate of change in bid (i.e. per second).
    threshold <- Obook$spread[i]      # ? what to use for the threshold ?

    if (pred.delta > 0 && pred.delta > threshold && position <= 0) {
        ## buy qty btc at ask price
        btcbal <- btcbal + qty
        usdbal <- usdbal - ask * qty
        position <- position + 1
        #message("purchased ", qty, " btc at ", ask)
        basis <- c(ask, basis)  # we purchased at the ask price, LIFO accounting
        n.buys <- n.buys + 1
    }

    if (pred.delta < 0 && abs(pred.delta) > threshold && position >= 0 && btcbal >= qty) {
        ## sell qty btc at bid price
        btcbal <- btcbal - qty
        usdbal <- usdbal + bid * qty
        position <- position - 1
        #message("sold ", qty, " btc at ", bid)
        stopifnot(length(basis) >= 1)
        this.profit <- (bid - basis[1])*qty  # we sold at the bid price, LIFO accounting
        profit  <- profit + this.profit      # accumulate total profit/loss
        basis <- basis[-1]                   # get rid of this cost basis
        n.sells <- n.sells + 1
        losses <- losses + ifelse(this.profit < 0, 1, 0) # count number of losing trades
    }
}

t <- diff(range(Obook$ts))
message("profit is $", round(profit, 2), " over ", round(t, 1), " days.")
message(n.buys, " buys and ", n.sells, " sells")
message("number of sells at a loss = ", losses)
message("average profit per sell = ", profit/n.sells)
