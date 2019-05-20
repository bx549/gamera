## prep-data.R with testing datadir
datadir <- "~/Downloads/data/test/"
source("prep-data.R")

## make predictions on out-of-sample data
pred.bid.up <- predict(fm.bid.up, newdata=Obook, type="response") > .4

summary(pred.bid.up)

## measure out-of-sample overall error rate
mean(pred.bid.up != (Obook$delta.bid>0), na.rm=TRUE)

## measure out-of-sample false positive error rate
sum(pred.bid.up==TRUE & (Obook$delta.bid>0)==FALSE, na.rm=TRUE)/
    sum((Obook$delta.bid>0)==FALSE, na.rm=TRUE)

## test the trading strategy
mkt <- "BTC-XRP"
xrpbal <- 100
btcbal <- 0.00100000  # 100000 satoshis
qty <- 1        # quantity of xrp to buy/sell
state <- "none"   # long, short, or none
profit <- 0
n <- nrow(Obook)
for (i in 3:n) {
    if (xrpbal < 1 || btcbal < 0.00001000) {
        message("stop trading due to low balance")
        break
    }
    bid <- Obook$bid[i]
    ask <- Obook$ask[i]
    if (pred.bid.up[i] && state=="none") {
        ## buy qty xrp at market ask price
        xrpbal <- xrpbal + 1
        btcbal <- btcbal - ask
        state <- "long"
        basis <- ask
        message("purchased ", qty, " xrp at ", ask)
    }
    if (state == "long" && bid > basis) {
        ## sell qty xrp at market bid price
        xrpbal <- xrpbal - 1
        btcbal <- btcbal + bid
        profit <- profit + bid - basis
        state <- "none"
        message("sold ", qty, " xrp at ", bid)
    }
}

