## this module reads data that was gathered via the exchange api
## and produces two data frames that are suitable for training
## and testing predictive models. The data frames are:
## Trade : history of recent trades
## Obook : timestamped order book data

## all times are converted to UTC (if not already in that timezone)

library(tidyverse)
library(lubridate)
source("finlib.R")

## load training data
mkthistoryfile <- "../data/train/Trade.csv"
buyorderbookfile <- "../data/train/Buy.csv"
sellorderbookfile <- "../data/train/Sell.csv"

exchange <- "kraken"   # "bittrex" or "kraken"

## market history data has UCT timezone
Trade <- read.csv(mkthistoryfile, as.is=TRUE)

if (exchange == "bittrex") {
    Trade$ts <- as.POSIXct(substr(Trade$TimeStamp, 1, 19),
                           "%Y-%m-%dT%H:%M:%S", tz="GMT")
} else {
    Trade$ts <- as.POSIXct(Trade$TimeStamp,
                           "%Y-%m-%d %H:%M:%S", tz="GMT")
}

stopifnot(length(unique(Trade$Id)) == nrow(Trade))
## there should be no duplicates

## put the data frame in increasing chronological order
Trade <- arrange(Trade, ts)

## derive some variables from the Trade data
Trade$price.ewma <- ewma(Trade$Price, lambda=.2)

## plot prices at which trades occurred
ggplot(Trade, aes(x=ts)) + geom_line(aes(y=Price))

## read in order book data
Buy  <- read.csv(buyorderbookfile, as.is=TRUE)
Sell <- read.csv(sellorderbookfile, as.is=TRUE)

## bittrex order book data has local timezone
## because I added the timestamp in local time
## (need to change that to GMT)
if (exchange == "bittrex") {
    Buy$ts  <- as.POSIXct(Buy$timestamp, "%Y-%m-%d %H:%M:%S",
                          tz="America/Chicago")
    Sell$ts <- as.POSIXct(Sell$timestamp, "%Y-%m-%d %H:%M:%S",
                          tz="America/Chicago")
    Buy$ts  <- with_tz(Buy$ts, tzone="GMT")
    Sell$ts <- with_tz(Sell$ts, tzone="GMT")
} else {
    Buy$ts  <- as.POSIXct(Buy$timestamp, "%Y-%m-%d %H:%M:%S", tz="GMT")
    Sell$ts <- as.POSIXct(Sell$timestamp, "%Y-%m-%d %H:%M:%S", tz="GMT")
}

stopifnot(length(unique(Buy$timestamp)) ==
          length(unique(Sell$timestamp)))

## summarize the orderbook
## the buy.distance is the distance between the bid and the
## weighted mean of the buy side of the order book. it is an
## indication of buy pressure.
Buy.summary <- Buy %>%
    group_by(ts) %>%
    summarise(buy.mean = weighted.mean(rate, quantity),
              bid = max(rate),
              buy.dist = bid - buy.mean)

## sell.dist is similar to buy.dist.
## it is an indication of sell pressure.
Sell.summary <- Sell %>%
    group_by(ts) %>%
    summarise(sell.mean = weighted.mean(rate, quantity),
              ask = min(rate),
              sell.dist = sell.mean - ask)

## combine order book data into a single data frame
stopifnot(all(Buy.summary$ts == Sell.summary$ts))
Obook <- cbind(Buy.summary, select(Sell.summary, -ts))

## derive some variables from the orderbook data
Obook$spread <- with(Obook, ask-bid)
Obook$midprice <- with(Obook, bid + spread/2)

## pass in a time t and vector of times x. return TRUE/FALSE indicator
## of all times within the last interval minutes
get.time.ind <- function(t, x, interval=10) {
    nsec <- interval*60
    diffs <- as.numeric(t - x)
    0 <= diffs & diffs <= nsec 
}

## compute the net order flow, which is the difference between the volume
## of buy and sell MOs and LOs during the last 30 minutes.
Obook$buy.volume <- numeric(nrow(Obook))
Obook$sell.volume <- numeric(nrow(Obook))
Obook$net.order.flow <- numeric(nrow(Obook))
for (i in 1:nrow(Obook)) {
    time.idx <- get.time.ind(Obook$ts[i], Trade$ts, 30)
    Obook$buy.volume[i]  <- with(Trade, sum(Quantity[Type1=="b" & time.idx]))
    Obook$sell.volume[i] <- with(Trade, sum(Quantity[Type1=="s" & time.idx]))
    Obook$net.order.flow[i] <- with(Obook, buy.volume[i] - sell.volume[i])
}

## what data will be available at the time the prediction is made?
## don't cheat!
## a positive value for buy.dist.delta/sell.dist.delta means that the
## buy.dist/sell.dist has moved further away from the bid/ask.
Obook$delta.buy.dist <- with(Obook, buy.dist - lag(buy.dist))
Obook$delta.sell.dist <- with(Obook, sell.dist - lag(sell.dist))

## we will need the actual time between successive observations
Obook$ts.diff <- c(NA, diff(Obook$ts))


Obook$delta.mid <- with(Obook, ifelse(20 <= ts.diff & ts.diff <= 40,
                               midprice - lag(midprice), NA))

Obook$delta.bid <- with(Obook, ifelse(20 <= ts.diff & ts.diff <= 40,
                              bid - lag(bid), NA))

Obook$delta.ask <- with(Obook, ifelse(20 <= ts.diff & ts.diff <= 40,
                              ask - lag(ask), NA))


## plot
ggplot(Obook, aes(x=ts)) + geom_line(aes(y=delta.bid))


## fit a logistic regression model
fm.bid.up <- glm(delta.bid > 0 ~ I(lag(delta.buy.dist)/lag(ts.diff)),
                 data=Obook, family=binomial(link="logit"))
summary(fm.bid.up)

# diagnostics for the regression model
layout(matrix(1:4, nrow=2))
plot(fm.bid.up)





