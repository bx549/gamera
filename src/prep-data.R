## this module reads data that was gathered via the exchange api
## and produces two data frames that are suitable for training
## and/or testing predictive models. The data frames are:
## Trade : history of recent trades
## Obook : timestamped order book data

## all times are converted to UTC (if not already in that timezone)

library(tidyverse)
library(lubridate)
source("finlib.R")

## read raw data
mkthistoryfile <- paste(datadir, "Trade.csv", sep="")
buyorderbookfile <- paste(datadir, "Buy.csv", sep="")
sellorderbookfile <- paste(datadir, "Sell.csv", sep="")

## market history data has UCT timezone
Trade <- read.csv(mkthistoryfile, as.is=TRUE)

if (exchange == "bittrex") {
    Trade$ts <- as.POSIXct(substr(Trade$TimeStamp, 1, 19),
                           "%Y-%m-%dT%H:%M:%S", tz="GMT")
} else {  # kraken
    Trade$ts <- as.POSIXct(Trade$TimeStamp,
                           "%Y-%m-%d %H:%M:%S", tz="GMT")
}
Trade$TimeStamp <- NULL   # no longer needed

## there should be no duplicates.
## remove any duplicate trades. this should not happen, but it happened once.
## for now let's handles this case-by-case
if (length(unique(Trade$Id)) != nrow(Trade)) {
    warning("duplicate trades detected!")
    dups <- duplicates(Trade$Id)
    for (d in dups) {
        idx <- which(Trade$Id == d)
        omit <- idx[-length(idx)] # indices to omit, all but the last
        Trade <- Trade[-omit,]
    }
}

## put the data frame in increasing chronological order
Trade <- arrange(Trade, ts)

## derive some variables from the Trade data
Trade$price.ewma <- ewma(Trade$Price, lambda=.2)

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
    summarise(bid = max(rate),
              buy.mean = weighted.mean(rate, quantity),
              buy.dist = bid - buy.mean)

## sell.dist is similar to buy.dist.
## it is an indication of sell pressure.
Sell.summary <- Sell %>%
    group_by(ts) %>%
    summarise(ask = min(rate),
              sell.mean = weighted.mean(rate, quantity),
              sell.dist = sell.mean - ask)

## combine order book data into a single data frame
stopifnot(all(Buy.summary$ts == Sell.summary$ts))
Obook <- cbind(Buy.summary, select(Sell.summary, -ts))

rm(Buy)
rm(Sell)

## derive some variables from the orderbook data
Obook$spread <- with(Obook, ask-bid)
Obook$midprice <- with(Obook, bid + spread/2)

## pass in times t1 and t2 and vector of times x. return TRUE/FALSE indicator
## of all times between t1 and t2. note that t1 < t2.
get.time.ind <- function(t1, t2, x) {
    t1 <= x & x <= t2
}

## compute the net order flow, which is the difference between the volume
## of buy and sell MOs and LOs during the last minute.
n <- nrow(Obook)
Obook$buy.volume <- numeric(n)
Obook$sell.volume <- numeric(n)
Obook$net.order.flow <- numeric(n)
Obook$buy.rate <- integer(n)   # number of market buys per minute (at the ask)
Obook$sell.rate <- integer(n)  # number of market sells per minute (at the bid)

for (i in 1:n) {
    ## compute buy/sell volume over the last 60 seconds
    ## compute buy/sell rate. orders per minute
    t2 <- Obook$ts[i]
    t1 <- t2 - 60
    time.idx1 <- get.time.ind(t1, t2, Trade$ts)
    if (exchange == "bittrex") {
        stop("cannot compute buy/sell rate")
        Obook$buy.volume[i]  <- with(Trade, sum(Quantity[OrderType=="BUY" & time.idx1]))
        Obook$sell.volume[i] <- with(Trade, sum(Quantity[OrderType=="SELL" & time.idx1]))
    } else { # data is from kraken
        Obook$buy.volume[i]  <- with(Trade, sum(Quantity[Type1=="b" & time.idx1]))
        Obook$sell.volume[i] <- with(Trade, sum(Quantity[Type1=="s" & time.idx1]))
        Obook$buy.rate[i] <- with(Trade, length(Id[Type1=="b" & Type2=="m" & time.idx1]))
        Obook$sell.rate[i] <- with(Trade, length(Id[Type1=="s" & Type2=="m" & time.idx1]))
    }
}
Obook$net.order.flow <- with(Obook, buy.volume - sell.volume)

## a positive value for buy.dist.delta/sell.dist.delta means that the
## buy.dist/sell.dist has moved further away from the bid/ask.
Obook$delta.buy.dist <- with(Obook, buy.dist - lag(buy.dist))
Obook$delta.sell.dist <- with(Obook, sell.dist - lag(sell.dist))

## we will need the actual time between successive observations because
## the time step is <granularity> seconds, but sometimes the data collection
## script misses a time step. 
Obook$ts.diff <- c(NA, diff(Obook$ts))

## try using a rate of change (i.e. per second)
Obook$delta.bid <- with(Obook, (bid - lag(bid))/ts.diff)
Obook$delta.ask <- with(Obook, (ask - lag(ask))/ts.diff)

## some plots
if (0) {
    ggplot(Trade, aes(x=ts)) + geom_line(aes(y=Price))

    ggplot(Obook) + geom_histogram(aes(x=delta.bid), binwidth=1)
    ggplot(Obook) + geom_histogram(aes(x=buy.volume), binwidth=1)
    ggplot(Obook) + geom_histogram(aes(x=sell.rate), binwidth=1)

    ggplot(Obook) + geom_line(aes(x=ts, y=net.order.flow))
    ggplot(Obook) + geom_line(aes(x=ts, y=spread))
}
