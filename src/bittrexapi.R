library(httr)
library(jsonlite)

## make API call to get market summary
## e.g. getmarketsummary("BTC-XMR")
getmarketsummary <- function(mkt) {
    base <- "https://bittrex.com/api/v1.1/"
    endpoint <- "public/getmarketsummary"
    params <- paste("market=", mkt, sep="")
    call <- paste(base,endpoint,"?",params,sep="")

    mktsummary.url  <- GET(call)
    mktsummary.text <- content(mktsummary.url, "text")
    mktsummary.list <- fromJSON(mktsummary.text, flatten=TRUE)

    mktsummary.list
}

## make API call to get order book
## e.g. getorderbook("BTC-XMR")
getorderbook <- function(mkt) {
    base <- "https://bittrex.com/api/v1.1/"
    endpoint <- "public/getorderbook"
    params <- paste("market=", mkt, "&type=both", sep="")
    call <- paste(base,endpoint,"?",params,sep="")

    orderbook.url  <- GET(call)
    orderbook.text <- content(orderbook.url, "text")
    orderbook.list <- fromJSON(orderbook.text, flatten=TRUE)

    orderbook.list
}

## make API call to the latest trades that have occurred
## for a specific market, e.g. getmarkethistory("BTC-DOGE")
getmarkethistory <- function(mkt) {
    base <- "https://bittrex.com/api/v1.1/"
    endpoint <- "public/getmarkethistory"
    params <- paste("market=", mkt, sep="")
    call <- paste(base,endpoint,"?",params,sep="")

    mkthistory.url  <- GET(call)
    mkthistory.text <- content(mkthistory.url, "text")
    mkthistory.list <- fromJSON(mkthistory.text, flatten=TRUE)

    mkthistory.list
}

##
buylimit <- function(mkt, qty, rate) {
    TRUE
}

##
selllimit <- function(mkt, qty, rate) {
    TRUE
}
    
##
getbalance <- function(currency) {
    NULL
}
