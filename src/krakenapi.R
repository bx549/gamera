## this file contains wrapper functions for the Kraken API

library(httr)
library(jsonlite)

## pass in api url
## return list that contains results
## e.g. makeapicall("https://api.kraken.com/0/public/Depth?pair=XBTUSD")
makeapicall <- function(call) {
    json.url  <- GET(call)
    json.text <- content(json.url, "text")
    fromJSON(json.text, flatten=TRUE)
}

## returns server time in number of seconds since Unix epoch
## and also as a string in RFC1123 time format
getservertime <- function() {
    base <- "https://api.kraken.com/0/"
    endpoint <- "public/Time"
    call <- paste(base, endpoint, sep="")
    makeapicall(call)
}
    
## get recent trades
## input
## pair = asset pair to get trade data for
## since = return trade data since given id (optional.  exclusive)
## output
## <pair_name> = pair name
##   array of array entries(<price>, <volume>, <time>, <buy/sell>, <market/limit>, <miscellaneous>)
## last = id to be used as since when polling for new trade data
gettrades <- function(mkt, id=NULL) {
    base <- "https://api.kraken.com/0/"
    endpoint <- "public/Trades"
    if (is.null(id)) {
        params <- paste("pair=", mkt, sep="")
    } else {
        params <- paste("pair=", mkt, "&since=", id, sep="")
    }
    call <- paste(base, endpoint, "?", params, sep="")
    makeapicall(call)
}

## get order book
## input: 
## pair = asset pair to get market depth for
## count = maximum number of asks/bids (optional)
## output: 
## asks = ask side array of array entries(<price>, <volume>, <timestamp>)
## bids = bid side array of array entries(<price>, <volume>, <timestamp>)
## e.g. getorderbook("LTCUSD")
getorderbook <- function(mkt) {
    base <- "https://api.kraken.com/0/"
    endpoint <- "public/Depth"
    params <- paste("pair=", mkt, sep="")
    call <- paste(base, endpoint, "?", params, sep="")
    makeapicall(call)
}

