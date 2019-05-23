## obtain data from Kraken
require(httr)
require(jsonlite)
require(lubridate)
source("~/mega/gamera/src/krakenapi.R")

buy.orders.filename <- "Buy.csv"
sell.orders.filename <- "Sell.csv"
recent.trades.filename <- "Trade.csv"
output.dir <- "~/xbt-usd/"

## collect order book data and trade data
mkt <- "XBTUSD"
interval <- 10                # granularity in seconds
seconds.per.day <- 24*60*60
n <- seconds.per.day/interval # number of data collections per day
days <- 21

if (exists("Trade")) { rm(Trade) } # recent trades
if (exists("Buy"))   { rm(Buy) }   # bid side of the order book
if (exists("Sell"))  { rm(Sell) }  # ask side of the order book

for (i in 1:(n*days)) {
    ## timestamp using server time
    ## if server-side timestamp is not available use client-side timestamp
    ts <- tryCatch({
        ts.json <- getservertime()  # server timestamp
        as.POSIXct(ts.json$result$unixtime,
                   origin = "1970-01-01",
                   tz = "GMT")
    },
    warning = function(w) {
        message("getservertime() generated a warning at i = ", i)
        message(w)
        return(with_tz(Sys.time(), tzone="GMT")) # client timestamp
    },
    error = function(e) {
        message("getservertime() generated an error at i = ", i)
        message(e)
        return(with_tz(Sys.time(), tzone="GMT")) # client timestamp
    },
    finally = {
        message("timestamp at i = ", i)
    })

    ## order book data from kraken contains the timestamp at which the
    ## order was placed, but I don't think we need that (at least not
    ## yet). our timestamp ts serves as the time of the snapshot of the
    ## order book. note that time is UTC.
    orderbook <- tryCatch({
        getorderbook(mkt)
    },
    warning = function(w) {
        message("getorderbook() generated a warning at i = ", i)
        message(w)
        return(NULL)
    },
    error = function(e) {
        message("getorderbook() generated an error at i = ", i)
        message(e)
        return(NULL)
    },
    finally = {
        if (exists("orderbook") && length(orderbook$error)) {
            message("getorderbook(): ", orderbook$error)
        }
    })

    if (!is.null(orderbook) && length(orderbook$error)==0) {
        ## bid side
        quantity <- as.numeric(orderbook$result[[1]]$bids[,2])
        rate <- as.numeric(orderbook$result[[1]]$bids[,1])

        if (exists("Buy")) {
            Buy <- rbind(Buy,
                         data.frame(quantity = quantity,
                                    rate = rate,
                                    timestamp = ts))
        } else {
            Buy <- data.frame(quantity = quantity,
                              rate = rate,
                              timestamp = ts)
        }

        ## ask side
        quantity = as.numeric(orderbook$result[[1]]$asks[,2])
        rate = as.numeric(orderbook$result[[1]]$asks[,1])

        if (exists("Sell")) {
            Sell <- rbind(Sell,
                          data.frame(quantity = quantity,
                                     rate = rate,
                                     timestamp = ts))
        } else {
            Sell <- data.frame(quantity = quantity,
                               rate = rate,
                               timestamp = ts)
        }
    }
    
    ## recent trades
    trades <- tryCatch({
        gettrades(mkt)
    },
    warning = function(w) {
        message("gettrades() generated a warning at i = ", i)
        message(w)
        return(NULL)
    },
    error = function(e) {
        message("gettrades() generated an error at i = ", i)
        message(e)
        return(NULL)
    },
    finally = {
        if (exists("trades") && length(trades$error)) {
            message("getrades(): ", trades$error)
        }
        
    })

    if (!is.null(trades) && length(trades$error)==0) {
        price <- as.numeric(trades$result[[1]][,1])
        quantity <- as.numeric(trades$result[[1]][,2])
        ts.num <- as.double(trades$result[[1]][,3])    # this will be the id
        ts <- as.POSIXct(ts.num, origin = "1970-01-01", tz = "GMT")
        ordertype <- trades$result[[1]][,4]   # buy or sell
        mltype <- trades$result[[1]][,5]      # market or limit

        ## Id,TimeStamp,Quantity,Price,Total,FillType,OrderType
        Trade.tmp <- data.frame(Id = ts.num,
                                TimeStamp = ts,
                                Quantity = quantity,
                                Price = price,
                                Type1 = ordertype,
                                Type2 = mltype,
                                stringsAsFactors = FALSE)

        if (nrow(Trade.tmp)) {
            if (exists("Trade")) {
                old.idx <- match(Trade$Id, Trade.tmp$Id)
                                        # already present in Trade
                new.idx <- setdiff(1:nrow(Trade.tmp), old.idx)
                if (length(new.idx)) {
                    Trade <- rbind(Trade.tmp[new.idx,], Trade)
                                        # add new trades only
                }
            } else {
                Trade <- Trade.tmp
            }
        }
    }

    ## write results to disk every 100 iterations
    if (i %% 100 == 0) {
        write.table(Buy, file=paste(output.dir, buy.orders.filename, sep=""),
                    sep=",", quote=FALSE, row.names=FALSE,
                    col.names=(i==100), append=TRUE)
        write.table(Sell, file=paste(output.dir, sell.orders.filename, sep=""),
                    sep=",", quote=FALSE, row.names=FALSE,
                    col.names=(i==100), append=TRUE)
        rm(Buy)   # free memory
        rm(Sell)
        ## Trade is kept in memory so no appending, just overwrite
        ## existing file
        write.table(Trade, file=paste(output.dir, recent.trades.filename, sep=""),
                    sep=",", quote=FALSE, row.names=FALSE, append=FALSE)
        cat("checkpoint completed at iteration", i, "of", n*days, "\n")
    }
    
    Sys.sleep(interval)
}
