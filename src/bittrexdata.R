## collect order book data and trade data from Bittrex
source("~/mega/gamera/src/bittrexapi.R")

mkt <- "BTC-XRP"              # the trade pair
outputdir <- "~/btc-xrp/"     # output files go here
interval <- 10                # granularity in seconds
seconds.per.day <- 24*60*60
n <- seconds.per.day/interval # number of data collections per day
days <- 14                    # number of days for data collection

if (exists("Trade")) { rm(Trade) }  # in case old data frames exist
if (exists("Buy"))   { rm(Buy) }
if (exists("Sell"))  { rm(Sell) }

for (i in 1:(n*days)) {
    message("iteration ", i)
    ts <- Sys.time()   # local timezone

    ## attempt to get market history data
    mkthistory <- tryCatch({
        getmarkethistory(mkt)
    }, warning = function(war) {
        print( paste("my warning: ", war) )
        return(FALSE)
    }, error = function(err) {
        print( paste("my error: ", err) )
        return(FALSE)
    }, finally = {
        FALSE
    })
    
    if (mkthistory[[1]]) {
        ## regarding the condition in the if() test, mkthistory is a data frame.
        ## the first element is a logical success indicator. in the event of a
        ## warning or error, FALSE[[1]] will evaluate to FALSE.
        if (exists("Trade")) {
            old.idx <- match(Trade$Id, mkthistory$result$Id)   # already present in Trade
            new.idx <- setdiff(1:nrow(mkthistory$result), old.idx)
            Trade <- rbind(mkthistory$result[new.idx,], Trade) # add new trades only
        } else {
            Trade <- mkthistory$result
        }
    } else {
        warning("failed to obtain market history data for i =", i, "\n")
    }

    ## attempt to get orderbook data
    orderbook <- tryCatch({
        getorderbook(mkt)
    }, warning = function(war) {
        print( paste("my warning: ", war) )
        return(FALSE)
    }, error = function(err) {
        print( paste("my error: ", err) )
        return(FALSE)
    }, finally = {
        FALSE
    })

    if (orderbook[[1]]) {
        if (exists("Buy")) {
            Buy <- rbind(Buy,
                         data.frame(quantity = orderbook$result$buy$Quantity,
                                    rate = orderbook$result$buy$Rate,
                                    timestamp = ts))
        } else {
            Buy <- data.frame(quantity = orderbook$result$buy$Quantity,
                              rate = orderbook$result$buy$Rate,
                              timestamp = ts)
        }
        if (exists("Sell")) {
            Sell <- rbind(Sell,
                          data.frame(quantity = orderbook$result$sell$Quantity,
                                     rate = orderbook$result$sell$Rate,
                                     timestamp = ts))
        } else {
            Sell <- data.frame(quantity = orderbook$result$sell$Quantity,
                               rate = orderbook$result$sell$Rate,
                               timestamp = ts)
        }
    } else {
        warning("failed to obtain order book data for i =", i, "\n")
    }
    
    ## write results to disk every 100 iterations
    if (i %% 100 == 0) {
        write.table(Buy, file=paste(outputdir, "Buy.csv", sep=""), sep=",",
                    quote=FALSE, row.names=FALSE, col.names=(i==100), append=TRUE)
        write.table(Sell, file=paste(outputdir, "Sell.csv", sep=""), sep=",",
                    quote=FALSE, row.names=FALSE, col.names=(i==100), append=TRUE)
        rm(Buy)   # free memory
        rm(Sell)
        ## Trade is kept in memory so no appending, just overwrite
        ## existing file
        write.table(Trade, file=paste(outputdir, "Trade.csv", sep=""), sep=",", quote=FALSE,
                    row.names=FALSE)
        cat("checkpoint completed at iteration", i, "of", n*days, "\n")
    }
    
    Sys.sleep(interval)
}
