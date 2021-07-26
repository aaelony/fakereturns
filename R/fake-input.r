suppressPackageStartupMessages(library("BatchGetSymbols"))
suppressPackageStartupMessages(library("data.table"))


#' Generate a list of random dates within a period of time.
#'
#' @param start.date A start date in a format understood by `as.Date` to be the earliest date in the faked data.
#' @param end.date An end date in a format understood by `as.Date` to be the latest date in the faked data.
#' @param n.dates The number of dates to generate.
#' @return Returns a list of dates generated.
#' @export
generate.fake.dates <- function(start.date= '2010-01-01',
                                end.date=Sys.Date(),
                                n.dates = 100
                                ) {

    sample(seq(as.Date(start.date),
               as.Date(end.date),
               by="day"),
           n.dates)

}


#' Generate fake input data for
#' personal investment portfolio
#' finance
#' 
#'
#' @param start.date A start date in a format understood by `as.Date` to be the earliest date in the faked data.
#' @param end.date An end date in a format understood by `as.Date` to be the latest date in the faked data.
#' @param n The number of records (rows) to generate.
#' @param tickers The stock `ticker` strings of the companies desired in the generated dataset.
#' @param pct.bought The percentage of random data per ticker to be labeled "bought" (as opposed to "sold").
#' 
#' @return Returns a simulated data.table
#' @export
fake.some.portfolio.data <- function(start.date= '2010-01-01',
                                     end.date=Sys.Date(),
                                     n = 20,
                                     tickers = c('AAPL', 'NFLX'),
                                     pct.bought = 0.5 ,
                                     purchase.ceiling = 5000  ## ~ $5k at most for purchase
                                     ) {

    d <- data.table::data.table(
                    date = generate.fake.dates(
                        start.date = start.date,
                        end.date   = end.date,
                        n.dates    = n
                    ),
                    ticker = sample(tickers, n, replace=TRUE)

                    ## long/short term capital gain?
                    )
    ## sort by ticker+date, assign the first (random percentage P) to be "bought"s and the remainder "sold"s.
    data.table::setorderv(d, c("ticker", "date"))

    d.cnts.by.ticker <- d[, .N, by="ticker"]
    d.cnts.by.ticker[, purch.labels := floor( N * pct.bought )]

    d[,  `:=`( grp.tot = .N , ith.per.j = 1:.N ) , by = "ticker" ]
    ## bought or sold ??
    d[, portfolio.status := ifelse( ith.per.j < floor(grp.tot * pct.bought), "BOUGHT", "SOLD")]

    ## pull the ticker data for the date interval, and populate the ticker price for each date
    ticker.historical.prices <- BatchGetSymbols::BatchGetSymbols(
                                                     tickers = tickers, 
                                                     first.date = start.date,
                                                     last.date = end.date, 
                                                     freq.data = "daily",
                                                     cache.folder = file.path(tempdir(),
                                                                              'BGS_Cache')
                                                 ) # cache in tempdir()
    
    ticker.hist <-  data.table::as.data.table( ticker.historical.prices[[2]] )

    
    ## Join with the Historical Ticker Prices for the data generated.
    d2 <- merge(d,
                ticker.hist[ticker %in% tickers],
                by.x = c("ticker", "date"),
                by.y = c("ticker", "ref.date"),
                all.x = TRUE
                )

    ## Only keep dates where we can retrieve the historical price
    d2 <- d2[is.na(price.open)==FALSE]
    d2[, price.adjusted:= NULL]
    d2[, ret.adjusted.prices:=NULL]
    d2[, ret.closing.prices:=NULL]

    ## Split the BOUGHT and SOLD transactions and match them together.    
    d2.bought <- d2[portfolio.status=="BOUGHT"]
    d2.sold   <- d2[portfolio.status=="SOLD"]
  
    ## d2.bought[, max.shares.bought := purchase.ceiling / price.high]

    ## Assign the number of shares (arbitrarily based on how many fit into the purchase.ceiling)
    d2.bought[, shares.bought := floor(sqrt(rnorm(
        nrow(d2.bought),
        mean= purchase.ceiling / price.high, sd=40)^2))]
    
    ##d2.bought$shares.bought <- floor(
    ##    rnorm(
    ##        nrow(d2.bought),
    ##        mean = purchase.ceiling / d2.bought$price.high,
    ##        sd = 40))

    ## library(truncnorm)
    ## rtruncnorm(n=10, a=0, b=340, mean=39.4, sd=25.09)
    
    d2.bought[, price.bought := truncnorm::rtruncnorm(1, a= price.low, b= price.high, mean = (price.high-price.low)/2, sd = 40) ]
    d2.bought[, initial.investment.value := price.bought * shares.bought]

    data.table::setcolorder(
                    d2.bought,
                    c("ticker", "portfolio.status", "date", "price.bought", "shares.bought",
                      "initial.investment.value",
                      "price.open", "price.close", "price.low", "price.high", "volume",
                      "grp.tot", "ith.per.j"
                      ))
    names(d2.bought) <-  c("ticker", "portfolio.status", "purchase.date", "purchase.price", "shares.bought",
                           "initial.investment.value",
                           "price.open.at.purchase.date", "price.close.at.purchase.date", "price.low.at.purchase.date", "price.high.at.purchase.date", "volume.at.purchase.date",
                           "grp.tot", "ith.per.j")


    latest.date.available <- ticker.hist[ , max(ref.date) ]
    d2.bought <- merge(
        d2.bought,
        ticker.hist[ref.date==latest.date.available, c("ticker",  "price.close") ],
        by.x="ticker",  by.y="ticker", x.all=TRUE)
    names(d2.bought)[length(names(d2.bought))] <- "latest.price"

    d2.bought[, latest.value:= latest.price * shares.bought]
    d2.bought[, gain.or.loss:= latest.value - initial.investment.value]
    
    
    list(
        fake.data = d2,         
        bought = d2.bought,
        sold = d2.sold,
        ticker.hist = ticker.hist,
        cnts.by.ticker = d.cnts.by.ticker
    )    


}


## test:
##  ll <- fake.some.portfolio.data(start.date= '2015-01-01', end.date=Sys.Date(), n = 40)



todo <- function() {
    cols <- c("ticker", "buy.date", "buy.shares", "buy.price", "sell.date", "sell.price", "sell")

    ## Purchases
    buy.transactions  <- c("ticker", "date", "shares", "price")


    ## Sales
    sell.transactions <- c("ticker", "date", "shares", "price")
    
    ## Tax considerations:
    ##  - [ ] determine if it is a short or long term capital gain.
    ##  - [ ] determine if it is retirement or taxable in the current year


    ## Establish FIFO, LIFO, or other method of matching Purchases to Sales.

    ## Report on Purchases without Sales:
    ## Percent retirement? Percent non-retirement?
    ## IRR retirement? IRR non-retirement?
    
    
}


