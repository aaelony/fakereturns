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

    x <- sample(seq(as.Date(start.date),
               as.Date(end.date),
               by="day"),
               n.dates * 2  ## get 2x and then only keep the weekdays (which is hopefully enough).
               )

    x[ ! weekdays(x, abbreviate=TRUE) %in% c('Sat', 'Sun')][1:n.dates]

}



generate.shares.sold.info <- function(bought.dataset,
                                      fraction.of.shares.to.sell,
                                      sold.reservoir
                                      ) {

    #' For each BUY, potentially sell some fraction of the BOUGHT shares.
    #'  Make sure:
    #'    1. The SOLD date is at or exceeds the BOUGHT date.
    #'    2. The number of shares sold is a fraction of the BOUGHT shares.

    d1 <- bought.dataset

    d1$sold_date <- as.Date(
        unlist(
            lapply(
                d1$purchase_date,
                function(DATE) {
                    generate.fake.dates(start.date= DATE,
                                        end.date = Sys.Date(),
                                        n.dates=100)[1]
                })),
        origin="1970-01-01")
        
    ## d1[, c("purchase_date", "sold_date")]
    d1[, shares_sold := floor(fraction.of.shares.to.sell * shares_bought)]

    d1[, c("ticker", "purchase_date",  "purchase_day", "purchase_price", "shares_bought",  "initial_investment_value",
           "price_open_at_purchase_date", "price_close_at_purchase_date", "price_low_at_purchase_date", "price_high_at_purchase_date",
           "volume_at_purchase_date", "latest_price",
           "latest_value",  "gain_or_loss", "fraction.of.shares.to.sell", "sold_date",  "shares_sold")]
}




match.bought.with.sold <- function(d.bought, d.sold, n.shares, method="FIFO") {

    dim(d.bought)
    dim(d.sold)

    ## FIFO

    ## For example, say you bought 150 shares of Company A stock for $40 per share
    ## six years ago and another 150 shares of Company A stock for $50 per share four years ago.
    ## If you're selling 200 shares today for $65 per share and using the FIFO method,

    ## you sell 150 shares with a cost of $40 and 50 shares with a cost of $50.
    ## That gives you a taxable profit of $4,500.
    ## If, in the same scenario, you use the LIFO method, you sell 50 shares with a
    ## cost of $40 and 150 with a cost of $50. That gives you a taxable profit of only $3,500

    ## ticker|date|shares|price|
    ## A|2015-01-01|150|40.00|BOUGHT
    ## A|2017-01-01|150|50.00|BOUGHT
    
    ## A|2021-01-01|200|65.00|SOLD   ## 200 shares to exhaust as FIFO, add row for any remainder of shares as needed

    ## combined & merged
    ## ticker|buydate|shares|sale_price|price_gain_or_loss|
    ## A|2015-01-01|150|40.00|65.00|65-40 = 25|
    ## A|2017-01-01|50|50.00|65.00|65-50 = 15|
    ## A|2017-01-01|100|50.00|NA|NA|

    
    ## 1. Get list of tickers sold.
    tickers <- unique(d.sold$ticker)
    
    ## 2. for each ticker do:
    d <- data.table::rbindlist(
                         lapply(
                             tickers,
                             function(T) {  ## n.shares
                                 dd.sold   <- d.sold[ticker==T]
                                 dd.bought <- d.bought[ticker==T]

                                 sold.shares.to.attribute.to.buys <- n.shares
                                 
                                 while (  sold.shares.to.attribute.to.buys > 0 ) {

                                     
                                 }
                                 
                                 ##  a. first sold date values and find the number of shares sold.
                                 ##  b. for that number of shares sold, find the earliest buys until the shares are exhausted
                                 ##  c. assign the buy prices to the shares sold as they are exhausted.
                                 ##  d. add price gain or loss to the buy data.

                             }
                             ))
    


    ## Sort by oldest sold_date.
    
    ## For each row in d.sold that has a sold_date >= purchase_date
    
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

    ## Generate `n` transactions
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

    d[, day:= weekdays(date, abbreviate = T)]
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

    ## Split the BOUGHT and SOLD transactions. 
    d2.bought <- d2[portfolio.status=="BOUGHT"]
    d2.sold   <- d2[portfolio.status=="SOLD"]

  
    ## Assign the number of shares (arbitrarily based on how many fit into the purchase.ceiling)
    d2.bought[, shares.bought := floor(1 + sqrt(rnorm(
        nrow(d2.bought),
        mean= purchase.ceiling / price.high, sd=40)^2))]

    d2.bought[, price.bought := truncnorm::rtruncnorm(1, a= price.low, b= price.high, mean = (price.high-price.low)/2, sd = 40) ]
    d2.bought[, initial.investment.value := price.bought * shares.bought]

    data.table::setcolorder(
                    d2.bought,
                    c("ticker", "portfolio.status", "date", "day", "price.bought", "shares.bought",
                      "initial.investment.value",
                      "price.open", "price.close", "price.low", "price.high", "volume",
                      "grp.tot", "ith.per.j"
                      ))
    names(d2.bought) <-  c("ticker", "portfolio_status", "purchase_date", "purchase_day", "purchase_price", "shares_bought",
                           "initial_investment_value",
                           "price_open_at_purchase_date", "price_close_at_purchase_date", "price_low_at_purchase_date", "price_high_at_purchase_date", "volume_at_purchase_date",
                           "grp_tot", "ith_per_j")


    latest.date.available <- ticker.hist[ , max(ref.date) ]
    d2.bought <- merge(
        d2.bought,
        ticker.hist[ref.date==latest.date.available, c("ticker",  "price.close") ],
        by.x="ticker",  by.y="ticker", x.all=TRUE)
    names(d2.bought)[length(names(d2.bought))] <- "latest_price"

    d2.bought[, latest_value:= latest_price * shares_bought]
    d2.bought[, gain_or_loss:= latest_value - initial_investment_value]


    match_algo          <- "FIFO"


    d2.bought
    #' ticker, portfolio_status, purchase_date, purchase_day, purchase_price, shares_bought, initial_investment_value, price_open_at_purchase_date,
    #' price_close_at_purchase_date, price_low_at_purchase_date, price_high_at_purchase_date, volume_at_purchase_date, grp_tot, ith_per_j, latest_price, latest_value, gain_or_loss



    d3 <- generate.shares.sold.info(
        bought.dataset             = d2.bought,
        fraction.of.shares.to.sell = 0.5,
        sold.reservoir             = d2.sold
    )

    
    ## list(
       ## fake.data = d2,         
       ## bought = d2.bought,
       ## sold = d2.sold,
       ## ticker.hist = ticker.hist ## ,
        ## cnts.by.ticker = d.cnts.by.ticker
    ## )    

    d3

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


