# fakereturns

A toy R package to generate _fake but realistic_ data for various analytic purposes.


Very Alpha, not ready for use.



## Overview

The idea is to 
 1. Start with a a set of tickers, a begin, and end date that defines a period of time. 
 2. Get a number of random dates from this period of time, and 
 3. Assign the tickers to these dates.
 4. Retrieve historical stock ticker information for (ticker, date) tuples
 5. Arbitrarily assign BOUGHT or SOLD status
 
 
Now we have a dataset to analyze.


## Examples


```r
library(fakereturns)

d <- fake.some.portfolio.data(
     start.date= '2015-01-01', 
	 end.date=Sys.Date(), 
	 n = 40,
	 tickers = c("AAPL", "NFLX", "GOOG"),
	 pct.bought = 0.5,
	 purchase.ceiling = 5000
	 
	 )



```



## Installation


```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("aaelony/fakereturns", dependencies = TRUE, build_vignettes = FALSE)
```


## TODO

 - [ ] offer ways (e.g. FIFO, LIFO) to match BOUGHT with SOLD
 - [ ] compute IRR
 - [ ] freq distribution of weekdays
 

## Changelog

### Version 0.0.5

Breaking changes, now returns one `data.table`.

 - [X] compute shares sold with dates occuring after the purchase


### Version 0.0.4

adding weekday to generated dates
