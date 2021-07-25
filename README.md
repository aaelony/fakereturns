# fakereturns

A toy R package to generate _fake but realistic_ data for various analytic purposes.


Very Alpha, not ready for use.



## Overview


## Examples


```r
library(fakereturns)

d <- fake.some.portfolio.data(start.date= '2015-01-01', end.date=Sys.Date(), n = 40)

```



## Installation


```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("aaelony/fakereturns", dependencies = TRUE, build_vignettes = FALSE)
```

