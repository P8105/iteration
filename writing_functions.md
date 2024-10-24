Writing Functions
================

Load key packages.

``` r
library(tidyverse)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## writing my first function!!

as an example, here’s a z-score computation

``` r
x_vec = rnorm(n = 25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.2185126 -0.7915703  0.3170631  0.7396091 -0.7842602  0.4128489
    ##  [7] -0.7925710 -1.4438789 -0.6924429 -1.6026936  2.2416331 -0.5873966
    ## [13]  1.3205437  1.2141476  1.3720902 -0.7685086 -0.7225317  0.8088742
    ## [19]  0.3055232 -0.1993189  0.4097886 -1.3109057  0.9536181  0.5981828
    ## [25] -0.7793316

Now i’ll write a function to do this.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 5) {
    stop("you need at least five numbers to compute the z score")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1] -0.2185126 -0.7915703  0.3170631  0.7396091 -0.7842602  0.4128489
    ##  [7] -0.7925710 -1.4438789 -0.6924429 -1.6026936  2.2416331 -0.5873966
    ## [13]  1.3205437  1.2141476  1.3720902 -0.7685086 -0.7225317  0.8088742
    ## [19]  0.3055232 -0.1993189  0.4097886 -1.3109057  0.9536181  0.5981828
    ## [25] -0.7793316

does this always work?

``` r
z_scores(x = 3)
```

    ## Error in z_scores(x = 3): you need at least five numbers to compute the z score

``` r
z_scores(x = c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(x = c("my", "name", "is", "jeff")): x needs to be numeric

``` r
z_scores(x = x_vec)
```

    ##  [1] -0.2185126 -0.7915703  0.3170631  0.7396091 -0.7842602  0.4128489
    ##  [7] -0.7925710 -1.4438789 -0.6924429 -1.6026936  2.2416331 -0.5873966
    ## [13]  1.3205437  1.2141476  1.3720902 -0.7685086 -0.7225317  0.8088742
    ## [19]  0.3055232 -0.1993189  0.4097886 -1.3109057  0.9536181  0.5981828
    ## [25] -0.7793316
