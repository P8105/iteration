Writing Functions
================

Load key packages.

``` r
library(tidyverse)
library(readxl)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Start small!

Everyone loves z scores.

``` r
x_vec = rnorm(20, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.98726117  1.05745929 -1.19877632  0.76841325 -0.18226514 -1.20715183
    ##  [7] -1.18570835  0.78477837  1.28985147 -1.38784280  1.10595634 -0.58803496
    ## [13] -0.02061279  0.08714187 -0.59241246  0.39194666  1.71409073 -1.65326371
    ## [19] -0.15624034 -0.01459047

Write a function to compute z scores.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("The input x should be numeric")
  }
  
  if (length(x) < 5) {
    stop("Only compute z scores when the input has 5 or more numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
  
}
```

Let’s try our function …

``` r
z_scores(x = x_vec)
```

    ##  [1]  0.98726117  1.05745929 -1.19877632  0.76841325 -0.18226514 -1.20715183
    ##  [7] -1.18570835  0.78477837  1.28985147 -1.38784280  1.10595634 -0.58803496
    ## [13] -0.02061279  0.08714187 -0.59241246  0.39194666  1.71409073 -1.65326371
    ## [19] -0.15624034 -0.01459047

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1] -0.14581508 -1.10033311  0.15755085  0.20837295  0.80019953 -0.75790479
    ##   [7]  1.34021735  0.30598932  0.82695811  0.10675092 -1.03447958  1.11755981
    ##  [13]  1.17012395 -0.03323578 -1.23372980  0.45941922  0.01024413  1.13357679
    ##  [19] -0.09410165 -0.20070651 -0.86604026 -1.76484329 -2.37532858 -0.57402513
    ##  [25] -1.96489566 -0.78298847  0.38035568 -0.26203932  1.34349516 -0.78501517
    ##  [31]  0.01712314 -0.36857294 -0.49751945 -0.55750717  0.23674387  0.23628320
    ##  [37]  1.23849879  0.66377390  0.46438919 -0.23127297  1.59468450  0.95030943
    ##  [43] -0.10286494  1.15194704  0.65533441 -0.68067487 -0.55990439  0.56767073
    ##  [49]  0.02663225 -0.55428871 -1.07808044  1.47854144 -1.12136028  0.12670179
    ##  [55]  1.22859516  1.84165922 -0.11943098  1.40255566  0.26645683  1.50675820
    ##  [61]  1.39653773 -0.81464882 -0.59947110 -1.32363946 -0.60888508 -0.97180785
    ##  [67] -0.85379610  1.87842160 -1.72687520 -0.56543322 -0.85135000 -0.14848768
    ##  [73]  0.85652877  0.50655497  0.30653791 -0.08933924 -0.32485849 -0.25281439
    ##  [79]  1.45488280 -0.19581120 -0.35355588  0.14566098 -1.79502105 -0.49737112
    ##  [85]  1.95435727 -0.87700075  0.13218370  0.03791544  0.87649152 -1.04959907
    ##  [91]  0.14712436 -0.45730118  0.70771147 -0.83175141  0.29591277  1.85186010
    ##  [97] -0.14615644 -0.27859701  0.28183664 -0.14771787 -1.66696506  1.27329700
    ## [103] -0.50925260  1.50259394 -0.50040129  1.13024088 -0.90952733  0.19426905
    ## [109] -0.11306664  1.89089983 -0.53759802 -1.59472235 -3.14596835  1.21178297
    ## [115]  1.68892869 -0.56599681  0.04837596 -1.20281108 -1.00766180 -0.22057045
    ## [121]  1.82902599 -0.62817907 -0.40443510

Let’s break our function …

``` r
z_scores(3)
```

    ## Error in z_scores(3): Only compute z scores when the input has 5 or more numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): The input x should be numeric

## Let’s compute some stuff

Let’s compute and return the mean and sd of a numeric vector.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("The input x should be numeric")
  }
  
  if (length(x) < 5) {
    stop("Only compute mean and sd when the input has 5 or more numbers")
  }
  
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  
  tibble(
    mean = mean_x, 
    sd = sd_x
  )
  
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.59  3.09

## Make up data ..

Let’s *simulate* some data

``` r
sim_df = 
  tibble(
    x = rnorm(n = 30, mean = 3, sd = 2)
  )

sim_df |> 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.53      1.93

Write a function to do simulations. The inputs are

- `n_subj` is number of subjects
- `mu` is the true mean
- `sigma` is the true sd

Function simulates data from a normal and computes sample mean and sd.

``` r
sim_mean_sd = function(n_subj, mu = 3, sigma = 2) {
  
  sim_df = 
    tibble(
      x = rnorm(n = n_subj, mean = mu, sd = sigma)
    )
  
  sim_df |> 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
  
}
```

Let’s run this function.

``` r
sim_mean_sd(mu = 48, 50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   48.3      2.16

Import the LoTR data

``` r
fellowship_ring = 
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") |> 
  mutate(movie = "Fellowship of the Ring")

two_towers = 
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") |> 
  mutate(movie = "Two Towers")

return_of_the_king = 
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") |> 
  mutate(movie = "Return of the King")

lotr_df = 
  bind_rows(fellowship_ring, two_towers, return_of_the_king)
```

Turn this into a function

``` r
lotr_import = function(cell_range, movie_title) {
  
  df = 
    read_excel("data/LotR_Words.xlsx", range = cell_range) |> 
    mutate(movie = movie_title)
  
  df
  
}

fellowship = lotr_import(cell_range = "B3:D6", movie_title = "Fellowship")
two_towers = lotr_import(cell_range = "F3:H6", movie_title = "Two Towers")
return =     lotr_import(cell_range = "J3:L6", movie_title = "Return")

bind_rows(fellowship, two_towers, return)
```

    ## # A tibble: 9 × 4
    ##   Race   Female  Male movie     
    ##   <chr>   <dbl> <dbl> <chr>     
    ## 1 Elf      1229   971 Fellowship
    ## 2 Hobbit     14  3644 Fellowship
    ## 3 Man         0  1995 Fellowship
    ## 4 Elf       331   513 Two Towers
    ## 5 Hobbit      0  2463 Two Towers
    ## 6 Man       401  3589 Two Towers
    ## 7 Elf       183   510 Return    
    ## 8 Hobbit      2  2673 Return    
    ## 9 Man       268  2459 Return

Look at one more example.

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj_year = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))

data_marj_month = 
  nsduh_html |> 
  html_table() |> 
  nth(2) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))

data_marj_first = 
  nsduh_html |> 
  html_table() |> 
  nth(3) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

write an import function

``` r
nsduh_import = function(html, table_num) {
  
  data = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent)) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  data
  
}

nsduh_import(nsduh_html, table_num = 1)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    9.98
    ##  2 Alabama 12+   2014-2015    9.6 
    ##  3 Alabama 12-17 2013-2014    9.9 
    ##  4 Alabama 12-17 2014-2015    9.71
    ##  5 Alabama 18-25 2013-2014   27.0 
    ##  6 Alabama 18-25 2014-2015   26.1 
    ##  7 Alabama 26+   2013-2014    7.1 
    ##  8 Alabama 26+   2014-2015    6.81
    ##  9 Alabama 18+   2013-2014    9.99
    ## 10 Alabama 18+   2014-2015    9.59
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html, table_num = 2)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    5.57
    ##  2 Alabama 12+   2014-2015    5.35
    ##  3 Alabama 12-17 2013-2014    4.98
    ##  4 Alabama 12-17 2014-2015    5.16
    ##  5 Alabama 18-25 2013-2014   15.0 
    ##  6 Alabama 18-25 2014-2015   14.3 
    ##  7 Alabama 26+   2013-2014    4.03
    ##  8 Alabama 26+   2014-2015    3.86
    ##  9 Alabama 18+   2013-2014    5.63
    ## 10 Alabama 18+   2014-2015    5.37
    ## # ℹ 500 more rows

don’t forget documentation
