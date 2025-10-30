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

    ##  [1]  1.52763720  1.29337583 -0.43380201 -0.03812632  1.23765687  0.22494032
    ##  [7]  0.65536987 -0.21594368  0.28684815 -0.73119925 -0.62017777 -2.28616748
    ## [13]  0.26561004  0.38380105 -1.92511137  0.68792867  0.27934591 -0.45121534
    ## [19]  0.88578570 -1.02655639

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

    ##  [1]  1.52763720  1.29337583 -0.43380201 -0.03812632  1.23765687  0.22494032
    ##  [7]  0.65536987 -0.21594368  0.28684815 -0.73119925 -0.62017777 -2.28616748
    ## [13]  0.26561004  0.38380105 -1.92511137  0.68792867  0.27934591 -0.45121534
    ## [19]  0.88578570 -1.02655639

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1] -0.44010658 -1.83718569 -1.27400517  1.02106527  0.10208095 -0.26559467
    ##   [7]  1.27667585 -0.66080802 -0.53585512 -0.33533281 -0.18089876 -0.28735693
    ##  [13]  1.00316159  1.57367073 -0.79636268 -0.20965171 -1.04694726 -0.46335684
    ##  [19]  1.82662775 -0.03043349  0.37247412 -0.42787384 -0.22855052 -0.80703430
    ##  [25]  0.81525176 -1.53045891 -1.66662073 -1.55513361 -1.13542516  0.97290296
    ##  [31]  0.28868683  0.59248341  1.46049380  0.56006956  0.87174944  0.18015050
    ##  [37] -2.89203537  0.38802802 -0.10379412  0.58107311  0.13544533 -0.91038455
    ##  [43] -0.84007374  1.40815004  0.22473341 -0.67893100  0.33914181 -1.00107920
    ##  [49]  2.11142310 -0.65025888  0.28893367 -0.31092692 -0.97981010 -0.23600757
    ##  [55] -1.61948429  0.40848167 -0.05466475  1.14965446  0.07156284 -2.04764565
    ##  [61] -0.32879047 -2.04978004  1.00909486  0.86262532  1.35120586 -0.16529678
    ##  [67]  0.66678051 -0.97697875  0.84687562  0.41920610 -0.61100744 -1.17302456
    ##  [73]  0.44530798 -1.53527861 -0.33860863  1.07576829 -0.69062871  0.71297292
    ##  [79] -1.06216899  1.61101472  1.54785953 -0.94431626  0.90621049  1.12229838
    ##  [85]  1.06792698 -0.56193852  0.28409332  0.52018818  0.82794950  0.14655367
    ##  [91]  1.03778188  0.03192677  1.58655408 -1.39876777  0.54199664  0.38691675
    ##  [97] -0.05619324  0.11672578  1.31645671 -1.93097457 -1.24778493  0.02848395
    ## [103] -0.58052283  1.51028699  1.01026412  1.01149617  0.82082565  0.94018703
    ## [109]  1.97964232  1.33857882  0.57075065 -0.82613961  0.57239348 -0.96206189
    ## [115] -1.63701082 -0.35103878 -0.66780725 -0.66565356  0.06631187 -0.34340025
    ## [121] -0.16640498  0.12448719 -1.12850383

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

I’ve written that function and saved it to `source/mean_and_sd.R`, along
with some documentation. For completeness, I’m printing that R script
below.

    ## #' mean_and_sd
    ## #' 
    ## #' Takes a numeric input vector and computes the mean and standard deviation
    ## #'
    ## #' @param x the input vector (must be numeric with 5 or more elements)
    ## #'
    ## #' @returns a dataframe containing the mean and sd of x
    ## #' 
    ## mean_and_sd = function(x) {
    ##   
    ##   if (!is.numeric(x)) {
    ##     stop("The input x should be numeric")
    ##   }
    ##   
    ##   if (length(x) < 5) {
    ##     stop("Only compute mean and sd when the input has 5 or more numbers")
    ##   }
    ##   
    ##   mean_x = mean(x, na.rm = TRUE)
    ##   sd_x = sd(x, na.rm = TRUE)
    ##   
    ##   tibble(
    ##     mean = mean_x, 
    ##     sd = sd_x
    ##   )
    ##   
    ## }

This will import the function for use here.

``` r
source("source/mean_and_sd.R")
```

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.4  3.04

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
    ## 1   3.26      2.23

Write a function to do simulations. The inputs are

- `n_subj` is number of subjects
- `mu` is the true mean
- `sigma` is the true sd

Function simulates data from a normal and computes sample mean and sd.

(We wrote this in a code chunk last time; now it’s being sourced.)

``` r
source("source/sim_mean_sd.R")
```

Let’s run this function.

``` r
sim_mean_sd(mu = 48, 50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   48.1      2.21

``` r
sim_mean_sd(n_subj = 30)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.04      2.01

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

``` r
nsduh_import(nsduh_html, table_num = 3)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    1.42
    ##  2 Alabama 12+   2014-2015    1.49
    ##  3 Alabama 12-17 2013-2014    4.46
    ##  4 Alabama 12-17 2014-2015    4.36
    ##  5 Alabama 18-25 2013-2014    6.04
    ##  6 Alabama 18-25 2014-2015    6.39
    ##  7 Alabama 26+   2013-2014    0.15
    ##  8 Alabama 26+   2014-2015    0.2 
    ##  9 Alabama 18+   2013-2014    0.95
    ## 10 Alabama 18+   2014-2015    1.05
    ## # ℹ 500 more rows
