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

    ##  [1] -1.01985001  2.65795877  0.78372499 -0.06544135  0.13791009 -1.10777559
    ##  [7]  0.23076704 -0.60240232 -0.44809210  0.32062818  0.80603025 -1.36324285
    ## [13] -0.73158041  0.53812303 -1.11953838 -0.42919834 -0.40268849  1.62298296
    ## [19]  0.69819352 -0.50650898

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

    ##  [1] -1.01985001  2.65795877  0.78372499 -0.06544135  0.13791009 -1.10777559
    ##  [7]  0.23076704 -0.60240232 -0.44809210  0.32062818  0.80603025 -1.36324285
    ## [13] -0.73158041  0.53812303 -1.11953838 -0.42919834 -0.40268849  1.62298296
    ## [19]  0.69819352 -0.50650898

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1]  0.92946835  0.90008645  0.80453764 -1.66862863 -1.91216104  0.42385174
    ##   [7]  2.43082762 -1.75375134  0.39869668  1.21748125  0.11021634  0.03273391
    ##  [13]  0.46315369 -0.44133192 -0.76560534  0.86488716  0.24714293  0.30990354
    ##  [19] -0.81083702 -2.62255090  0.95629425 -0.37746656 -0.91064174  0.59886505
    ##  [25]  1.15795067 -0.70086452  0.15369259 -1.20095288  0.42927154 -0.86270867
    ##  [31] -0.96424654  0.12852631  0.48049831 -0.34360909  1.24430957 -0.15023204
    ##  [37] -1.04160810  0.61113248 -1.15134132 -0.33727740  0.62765076 -0.30170368
    ##  [43]  0.72494498  2.21700842 -0.20225226 -0.06549913  0.59170193  0.61893719
    ##  [49] -1.43894933  0.66061480  1.32499672  1.15898320  0.05685033 -0.16847979
    ##  [55] -1.73319640 -2.50717662  0.47942653  0.47541806  1.19156547 -1.01045186
    ##  [61]  2.28257737  0.32997661  1.45763032 -0.18622398  1.32765985 -0.67686376
    ##  [67] -1.01619529 -0.77618624  0.73235085 -1.35295188  2.00793604 -0.12376057
    ##  [73]  0.92022558 -1.09116829 -0.85581513  0.28397596  0.05258344  0.41802166
    ##  [79]  0.53035379  0.16497707  0.14779776  0.57904532 -0.00814974 -0.91571654
    ##  [85]  0.21886200  0.78144648 -0.54071049 -0.40053889 -1.60810052 -1.50188673
    ##  [91]  0.12810726  1.25951310 -0.07429731  0.78121973  1.14211323  1.02693180
    ##  [97] -0.23153427  1.13811636 -0.96784937 -0.13560005 -0.01608372 -0.09885458
    ## [103] -2.37675976  0.93749665 -1.14984882 -2.50856900  0.85041660 -0.28823195
    ## [109]  0.11081565  0.04950146  0.43629614  0.20917178  0.03258291  0.05884296
    ## [115] -0.91919758 -0.91035578  0.59728198 -1.18962305  1.18817486  0.14974664
    ## [121]  0.43964807 -0.81727660  0.38885030

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

    ## #' mean_and_asd
    ## #' 
    ## #' Takes a numeric input vector and computes the mean and stnadard deviation
    ## #'
    ## #' @param x the input vector (must be numeric with 5 or more elements)
    ## #'
    ## #' @returns a dataframe containing the mean and sd of x
    ## #' @export
    ## #'
    ## #' @examples
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
    ## 1  10.4  3.12

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
    ## 1   2.76      2.31

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
    ## 1   48.2      2.16

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
