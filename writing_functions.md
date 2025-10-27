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

    ##  [1]  1.08372686 -1.57711854 -0.35929905 -0.16139412  1.58672835 -1.22339381
    ##  [7] -1.74136244 -0.22770652  0.69055427  0.04030655 -0.69254500  0.48068724
    ## [13] -0.59168351  0.43356093  0.69570709 -0.68213061  1.27770457 -0.67745697
    ## [19] -0.16821886  1.81333356

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

    ##  [1]  1.08372686 -1.57711854 -0.35929905 -0.16139412  1.58672835 -1.22339381
    ##  [7] -1.74136244 -0.22770652  0.69055427  0.04030655 -0.69254500  0.48068724
    ## [13] -0.59168351  0.43356093  0.69570709 -0.68213061  1.27770457 -0.67745697
    ## [19] -0.16821886  1.81333356

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1] -0.169301058 -0.242897576  1.225111231  0.577998332  0.231823946
    ##   [6]  0.397349395 -0.892396216  1.019091300 -0.124858369 -0.822099513
    ##  [11]  0.519608931 -0.503143431  0.540635980  1.564587392  0.518825174
    ##  [16]  1.184015767 -1.264075762 -0.550466629  0.130443283  0.947263140
    ##  [21]  1.449484657  0.092456966 -1.256155066  0.960028522 -0.611483885
    ##  [26] -0.795602490  0.924061322 -0.374408035  1.149576227  0.444263426
    ##  [31]  0.220479640  1.876047510 -0.981341856  0.826502956  0.181744254
    ##  [36]  1.450898896  0.501160171  1.129373821  0.534114188 -1.399000888
    ##  [41] -1.930264324  0.040654087 -1.313774948  0.189456223  0.596108237
    ##  [46] -0.385525500 -2.683603651  0.505296239 -0.553381746 -0.314678591
    ##  [51]  0.071387353  0.401947454 -0.169236302 -0.655682667  0.676845094
    ##  [56]  0.012014766 -0.653753469  0.001502086  0.518712751 -0.063848505
    ##  [61] -1.328980727  0.437077867  0.561056458 -0.622043940 -0.355022785
    ##  [66] -2.717808343 -0.664618204 -1.284650372 -0.691215950  1.477148227
    ##  [71]  0.707105482  0.944370259 -1.382880036 -1.544747613  0.182714537
    ##  [76]  1.651418562  0.671880918 -0.372935028  0.803295765 -0.070762088
    ##  [81]  0.118401430 -0.043783969  0.201160889  0.131733134 -0.067265517
    ##  [86]  1.306663352 -0.042117585  0.329052396  0.180029956 -0.866559363
    ##  [91]  0.054703372 -0.217582136 -0.202501017 -0.012763030  0.141383948
    ##  [96] -0.088876774 -1.026895241 -1.993672055 -1.232028093  0.886382995
    ## [101] -2.429418652  1.592334490 -0.026885423  1.140359520  0.985505644
    ## [106]  0.491173346 -0.420309612 -0.060952994 -1.645370694  0.336815032
    ## [111]  2.392499831 -0.999274156 -0.550283573 -1.913680252 -0.462059684
    ## [116]  1.513134990  0.690843266 -0.756378897 -0.976189282 -1.098844839
    ## [121]  0.401992874 -0.130262136  3.069491279

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
    ## 1  11.0  3.53

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
    ## 1   2.70      1.65

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
    ## 1   47.9      1.77

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
