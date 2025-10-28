Iteration and List Columns
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

## Make a list

``` r
l = 
  list(
    vec_numeric = 1:23,
    char_vec = c("Jeff"), 
    mat = matrix(1:8, nrow = 2, ncol = 4), 
    summary = summary(rnorm(1000, mean = 4))
  )

l
```

    ## $vec_numeric
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
    ## 
    ## $char_vec
    ## [1] "Jeff"
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.963   3.332   4.000   4.012   4.708   6.813

``` r
l[[1]]
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

``` r
l[["vec_numeric"]]
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

``` r
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

## Make a different list

``` r
list_normals = 
  list(
    a = rnorm(30, mean = 3, sd = 1),
    b = rnorm(30, mean = 30, sd = 1),
    c = rnorm(30, mean = 3, sd = 10),
    d = rnorm(30, mean = -3, sd = 4)
  )
```

(copy and paste the function from last time)

``` r
source("source/mean_and_sd.R")
```

``` r
mean_and_sd(list_normals[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.84 0.806

``` r
mean_and_sd(list_normals[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.8 0.960

``` r
mean_and_sd(list_normals[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24  9.13

``` r
mean_and_sd(list_normals[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.72  4.10

Use a loop to iterate!!

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_normals[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.84 0.806
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.8 0.960
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24  9.13
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.72  4.10

Use `map` to do the same thing.

``` r
output = map(list_normals, mean_and_sd)

output = map(list_normals, median)
```

Check out some `map` variants

``` r
map_dfr(list_normals, mean_and_sd, .id = "sample")
```

    ## # A tibble: 4 × 3
    ##   sample  mean    sd
    ##   <chr>  <dbl> <dbl>
    ## 1 a       2.84 0.806
    ## 2 b      29.8  0.960
    ## 3 c       2.24 9.13 
    ## 4 d      -2.72 4.10

``` r
map_dbl(list_normals, median)
```

    ##         a         b         c         d 
    ##  2.819250 29.559952  1.423989 -2.632351

## LIST COLUMNS

Try to put my list into a dataframe!!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    sample = list_normals
  )
```

Did this really work??

``` r
pull(listcol_df, name)
```

    ## [1] "a" "b" "c" "d"

``` r
pull(listcol_df, sample)
```

    ## $a
    ##  [1] 3.169788 4.672604 2.513791 2.545189 2.888069 1.086418 3.120455 2.467755
    ##  [9] 4.510111 1.915370 2.202466 3.005129 1.575434 2.948888 3.014087 2.377518
    ## [17] 4.112720 3.509942 2.320257 2.949072 2.058365 3.517572 2.397276 2.750432
    ## [25] 2.241831 3.606912 2.605103 2.223076 3.743305 3.216812
    ## 
    ## $b
    ##  [1] 29.33777 32.66822 29.67040 29.31666 29.04973 29.15334 29.43236 31.63710
    ##  [9] 29.74532 30.58942 28.94419 28.85954 30.41647 30.81192 28.81292 29.94309
    ## [17] 28.04970 29.20236 30.64991 29.61840 29.27492 29.13629 30.08586 29.90128
    ## [25] 31.46367 29.50150 30.59342 29.27924 29.63169 29.40280
    ## 
    ## $c
    ##  [1]  19.7222636   7.9110709  -9.7635396  -4.9791761   5.7291319   1.8608859
    ##  [7]   2.6739446  26.8645339  -3.5460763  -3.6283249 -12.8151965  -4.0288780
    ## [13]   1.3662139  17.7711809   5.1986021  15.2918923   1.0915245  -5.6325859
    ## [19]  -8.0606065  -7.2109003   2.8297227  12.7742729  -2.3160887  -3.8881039
    ## [25]   3.2353172   8.8885634   1.4817647  -0.1567007  -3.4101940   1.8177363
    ## 
    ## $d
    ##  [1] -7.9299364 -2.6399967  4.4370456 -0.4442831  1.9326428 -3.5160829
    ##  [7] -6.3096681 -3.6294789  2.3202164 -2.6247046 -1.0134528 -8.9461562
    ## [13] -2.4454389 -3.4333991  2.5870359 -8.9364698 -0.8101320  6.5167978
    ## [19] -5.2102819 -1.8607170 -9.4639314 -4.7971343 -2.3495328 -3.2423676
    ## [25] -7.1156131 -1.5029430  1.8942100 -4.0049359 -0.1707101 -8.8090992

can I apply `mean_and_sd`??

``` r
mean_and_sd(pull(listcol_df, sample)[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.84 0.806

``` r
mean_and_sd(pull(listcol_df, sample)[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.8 0.960

``` r
mean_and_sd(pull(listcol_df, sample)[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24  9.13

``` r
mean_and_sd(pull(listcol_df, sample)[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.72  4.10

iterate using `map`

``` r
map(pull(listcol_df, sample), mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.84 0.806
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.8 0.960
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24  9.13
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.72  4.10

adding a column …

``` r
listcol_df = 
  listcol_df |> 
  mutate(
    summary = map(sample, mean_and_sd)
  )

pull(listcol_df, summary)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.84 0.806
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  29.8 0.960
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.24  9.13
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.72  4.10

``` r
listcol_df |> 
  select(-sample) |> 
  unnest(summary)
```

    ## # A tibble: 4 × 3
    ##   name   mean    sd
    ##   <chr> <dbl> <dbl>
    ## 1 a      2.84 0.806
    ## 2 b     29.8  0.960
    ## 3 c      2.24 9.13 
    ## 4 d     -2.72 4.10

## Revisit NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

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

Try this with a `for` loop

``` r
output = vector("list", length = 3)

for (i in 1:3) {
  
  output[[i]] = nsduh_import(html = nsduh_html, i)
  
}
```

Do this with `map`

``` r
map(1:3, nsduh_import, html = nsduh_html)
```

    ## [[1]]
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
    ## 
    ## [[2]]
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
    ## 
    ## [[3]]
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

Do this all in a dataframe.

``` r
nsduh_df = 
  tibble(
    name = c("marj year", "marj month", "marj first"),
    number = 1:3
  ) |> 
  mutate(
    table = map(number, nsduh_import, html = nsduh_html)
  ) |> 
  unnest(table)
```

## Look at weather data

``` r
library(p8105.datasets)
data("weather_df")
```

``` r
weather_df |> 
  filter(name == "CentralPark_NY") |> 
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_point() + 
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="iteration_and_listcols_files/figure-gfm/unnamed-chunk-21-1.png" width="90%" />

Let’s do a regression

``` r
weather_df |> 
  filter(name == "CentralPark_NY") |> 
  lm(tmax ~ tmin, data = _)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = filter(weather_df, name == "CentralPark_NY"))
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
weather_df |> 
  filter(name == "Molokai_HI") |> 
  lm(tmax ~ tmin, data = _)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = filter(weather_df, name == "Molokai_HI"))
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

``` r
weather_df |> 
  filter(name == "Waterhole_WA") |> 
  lm(tmax ~ tmin, data = _)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = filter(weather_df, name == "Waterhole_WA"))
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

Let’s iterate differently…

``` r
weather_nest = 
  weather_df |> 
  nest(data = date:tmin)
```

``` r
lm(tmax ~ tmin, data = pull(weather_nest, data)[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = pull(weather_nest, data)[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
lm(tmax ~ tmin, data = pull(weather_nest, data)[[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = pull(weather_nest, data)[[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

``` r
lm(tmax ~ tmin, data = pull(weather_nest, data)[[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = pull(weather_nest, data)[[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

Do this using `map` ..

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}
```

``` r
map(pull(weather_nest, data), weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

``` r
weather_nest |> 
  mutate(
    lm_fits = map(data, weather_lm)
  )
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_fits
    ##   <chr>          <chr>       <list>             <list> 
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>   
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>   
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>
