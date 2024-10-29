Iteration and list cols
================

``` r
library(tidyverse)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Here’s some lists

``` r
l = list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE),
  summary = summary(rnorm(1000))
)

l
```

    ## $vec_numeric
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.7067323853 0.1018980739 0.4934954878 0.8135630446 0.3927752192
    ##   [6] 0.7706306630 0.0821048038 0.9613722689 0.8187464722 0.3932708479
    ##  [11] 0.2400940389 0.2522892253 0.9154589241 0.4975504805 0.5703413254
    ##  [16] 0.9614844671 0.8785090453 0.7329279052 0.5641323833 0.3360978137
    ##  [21] 0.3331771330 0.8182712500 0.9991633957 0.5830034320 0.5163992483
    ##  [26] 0.5054784121 0.6146425949 0.1299503730 0.7542454072 0.4364583490
    ##  [31] 0.2280618844 0.1061347108 0.8283656819 0.4106700406 0.1634977476
    ##  [36] 0.7142093398 0.4849339537 0.1821336693 0.9626892784 0.1223171523
    ##  [41] 0.1146112806 0.0007904477 0.0629188423 0.4949564952 0.1427110084
    ##  [46] 0.6564781377 0.6163043499 0.2622746066 0.4730045497 0.0142076078
    ##  [51] 0.1887374455 0.4888735774 0.6437991930 0.0364873656 0.8962765101
    ##  [56] 0.0526890729 0.6768229434 0.9597284379 0.4539940921 0.2026396822
    ##  [61] 0.5187395462 0.7947783272 0.2749319803 0.2327456311 0.4091535027
    ##  [66] 0.8574373021 0.3352509423 0.2913501058 0.9822718457 0.1771365218
    ##  [71] 0.8943739370 0.0699799920 0.9064398888 0.6642265641 0.3371236653
    ##  [76] 0.1917575151 0.0340531974 0.5984406925 0.0798398219 0.5262999895
    ##  [81] 0.7475607414 0.3350966338 0.6426454473 0.1311927740 0.4562431299
    ##  [86] 0.0358952298 0.1469399864 0.8837584290 0.2839555794 0.5253287216
    ##  [91] 0.4611444406 0.2209302844 0.5644038273 0.8367659946 0.8674863293
    ##  [96] 0.1647425892 0.6698249113 0.7678571206 0.0742335643 0.7780857764
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.875863 -0.642234  0.009632  0.047757  0.754282  3.854392

``` r
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

``` r
l[["mat"]][1, 3]
```

    ## [1] 3

``` r
l[[1]]
```

    ## [1] 1 2 3 4

``` r
l[[4]]
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.875863 -0.642234  0.009632  0.047757  0.754282  3.854392

Make a list that’s hopefully a bit more useful.

``` r
list_norm = 
  list(
    a = rnorm(20, 0, 5),
    b = rnorm(20, 4, 5),
    c = rnorm(20, 0, 10),
    d = rnorm(20, 4, 10)
  )
  
list_norm[["b"]]
```

    ##  [1]  3.6359753 -1.3301460  4.9204066  2.6083093  3.1694024  6.0215376
    ##  [7] -3.5739647  6.6104679  0.7309915 -2.1828346  0.5047395  5.5682338
    ## [13] 12.2474012  2.8732548  3.7923336  3.2958756  8.1522575  9.4930943
    ## [19]  6.2915651 13.1276389

Let’s reuse the function we wrote last time.

``` r
mean_and_sd = function(x) {
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(out_df)
  
}
```

Let’s use the function to take mean and sd of all samples.

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.13  4.32

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.30  4.40

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.80  9.33

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.89  9.89

## Use a for loop

Create output list, and run a for loop

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.13  4.32
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.30  4.40
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.80  9.33
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.89  9.89

## Do the same thing

but with `map` instead

``` r
output = map(list_norm, mean_and_sd)
```

let’s do a couple of other things

``` r
output = 
  map(list_norm, mean_and_sd) |> 
  bind_rows()

output = map_dfr(list_norm, mean_and_sd)

output = map_dbl(list_norm, IQR)
```

## LIST COLUMNS!!!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )

listcol_df
```

    ## # A tibble: 4 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>  
    ## 3 c     <dbl [20]>  
    ## 4 d     <dbl [20]>

``` r
listcol_df |> 
  filter(name %in% c("a", "b"))
```

    ## # A tibble: 2 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>

``` r
listcol_df |> 
  select(-samp)
```

    ## # A tibble: 4 × 1
    ##   name 
    ##   <chr>
    ## 1 a    
    ## 2 b    
    ## 3 c    
    ## 4 d

``` r
listcol_df[["samp"]][["a"]]
```

    ##  [1] -0.1192828  6.0464099  3.0371715 -0.6755775  4.1769237 -0.6092717
    ##  [7] -5.1225732  0.6940039 -3.8752130 -7.0042620  0.8770902 -8.8988513
    ## [13] -3.8481792  3.9365327 -3.0869356 -4.1725316  6.3148026 -0.7067946
    ## [19] -5.6462244 -3.8612972

Compute mean and sd

``` r
mean_and_sd(listcol_df[["samp"]][["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.13  4.32

``` r
mean_and_sd(listcol_df[["samp"]][["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.30  4.40

``` r
map(listcol_df[["samp"]], mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.13  4.32
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.30  4.40
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.80  9.33
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.89  9.89

ADD A LIST COLUMN!!!

``` r
listcol_df |> 
  mutate(
    output = map(samp, mean_and_sd),
    iqr = map_dbl(samp, IQR))
```

    ## # A tibble: 4 × 4
    ##   name  samp         output             iqr
    ##   <chr> <named list> <named list>     <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>  5.37
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>  4.23
    ## 3 c     <dbl [20]>   <tibble [1 × 2]> 10.8 
    ## 4 d     <dbl [20]>   <tibble [1 × 2]> 12.2

``` r
listcol_df |> 
  mutate(
    output = map(samp, mean_and_sd),
    iqr = map_dbl(samp, IQR)) |> 
  select(-samp) |> 
  unnest(output)
```

    ## # A tibble: 4 × 4
    ##   name   mean    sd   iqr
    ##   <chr> <dbl> <dbl> <dbl>
    ## 1 a     -1.13  4.32  5.37
    ## 2 b      4.30  4.40  4.23
    ## 3 c     -2.80  9.33 10.8 
    ## 4 d      5.89  9.89 12.2

### NSDUH

This is a version of our function last time.

``` r
nsduh_table_format = function(html, table_num) {
  
  out_table = 
    html |> 
    html_table() |> 
    nth(table_num) |> 
    slice(-1) |> 
    select(-contains("P Value"))
  
  return(out_table)
  
}
```

We need to import the html, and then extract the correct tables.

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

``` r
nsduh_table_format(html = nsduh_html, table_num = 1)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html, table_num = 4)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 1.66a            1.76             0.60               0.64              
    ##  2 Nort… 1.94a            2.18             0.60               0.66              
    ##  3 Midw… 1.37             1.43             0.48               0.54              
    ##  4 South 1.45b            1.56             0.53               0.57              
    ##  5 West  2.03             2.05             0.82               0.85              
    ##  6 Alab… 1.23             1.22             0.42               0.41              
    ##  7 Alas… 1.54a            2.00             0.51               0.65              
    ##  8 Ariz… 2.25             2.29             1.01               0.85              
    ##  9 Arka… 0.93             1.07             0.41               0.48              
    ## 10 Cali… 2.14             2.16             0.89               0.94              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html, table_num = 5)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 0.30             0.33             0.12               0.10              
    ##  2 Nort… 0.43a            0.54             0.13               0.13              
    ##  3 Midw… 0.30             0.31             0.11               0.10              
    ##  4 South 0.27             0.26             0.12               0.08              
    ##  5 West  0.25             0.29             0.13               0.11              
    ##  6 Alab… 0.22             0.27             0.10               0.08              
    ##  7 Alas… 0.70a            1.23             0.11               0.08              
    ##  8 Ariz… 0.32a            0.55             0.17               0.20              
    ##  9 Arka… 0.19             0.17             0.10               0.07              
    ## 10 Cali… 0.20             0.20             0.13               0.09              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_df =
  tibble(
    drug = c("marj", "cocaine", "herion"),
    table_n = c(1, 4, 5)
  ) |> 
  mutate(table = map(table_n, nsduh_table_format, html = nsduh_html)) |> 
  unnest(table)

nsduh_df =
  tibble(
    drug = c("marj", "cocaine", "herion"),
    table_n = c(1, 4, 5)
  ) |> 
  mutate(table = 
           map(table_n, \(x) nsduh_table_format(html = nsduh_html, table_num = x))) |> 
  unnest(table)


nsduh_df |> 
  filter(State == "New York")
```

    ## # A tibble: 3 × 13
    ##   drug    table_n State    `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)`
    ##   <chr>     <dbl> <chr>    <chr>            <chr>            <chr>             
    ## 1 marj          1 New York 14.24b           15.04            13.94             
    ## 2 cocaine       4 New York 2.28             2.54             0.71              
    ## 3 herion        5 New York 0.38a            0.52             0.13              
    ## # ℹ 7 more variables: `12-17(2014-2015)` <chr>, `18-25(2013-2014)` <chr>,
    ## #   `18-25(2014-2015)` <chr>, `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>

### Weather data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/jeffgoldsmith/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-09-28 10:20:04.003054 (8.524)

    ## file min/max dates: 1869-01-01 / 2023-09-30

    ## using cached file: /Users/jeffgoldsmith/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2023-09-28 10:20:09.885503 (3.83)

    ## file min/max dates: 1949-10-01 / 2023-09-30

    ## using cached file: /Users/jeffgoldsmith/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2023-09-28 10:20:11.8066 (0.994)

    ## file min/max dates: 1999-09-01 / 2023-09-30

Create a list column.

``` r
weather_nest = 
  weather_df |> 
  nest(data = date:tmin)
```

``` r
weather_nest[["data"]][[1]]
```

    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows

Let’s try regressing tmax on tmin.

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

``` r
weather_nest |> 
  mutate(model_fit = map(data, \(x) lm(tmax ~ tmin, data = x))) 
```

    ## # A tibble: 3 × 4
    ##   name           id          data               model_fit
    ##   <chr>          <chr>       <list>             <list>   
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>     
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>     
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>
