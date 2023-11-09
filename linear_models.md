Linear Models
================
Luan Mengxiao
2023-11-09

Load key packages.

``` r
library(tidyverse)
library(p8105.datasets)
```

## Load and clean the Airbnb data

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb |> 
  mutate(stars = review_scores_location / 2) |> 
  select(price, stars, borough = neighbourhood_group, 
         neighbourhood, room_type) |>
  filter(borough != "Staten Island")
```

Let’s fit a model.

``` r
fit = 
  nyc_airbnb |>
  lm(price ~ stars + borough, data = _)
fit
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Coefficients:
    ##      (Intercept)             stars   boroughBrooklyn  boroughManhattan  
    ##           -70.41             31.99             40.50             90.25  
    ##    boroughQueens  
    ##            13.21

Let’s look at the `fit`.

tidy up the output instead

``` r
fit |>
  broom::glance()
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

tidy up the coefficients

``` r
fit |>
  broom::tidy() |>
  mutate(term = str_replace(term, "^borough", "Borough: ")) |>
  select(term, estimate, p.value) |>
  knitr::kable(digits = 3)
```

| term               | estimate | p.value |
|:-------------------|---------:|--------:|
| (Intercept)        |  -70.414 |   0.000 |
| stars              |   31.990 |   0.000 |
| Borough: Brooklyn  |   40.500 |   0.000 |
| Borough: Manhattan |   90.254 |   0.000 |
| Borough: Queens    |   13.206 |   0.145 |

## Fit another model

``` r
fit = 
  nyc_airbnb |>
  mutate(
    borough = fct_infreq(borough),
    room_type = fct_infreq(room_type)
  ) |>
  lm(price ~ stars + borough + room_type, data = _)

fit |>
  broom::tidy()
```

    ## # A tibble: 7 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              113.      11.8       9.54 1.56e-21
    ## 2 stars                     21.9      2.43      9.01 2.09e-19
    ## 3 boroughBrooklyn          -40.3      2.15    -18.8  4.62e-78
    ## 4 boroughQueens            -55.5      3.59    -15.4  1.32e-53
    ## 5 boroughBronx             -63.0      8.22     -7.67 1.76e-14
    ## 6 room_typePrivate room   -105.       2.05    -51.2  0       
    ## 7 room_typeShared room    -129.       6.15    -21.0  2.24e-97

## Quick look at diagnostics

``` r
nyc_airbnb |>
  modelr::add_residuals(fit) |>
  ggplot(aes(x = resid)) +
  geom_density() +
  xlim(-100, 500)
```

    ## Warning: Removed 11681 rows containing non-finite values (`stat_density()`).

![](linear_models_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
nyc_airbnb |>
  modelr::add_residuals(fit) |>
  ggplot(aes(x = borough, y = resid)) +
  geom_violin()
```

    ## Warning: Removed 9962 rows containing non-finite values (`stat_ydensity()`).

![](linear_models_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
nyc_airbnb |>
  modelr::add_residuals(fit) |>
  ggplot(aes(x = stars, y = resid)) +
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (`geom_point()`).

![](linear_models_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

## Hypothesis test for categorical predictor

fit a “null” and “alternative” model

``` r
fit |>
  broom::tidy()
```

    ## # A tibble: 7 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              113.      11.8       9.54 1.56e-21
    ## 2 stars                     21.9      2.43      9.01 2.09e-19
    ## 3 boroughBrooklyn          -40.3      2.15    -18.8  4.62e-78
    ## 4 boroughQueens            -55.5      3.59    -15.4  1.32e-53
    ## 5 boroughBronx             -63.0      8.22     -7.67 1.76e-14
    ## 6 room_typePrivate room   -105.       2.05    -51.2  0       
    ## 7 room_typeShared room    -129.       6.15    -21.0  2.24e-97

``` r
fit_null = lm(price ~ stars + borough, data = nyc_airbnb)
fit_alternative = lm(price ~ stars + borough + room_type, data = nyc_airbnb)

anova(fit_null, fit_alternative) |>
  broom::tidy()
```

    ## # A tibble: 2 × 7
    ##   term                        df.residual    rss    df   sumsq statistic p.value
    ##   <chr>                             <dbl>  <dbl> <dbl>   <dbl>     <dbl>   <dbl>
    ## 1 price ~ stars + borough           30525 1.01e9    NA NA            NA       NA
    ## 2 price ~ stars + borough + …       30523 9.21e8     2  8.42e7     1394.       0

## Borough-level differences

``` r
fit =
  nyc_airbnb |>
  lm(price ~ stars * borough + room_type * borough, data = _)

fit |>
  broom::tidy()
```

    ## # A tibble: 16 × 5
    ##    term                                   estimate std.error statistic   p.value
    ##    <chr>                                     <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                               90.1       75.4    1.19   0.232    
    ##  2 stars                                      4.45      16.6    0.267  0.789    
    ##  3 boroughBrooklyn                          -20.4       77.1   -0.265  0.791    
    ##  4 boroughManhattan                           5.63      77.8    0.0723 0.942    
    ##  5 boroughQueens                              1.51      83.5    0.0181 0.986    
    ##  6 room_typePrivate room                    -52.9       17.8   -2.98   0.00288  
    ##  7 room_typeShared room                     -70.5       41.6   -1.70   0.0896   
    ##  8 stars:boroughBrooklyn                     16.5       17.0    0.973  0.331    
    ##  9 stars:boroughManhattan                    22.7       17.1    1.33   0.185    
    ## 10 stars:boroughQueens                        5.21      18.3    0.285  0.776    
    ## 11 boroughBrooklyn:room_typePrivate room    -39.3       18.0   -2.18   0.0292   
    ## 12 boroughManhattan:room_typePrivate room   -71.3       18.0   -3.96   0.0000754
    ## 13 boroughQueens:room_typePrivate room      -16.3       19.0   -0.859  0.390    
    ## 14 boroughBrooklyn:room_typeShared room     -35.3       42.9   -0.822  0.411    
    ## 15 boroughManhattan:room_typeShared room    -83.1       42.5   -1.96   0.0503   
    ## 16 boroughQueens:room_typeShared room       -24.4       44.4   -0.550  0.582

``` r
airbnb_lm = function(df){
  lm(price ~ stars + room_type, data = df)
}

nyc_airbnb |>
  nest(df = -borough) |>
  mutate(
    models = map(df, airbnb_lm), 
    results = map(models, broom::tidy)
  ) |>
  select(borough, results) |>
  unnest(results) |>
  select(borough, term, estimate) |>
  pivot_wider(
    names_from = term,
    values_from = estimate
  ) |>
  knitr::kable(digits = 3)
```

| borough   | (Intercept) |  stars | room_typePrivate room | room_typeShared room |
|:----------|------------:|-------:|----------------------:|---------------------:|
| Bronx     |      90.067 |  4.446 |               -52.915 |              -70.547 |
| Queens    |      91.575 |  9.654 |               -69.255 |              -94.973 |
| Brooklyn  |      69.627 | 20.971 |               -92.223 |             -105.839 |
| Manhattan |      95.694 | 27.110 |              -124.188 |             -153.635 |

same thing but just a little different

``` r
nyc_airbnb |>
  nest(df = -borough) |>
  mutate(
    models = map(df, \(df) lm(price ~ stars + room_type, data = df)), 
    results = map(models, broom::tidy)
  ) |>
  select(borough, results) |>
  unnest(results) |>
  select(borough, term, estimate) |>
  pivot_wider(
    names_from = term,
    values_from = estimate
  ) |>
  knitr::kable(digits = 3)
```

| borough   | (Intercept) |  stars | room_typePrivate room | room_typeShared room |
|:----------|------------:|-------:|----------------------:|---------------------:|
| Bronx     |      90.067 |  4.446 |               -52.915 |              -70.547 |
| Queens    |      91.575 |  9.654 |               -69.255 |              -94.973 |
| Brooklyn  |      69.627 | 20.971 |               -92.223 |             -105.839 |
| Manhattan |      95.694 | 27.110 |              -124.188 |             -153.635 |

## Homicides in Baltimore

``` r
baltimore_df = 
  read_csv("data/homicide-data.csv") |>
  filter(city == "Baltimore") |>
  mutate(
    resolved = as.numeric(disposition == "Closed by arrest"),
    victim_age = as.numeric(victim_age)
  ) |>
  select(resolved, victim_age, victim_race, victim_sex)
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

fitting a logistic regression

``` r
fit_logistic = 
  baltimore_df |>
  glm(
    resolved ~ victim_age + victim_race + victim_sex, 
    data = _, 
    family = binomial())
fit_logistic
```

    ## 
    ## Call:  glm(formula = resolved ~ victim_age + victim_race + victim_sex, 
    ##     family = binomial(), data = baltimore_df)
    ## 
    ## Coefficients:
    ##         (Intercept)           victim_age     victim_raceBlack  
    ##             1.48640             -0.00724             -1.13811  
    ## victim_raceHispanic     victim_raceOther     victim_raceWhite  
    ##            -0.56162             -1.06445             -0.29635  
    ##      victim_sexMale  
    ##            -0.87966  
    ## 
    ## Degrees of Freedom: 2826 Total (i.e. Null);  2820 Residual
    ## Null Deviance:       3676 
    ## Residual Deviance: 3589  AIC: 3603

look at model results

``` r
fit_logistic |>
  broom::tidy() |>
  mutate(OR = exp(estimate)) |>
  select(term, estimate, OR)
```

    ## # A tibble: 7 × 3
    ##   term                estimate    OR
    ##   <chr>                  <dbl> <dbl>
    ## 1 (Intercept)          1.49    4.42 
    ## 2 victim_age          -0.00724 0.993
    ## 3 victim_raceBlack    -1.14    0.320
    ## 4 victim_raceHispanic -0.562   0.570
    ## 5 victim_raceOther    -1.06    0.345
    ## 6 victim_raceWhite    -0.296   0.744
    ## 7 victim_sexMale      -0.880   0.415

``` r
baltimore_df |>
  count(victim_race)
```

    ## # A tibble: 5 × 2
    ##   victim_race     n
    ##   <chr>       <int>
    ## 1 Asian          11
    ## 2 Black        2596
    ## 3 Hispanic       57
    ## 4 Other           6
    ## 5 White         157
