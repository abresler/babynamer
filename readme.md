babynamer
================

[![Travis build
status](https://travis-ci.com/abresler/babynamer.svg?branch=master)](https://travis-ci.com/abresler/babynamer)

Real-time access to all United States baby-name data.

### Install

``` r
remotes::install_github("abresler/babynamer")
```

### Usage

  - `us_baby_names()`: Provides access to U.S. baby name data for
    specified dataset(s)

### Data

  - National baby names:

<!-- end list -->

``` r
us_baby_names(type = c("national"))
```

  - State baby names:

<!-- end list -->

``` r
us_baby_names(type = c("state"))
```

  - Territorial baby names:

<!-- end list -->

``` r
us_baby_names(type = c("territory"))
```

  - All baby name data:

<!-- end list -->

``` r
us_baby_names(type = c("national", "state", "territory"))
```
