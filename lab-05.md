Lab 04 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Conor Lacey
02-07-23

### Load packages and data

``` r
suppressWarnings(library(tidyverse))
library(dsbox) 
library(ggplot2)
library(psych)
source("haversine.R")
```

``` r
states <- read_csv("data/states.csv")
dn <- dennys
lq <- laquinta
```

### Exercise 1

``` r
dn_ak <- dn %>%
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

Wow. There are only 3 Denny’s locations in Alasksa.

### Exercise 2

``` r
lq_ak <- lq %>%
  filter(state == "AK")
nrow(lq_ak)
```

    ## [1] 2

Alaska is not very popular. There are only 2 La Quintas in Alaska.

### Exercise 3

Now we need to join all Denny’s locations with all La Quinta locations
in Alaska. From the looks of things we will need to calculate 6
distances.

``` r
dn_lq_ak <- full_join(dn_ak, lq_ak, by = "state")
dn_lq_ak
```

    ## # A tibble: 6 × 11
    ##   address.x      city.x state zip.x longi…¹ latit…² addre…³ city.y zip.y longi…⁴
    ##   <chr>          <chr>  <chr> <chr>   <dbl>   <dbl> <chr>   <chr>  <chr>   <dbl>
    ## 1 2900 Denali    Ancho… AK    99503   -150.    61.2 3501 M… "\nAn… 99503   -150.
    ## 2 2900 Denali    Ancho… AK    99503   -150.    61.2 4920 D… "\nFa… 99709   -148.
    ## 3 3850 Debarr R… Ancho… AK    99508   -150.    61.2 3501 M… "\nAn… 99503   -150.
    ## 4 3850 Debarr R… Ancho… AK    99508   -150.    61.2 4920 D… "\nFa… 99709   -148.
    ## 5 1929 Airport … Fairb… AK    99701   -148.    64.8 3501 M… "\nAn… 99503   -150.
    ## 6 1929 Airport … Fairb… AK    99701   -148.    64.8 4920 D… "\nFa… 99709   -148.
    ## # … with 1 more variable: latitude.y <dbl>, and abbreviated variable names
    ## #   ¹​longitude.x, ²​latitude.x, ³​address.y, ⁴​longitude.y

### Exercise 4

There are 6 observations in the dn_lq_ak data set. The names of the
variables are as follows:

``` r
colnames(dn_lq_ak)
```

    ##  [1] "address.x"   "city.x"      "state"       "zip.x"       "longitude.x"
    ##  [6] "latitude.x"  "address.y"   "city.y"      "zip.y"       "longitude.y"
    ## [11] "latitude.y"

### Exercise 5

We can use the mutate function to add a new variable to a data frame
while keeping the existing variables.

### Exercise 6

``` r
dn_lq_ak <- dn_lq_ak %>% mutate(distance = haversine(dn_lq_ak$longitude.x,dn_lq_ak$latitude.x,
                                                      dn_lq_ak$longitude.y,dn_lq_ak$latitude.y))
dn_lq_ak["distance"]
```

    ## # A tibble: 6 × 1
    ##   distance
    ##      <dbl>
    ## 1     2.04
    ## 2   416.  
    ## 3     6.00
    ## 4   414.  
    ## 5   420.  
    ## 6     5.20

### Exercise 7

``` r
dn_lq_ak_mindist <- dn_lq_ak %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))

dn_lq_ak_mindist
```

    ## # A tibble: 3 × 2
    ##   address.x        closest
    ##   <chr>              <dbl>
    ## 1 1929 Airport Way    5.20
    ## 2 2900 Denali         2.04
    ## 3 3850 Debarr Road    6.00

### Exercise 8

``` r
dn_lq_ak_mindist %>% ggplot(aes(x = closest)) +
  geom_histogram(fill = "#115740", color = "#ffc72c", binwidth = 3) +
  labs(title = "Denny's Distance to Closest La Quinta in Alaska",
       x = "Distance (km)",
       y = "Number of Denny's Locations")
```

![](lab-05_files/figure-gfm/dn_lq_ak_mindist-histogram-1.png)<!-- -->

``` r
describe(dn_lq_ak_mindist$closest)
```

    ##    vars n mean  sd median trimmed  mad  min max range  skew kurtosis   se
    ## X1    1 3 4.41 2.1    5.2    4.41 1.19 2.04   6  3.96 -0.32    -2.33 1.21

There is a mean distance of 4.41km from Denny’s to the closest La Quinta
location and a variance of 4.41km. However, with just three data points
this is not a lot of information to go off of.

### Exercise 9

``` r
#Create Texas data
lq_nc <- lq %>%
  filter(state == "NC")

dn_nc <- dn %>%
  filter(state == "NC")

#join datasets
dn_lq_nc <- full_join(dn_nc, lq_nc, by = "state")

#add distance variable
dn_lq_nc <- dn_lq_nc %>% mutate(distance = haversine(dn_lq_nc$longitude.x,dn_lq_nc$latitude.x,
                                                      dn_lq_nc$longitude.y,dn_lq_nc$latitude.y))
#minimum distance
dn_lq_nc_mindist <- dn_lq_nc %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))

#plots
dn_lq_nc_mindist %>% ggplot(aes(x = closest)) +
  geom_histogram(fill = "#4B9CD3", color = "#151515", binwidth = 10) +
  labs(title = "Denny's Distance to Closest La Quinta in North Carolina",
       x = "Distance (km)",
       y = "Number of Denny's Locations")
```

![](lab-05_files/figure-gfm/North_Carolina-1.png)<!-- -->

``` r
#describe data
describe(dn_lq_nc_mindist$closest)
```

    ##    vars  n  mean    sd median trimmed   mad  min    max  range skew kurtosis
    ## X1    1 28 65.44 53.42  53.46   60.72 51.06 1.78 187.94 186.16 0.81    -0.35
    ##      se
    ## X1 10.1

For North Carolina, there is a mean distance of 65.44km from Denny’s to
the closest La Quinta location and a variance of 2853.7km (That’s pretty
big!). There is a bit of a positive skew in this dataset however.

### Exercise 10

``` r
#Create Texas data
lq_tx <- lq %>%
  filter(state == "TX")

dn_tx <- dn %>%
  filter(state == "TX")

#join datasets
dn_lq_tx <- full_join(dn_tx, lq_tx, by = "state")

#add distance variable
dn_lq_tx <- dn_lq_tx %>% mutate(distance = haversine(dn_lq_tx$longitude.x,dn_lq_tx$latitude.x,
                                                      dn_lq_tx$longitude.y,dn_lq_tx$latitude.y))
#minimum distance
dn_lq_tx_mindist <- dn_lq_tx %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))

#plots
dn_lq_tx_mindist %>% ggplot(aes(x = closest)) +
  geom_histogram(fill = "#BF5700", color = "#333F48", binwidth = 1) +
  labs(title = "Denny's Distance to Closest La Quinta in Texas",
       x = "Distance (km)",
       y = "Number of Denny's Locations")
```

![](lab-05_files/figure-gfm/texas-1.png)<!-- -->

``` r
#describe data
describe(dn_lq_tx_mindist$closest)
```

    ##    vars   n mean   sd median trimmed  mad  min   max range skew kurtosis   se
    ## X1    1 200 5.79 8.83   3.37    3.88 4.06 0.02 60.58 60.57 3.37    13.53 0.62

For Texas, there is a mean distance of 5.79km from Denny’s to the
closest La Quinta location and a variance of 78km. There is a large
positive skew in this dataset however. It appears the majority of
Denny’s locations in Texas are less than 10km away from the nearest La
Quinta.

### Exercise 11

``` r
lq_ca <- lq %>%
  filter(state == "CA")

dn_ca <- dn %>%
  filter(state == "CA")

#join datasets
dn_lq_ca <- full_join(dn_ca, lq_ca, by = "state")

#add distance variable
dn_lq_ca <- dn_lq_ca %>% mutate(distance = haversine(dn_lq_ca$longitude.x,dn_lq_ca$latitude.x,
                                                      dn_lq_ca$longitude.y,dn_lq_ca$latitude.y))
#minimum distance
dn_lq_ca_mindist <- dn_lq_ca %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))

#plots
dn_lq_ca_mindist %>% ggplot(aes(x = closest)) +
  geom_histogram(fill = "#2D68C4", color = "#F2A900", binwidth = 3) +
  labs(title = "Denny's Distance to Closest La Quinta in California",
       x = "Distance (km)",
       y = "Number of Denny's Locations")
```

![](lab-05_files/figure-gfm/california-1.png)<!-- -->

``` r
#describe data
describe(dn_lq_ca_mindist$closest)
```

    ##    vars   n  mean    sd median trimmed   mad  min    max  range skew kurtosis
    ## X1    1 403 22.08 33.05   11.9   14.65 10.77 0.02 253.46 253.45 3.55     15.5
    ##      se
    ## X1 1.65

For California, there is a mean distance of 22.08km from Denny’s to the
closest La Quinta location and a variance of 1092.25km (This is huge!).
There is a large positive skew in this dataset however. It appears the
majority of Denny’s locations in California are less than 50km away from
the nearest La Quinta.
