Lab 04 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Conor Lacey
02-07-23

### Load packages and data

``` r
suppressWarnings(library(tidyverse))
library(dsbox) 
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

Wow. There are only 3 Denny’s in Alaksa.

### Exercise 2

Remove this text, and add your answer for Exercise 1 here. Add code
chunks as needed. Don’t forget to label your code chunk. Do not use
spaces in code chunk labels.

### Exercise 3

…

### Exercise 4

…

### Exercise 5

…

### Exercise 6

…

Add exercise headings as needed.
