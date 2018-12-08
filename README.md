# Car Deregistration Problem

## Trying out something new
- test 1
- test 2

```R
dereg <- fread("car_dereg.csv")
dereg$date <- seq(as.Date("1990/05/01"),by = "month",length.out = nrow(dereg))
