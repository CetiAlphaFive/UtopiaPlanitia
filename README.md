![UtopiaPlanitia](https://github.com/CetiAlphaFive/UtopiaPlanitia/blob/main/Utopia_Planitia_on_Mars_pillars.jpg)

# UtopiaPlanitia

Variable importance, heterogeneity tests, and visualization tools for causal forests built on the [grf](https://grf-labs.github.io/grf/) package.

## Features

- **LOCO variable importance** for causal forests (`cf_loco()`) and ranger models (`loco()`)
- **Omnibus heterogeneity testing** combining calibration, high/low CATE, and RATE tests (`omni_hetero()`)
- **Diagnostic, PDP, rank, and interaction plots** for causal forests
- **S3 methods**: `summary()` and `plot()` for `causal_forest` and `cf_loco` objects

## Installation

You can install UtopiaPlanitia from GitHub with:

```r
# install.packages("pak")
pak::pak("CetiAlphaFive/UtopiaPlanitia")
```

## Quick start

```r
library(grf)
library(UtopiaPlanitia)

set.seed(1995)
n <- 500; p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", 1:p)
W <- rbinom(n, 1, 0.5)
Y <- X[, 1] * W + rnorm(n)

cf <- causal_forest(X, Y, W)

# S3 summary: ATE, variable importance, heterogeneity tests
summary(cf)

# S3 plot: diagnostics (default), or type = "rank", "pdp", "inter"
plot(cf, type = "rank")

# LOCO variable importance
vi <- cf_loco(cf)
summary(vi)
plot(vi)
```
