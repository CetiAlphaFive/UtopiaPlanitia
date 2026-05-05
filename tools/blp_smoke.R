#!/usr/bin/env Rscript
# BLP calibration plot smoke test.
#
# Runs two toy DGPs through omni_hetero() |> plot():
#   (1) Hetero DGP: tau = 0.5 + 1.5 * X1   -> expect large positive beta_diff
#   (2) Null DGP:   tau = 0.4 (constant)   -> expect beta_diff ~ 0, large p
#
# Usage (from package root):
#   Rscript inst/examples/blp_smoke.R
#
# Outputs two PNGs (paths below) and prints the omni_hetero table plus the
# attached BLP payload for each forest.
#
# Constraints: small sim — n=300, num.trees=300. Safe for local poke.

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(grf)
  library(ggplot2)
})

out_hetero <- "/tmp/blp_smoke_hetero.png"
out_null   <- "/tmp/blp_smoke_null.png"

# ---- (1) Hetero DGP -------------------------------------------------------
set.seed(1995)
n <- 500; p <- 10
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", seq_len(p))
W <- rbinom(n, 1, 0.5)
tau <- 0.5 + 1.5 * X[, 1]          # true hetero on X1
Y  <- tau * W + X[, 2] + rnorm(n,0,3)

cf <- causal_forest(X, Y, W)

oh <- suppressWarnings(omni_hetero(cf))
cat("\n=== Hetero DGP: omni_hetero ===\n")
print(oh)

blp <- attr(oh, "blp")
cat("\nBLP payload (hetero):\n")
cat("  length(dr):     ", length(blp$dr), "\n")
cat("  length(tau_oob):", length(blp$tau_oob), "\n")
cat("  beta_mean:      ", blp$beta_mean, "\n")
cat("  beta_diff:      ", blp$beta_diff, "\n")
cat("  se_diff:        ", blp$se_diff, "\n")
cat("  p_diff:         ", blp$p_diff, "\n")

oh |> plot()
# ggsave(out_hetero, p_hetero, width = 7.2, height = 5.2, dpi = 120)
cat("\nsaved:", out_hetero, "\n")

# Pipe-chain sanity (matches requested API)
p_pipe <- omni_hetero(cf) |> plot()
cat("pipe plot class:", paste(class(p_pipe), collapse = ","), "\n")

# ---- (2) Null DGP ---------------------------------------------------------
set.seed(2)
Y0 <- 0.4 * W + rnorm(n)
cf0 <- causal_forest(X, Y0, W, num.trees = 300, seed = 2)

oh0 <- suppressWarnings(omni_hetero(cf0))
cat("\n=== Null DGP: omni_hetero ===\n")
print(oh0)

blp0 <- attr(oh0, "blp")
cat("\nBLP payload (null):\n")
cat("  beta_diff:", blp0$beta_diff,
    " se_diff:", blp0$se_diff,
    " p_diff:",  blp0$p_diff, "\n")

p_null <- oh0 |> plot()
ggsave(out_null, p_null, width = 7.2, height = 5.2, dpi = 120)
cat("saved:", out_null, "\n")

# ---- Poke knobs -----------------------------------------------------------
# Uncomment to try options interactively:
   oh |> plot(bins = 50)                 # more bins
   oh |> plot(point_alpha = 0.6)         # darker points
   oh |> plot(bins = 3, point_alpha = 0.1)
