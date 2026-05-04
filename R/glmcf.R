#' Causal Forest with cv.glmnet-Estimated Nuisances
#'
#' Refits a `grf::causal_forest()` after replacing the conditional outcome
#' `m(x) = E[Y | X = x]` and propensity `e(x) = E[W | X = x]` with
#' cross-fitted predictions from `glmnet::cv.glmnet()` (penalized
#' elastic-net regression / classification, optionally relaxed). The CATE
#' estimator and identification assumptions of `grf` are unchanged --- only
#' the Robinson-residualisation nuisances are swapped.
#'
#' @param c.forest A fitted causal forest object from the \pkg{grf} package,
#'   used both to recover `(X, Y, W)` (via `c.forest$X.orig`,
#'   `c.forest$Y.orig`, `c.forest$W.orig`) and to inherit tunable
#'   hyperparameters for the refit.
#' @param X,Y,W Optional overrides for the design matrix, outcome vector,
#'   and treatment vector. If `NULL` (default) they are recovered from
#'   `c.forest`.
#' @param K Integer >= 2. Number of folds for cross-fitting the cv.glmnet
#'   nuisances. Default `5`.
#' @param seed Integer seed controlling fold assignment, the inner
#'   cv.glmnet fold ids, and the downstream `grf::causal_forest()` call.
#'   Default `1995`.
#' @param eps Numeric in `(0, 0.5)`. Propensity clipping bound for binary
#'   `W`: cv.glmnet binomial predictions are clipped to `[eps, 1 - eps]`
#'   to preserve overlap. Default `1e-3`.
#' @param tuning Character scalar controlling how `grf::causal_forest()`
#'   tunable hyperparameters are handled in the refit. One of `"orig"`
#'   (default, inherit verbatim from `c.forest$tunable.params`),
#'   `"cf.default"` (let grf use its hardcoded defaults), or
#'   `"cf.autotune"` (set `tune.parameters = "all"` and re-tune). User
#'   `...` overrides win.
#' @param alpha Numeric vector of elastic-net mixing parameters in
#'   `[0, 1]`. If length > 1 (default
#'   `c(0, 0.25, 0.5, 0.75, 1)`), each candidate is fit with
#'   `cv.glmnet()` using a shared fold id (so CV scores are comparable),
#'   and the alpha with the lowest minimum cross-validated deviance is
#'   selected per fold and per nuisance. If length 1, no alpha tuning.
#' @param s Lambda value passed to `predict.cv.glmnet()`. Default
#'   `"lambda.min"` (CV minimum). `"lambda.1se"` is the sparser
#'   one-standard-error rule.
#' @param relax Logical. If `TRUE` (default), `cv.glmnet()` is called
#'   with `relax = TRUE`, fitting an unpenalized model on the active
#'   set at each lambda and CV-tuning a blending parameter `gamma` in
#'   `[0, 1]`. Improves prediction when the true signal is sparse but
#'   coefficients are biased by the lasso shrinkage.
#' @param glmnet_args Named list of additional arguments forwarded to
#'   `glmnet::cv.glmnet()` (e.g. `list(nfolds = 5L, nlambda = 50L)`).
#'   Note: `nfolds`/`foldid` supplied here are overridden by the
#'   internal shared `foldid` used for alpha tuning.
#' @param verbose Logical. If `TRUE`, print fold-by-fold progress.
#'   Default `FALSE`.
#' @param ... Additional arguments forwarded to `grf::causal_forest()`,
#'   overriding the values inherited from `c.forest` and the values
#'   implied by `tuning`.
#'
#' @return A `grf::causal_forest` object with cv.glmnet-derived `Y.hat`
#'   and `W.hat`. The S3 class is unchanged. An extra attribute
#'   `"glmcf_meta"` records the cross-fit configuration:
#'   \describe{
#'     \item{K}{Number of cross-fit folds.}
#'     \item{seed}{Seed used.}
#'     \item{eps}{Propensity clipping bound used (binary `W` only).}
#'     \item{w_type}{`"binary"` or `"continuous"`.}
#'     \item{clipped}{Number of propensity predictions clipped (binary
#'       `W` only).}
#'     \item{tuning}{The `tuning` mode used.}
#'     \item{alpha_grid}{The alpha grid searched.}
#'     \item{alpha_y, alpha_w}{Selected alpha per fold for Y and W.}
#'     \item{relax}{Whether `relax = TRUE` was used.}
#'     \item{s}{The lambda spec used at predict time.}
#'   }
#'
#' @details
#' **What changes vs. `grf::causal_forest()`?** Only the nuisance
#' predictions used to centre `Y` and `W`. The CATE estimator, honesty
#' splits, and identification assumptions are inherited from \pkg{grf}.
#'
#' **Why cv.glmnet?** Penalized regression is a fast, well-understood
#' baseline for nuisance estimation in Robinson-style residualisation
#' (Chernozhukov et al., 2018, treat lasso/elastic-net nuisances as the
#' canonical DML first stage).
#'
#' **Linearity-in-X assumption (load-bearing).** `glmcf()` fits cv.glmnet
#' on `X` as supplied, so `Y.hat` and `W.hat` are linear in the columns
#' of `X` (with elastic-net penalty + optional relax). If the true
#' `m(x) = E[Y | X]` or `e(x) = E[W | X]` is non-linear in `X`, the
#' lasso residuals carry residual confounding correlated with `X`, which
#' biases the Robinson centering and propagates into the CATE estimates
#' downstream. \pkg{grf}'s residual-on-residual splits absorb some of
#' this but not all. **For non-linear nuisances, pre-transform `X`
#' (interactions, splines, polynomials) before passing to `glmcf()`, or
#' use [tabcf()] which handles non-linearity natively.**
#'
#' **DML rate requirement for downstream inference.** For valid
#' sqrt(n) inference from `grf::average_treatment_effect()` (and
#' similar), DML theory requires both nuisances to converge at
#' `o(n^{-1/4})`. Lasso achieves
#' `s_n * sqrt(log(p) / n)` under approximate sparsity (effective
#' sparsity `s_n`, ambient dimension `p`). At small `p` and sparse
#' signal this is satisfied; at moderate `p` (e.g. `p > sqrt(n)`) or
#' non-sparse signal it is **not**, and ATE confidence intervals from
#' `grf` will under-cover. If you are in a wide-X / non-sparse regime,
#' prefer [tabcf()] or random-forest nuisances.
#'
#' **Cross-fitting.** Predictions are produced via K-fold cross-fitting:
#' for each fold, `cv.glmnet()` is fit on the other `K - 1` folds and
#' predicts on the held-out fold. If `c.forest$clusters` is non-trivial,
#' folds are built so that all units in a cluster end up in the same
#' fold (preserving \pkg{grf}'s cluster-aware inference).
#'
#' **Treatment type.** `W` is treated as binary if it is `{0, 1}`-valued
#' (numeric or integer with both levels present), a logical, or a 2-level
#' factor; in that case `family = "binomial"` is used and predictions are
#' clipped to `[eps, 1 - eps]` to preserve overlap. Otherwise `W` is
#' treated as continuous and `family = "gaussian"` is used. If `W` is
#' numeric with exactly 2 unique values that are not `{0, 1}` (e.g.
#' `{1, 2}` or `{-1, 1}`), `glmcf()` emits a warning and falls back to
#' the gaussian regressor; encode such treatments as `0/1` to use the
#' classifier.
#'
#' **Alpha tuning and reproducibility.** Within each cross-fit fold,
#' the inner CV is run with a single shared `foldid` across all candidate
#' alphas so the minimum-CV-deviance comparison across alphas is on the
#' same splits. The selected alpha may differ across folds and between
#' Y and W. Per-fold reselection is consistent in rate but
#' data-dependent: at small `n_train`, differences in CV deviance across
#' alphas are within sampling error, so the selected alpha can vary
#' under resampling (the actual selections are recorded in
#' `attr(out, "glmcf_meta")$alpha_y` and `$alpha_w` for diagnostics).
#' For studies that need reproducible CIs under resampling (bootstrap,
#' Monte Carlo), prefer a single fixed alpha (e.g. `alpha = 1` for pure
#' lasso) over the default grid.
#'
#' **Sample weights.** `c.forest$sample.weights` are passed to
#' `cv.glmnet()` via the `weights` argument so that weighted training is
#' consistent between the nuisance fits and the downstream causal forest.
#'
#' **Dependencies.** The `glmnet` package is gated as a Suggests
#' dependency. `glmcf()` calls `rlang::check_installed("glmnet")` at the
#' top and raises an informative error if missing.
#'
#' @references
#' Friedman, J., Hastie, T., and Tibshirani, R. (2010). Regularization
#' Paths for Generalized Linear Models via Coordinate Descent. *Journal
#' of Statistical Software*, 33(1), 1--22.
#'
#' Hastie, T., Tibshirani, R., and Tibshirani, R. (2020). Best Subset,
#' Forward Stepwise, or Lasso? Analysis and Recommendations Based on
#' Extensive Comparisons. *Statistical Science*, 35(4), 579--592.
#'
#' Robinson, P. M. (1988). Root-N-Consistent Semiparametric Regression.
#' *Econometrica*, 56(4), 931--954.
#'
#' Athey, S., Tibshirani, J., and Wager, S. (2019). Generalized Random
#' Forests. *Annals of Statistics*, 47(2), 1148--1178.
#' \doi{10.1214/18-AOS1709}
#'
#' @seealso [grf::causal_forest()] for the underlying estimator,
#'   [tabcf()] for the TabPFN-nuisance analogue,
#'   [cf_loco()] and [omni_hetero()] for downstream analysis.
#'
#' @export
#' @examples
#' \dontrun{
#' library(grf)
#' set.seed(1995)
#' n <- 200; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf  <- causal_forest(X, Y, W, num.trees = 200)
#'
#' # default: tune alpha over {0, .25, .5, .75, 1}, relax = TRUE
#' cf2 <- glmcf(cf, K = 5)
#'
#' # pure lasso, no relax
#' cf3 <- glmcf(cf, K = 5, alpha = 1, relax = FALSE)
#' }
glmcf <- function(c.forest,
                  X = NULL, Y = NULL, W = NULL,
                  K = 5L,
                  seed = 1995L,
                  eps = 1e-3,
                  tuning = c("orig", "cf.default", "cf.autotune"),
                  alpha = c(0, 0.25, 0.5, 0.75, 1),
                  s = "lambda.min",
                  relax = TRUE,
                  glmnet_args = list(),
                  verbose = FALSE,
                  ...) {

  # ---- 1. validate inputs --------------------------------------------------
  tuning <- match.arg(tuning)
  if (!inherits(c.forest, "causal_forest")) {
    stop("`c.forest` must be a grf causal_forest object.", call. = FALSE)
  }
  if (is.null(X)) X <- c.forest$X.orig
  if (is.null(Y)) Y <- c.forest$Y.orig
  if (is.null(W)) W <- c.forest$W.orig
  if (is.null(X) || is.null(Y) || is.null(W)) {
    stop("Could not recover X, Y, W from `c.forest`. ",
         "Pass them explicitly via the X, Y, W arguments.", call. = FALSE)
  }
  X <- as.matrix(X)
  if (anyNA(X)) {
    stop("`X` contains NAs. glmcf() does not support missing covariates.",
         call. = FALSE)
  }
  # Defensive colname normalization (cv.glmnet doesn't require it, but
  # downstream tools that consume the refit forest may).
  if (is.null(colnames(X)) || any(!nzchar(colnames(X))) ||
      any(duplicated(colnames(X)))) {
    colnames(X) <- paste0("V", seq_len(ncol(X)))
  }
  if (anyNA(Y)) stop("`Y` contains NAs.", call. = FALSE)
  if (anyNA(W)) stop("`W` contains NAs.", call. = FALSE)
  n <- nrow(X)
  if (length(Y) != n || length(W) != n) {
    stop("X, Y, W must have matching number of observations.", call. = FALSE)
  }
  if (!is.numeric(K) || length(K) != 1L || is.na(K) ||
      K < 2L || K > n || K != as.integer(K)) {
    stop("`K` must be an integer in [2, n].", call. = FALSE)
  }
  K <- as.integer(K)
  if (!is.numeric(eps) || length(eps) != 1L || eps <= 0 || eps >= 0.5) {
    stop("`eps` must be a single numeric in (0, 0.5).", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) < 1L ||
      anyNA(alpha) || any(alpha < 0) || any(alpha > 1)) {
    stop("`alpha` must be a numeric vector with entries in [0, 1].",
         call. = FALSE)
  }
  if (!is.logical(relax) || length(relax) != 1L || is.na(relax)) {
    stop("`relax` must be a single logical (TRUE or FALSE).",
         call. = FALSE)
  }

  # ---- 2. detect treatment type -------------------------------------------
  w_type <- .glmcf_w_type(W)

  # ---- 3. require glmnet --------------------------------------------------
  rlang::check_installed("glmnet",
                         reason = "to fit cv.glmnet nuisances in `glmcf()`.")

  # ---- 4. build folds (cluster-aware) -------------------------------------
  set.seed(seed)
  clusters <- c.forest$clusters
  fold <- .glmcf_make_folds(n = n, K = K, clusters = clusters)

  # sample weights passthrough (NULL if grf had none)
  sw <- c.forest$sample.weights

  # ---- 5. cross-fit nuisances ---------------------------------------------
  Y.hat <- numeric(n)
  W.hat <- numeric(n)
  alpha_y <- numeric(K)
  alpha_w <- numeric(K)

  for (k in seq_len(K)) {
    test  <- which(fold == k)
    train <- which(fold != k)
    if (verbose) message("[glmcf] fold ", k, "/", K,
                         " (train n = ", length(train),
                         ", test n = ", length(test), ")")

    weights_train <- if (!is.null(sw)) sw[train] else NULL

    y_pred <- .glmcf_fit_predict(
      X_train = X[train, , drop = FALSE], y_train = Y[train],
      X_test  = X[test,  , drop = FALSE],
      family  = "gaussian",
      alpha   = alpha, s = s, relax = relax,
      weights = weights_train,
      glmnet_args = glmnet_args,
      seed    = as.integer(seed) + k
    )
    Y.hat[test] <- y_pred
    alpha_y[k]  <- attr(y_pred, "alpha")

    w_pred <- .glmcf_fit_predict(
      X_train = X[train, , drop = FALSE], y_train = W[train],
      X_test  = X[test,  , drop = FALSE],
      family  = if (w_type == "binary") "binomial" else "gaussian",
      alpha   = alpha, s = s, relax = relax,
      weights = weights_train,
      glmnet_args = glmnet_args,
      seed    = as.integer(seed) + K + k
    )
    W.hat[test] <- w_pred
    alpha_w[k]  <- attr(w_pred, "alpha")
  }

  # ---- 5b. validate cv.glmnet postconditions ------------------------------
  if (!all(is.finite(Y.hat))) {
    stop("glmcf(): cv.glmnet returned non-finite Y.hat.", call. = FALSE)
  }
  if (w_type == "binary" &&
      !all(W.hat >= 0 & W.hat <= 1, na.rm = TRUE)) {
    stop("glmcf(): cv.glmnet binomial returned out-of-range W.hat ",
         "(not in [0,1]).", call. = FALSE)
  }
  if (!all(is.finite(W.hat))) {
    stop("glmcf(): cv.glmnet returned non-finite W.hat.", call. = FALSE)
  }

  # clip propensity to preserve overlap
  clip_res <- .glmcf_clip_propensity(W.hat, eps = eps,
                                     active = (w_type == "binary"))
  W.hat   <- clip_res$W.hat
  clipped <- clip_res$clipped
  if (clipped > 0L) {
    frac <- clipped / n
    severity <- if (frac > 0.05) {
      paste0(" Clipped fraction (", round(100 * frac, 1),
             "%) exceeds 5%: overlap is being manufactured by clipping ",
             "rather than learned. Treat downstream estimates as ",
             "exploratory; consider stronger regularisation (lower ",
             "alpha grid), `relax = FALSE`, or a richer basis.")
    } else {
      ""
    }
    warning("glmcf(): clipped ", clipped, " of ", n,
            " cv.glmnet propensity predictions to [eps, 1 - eps] = [",
            eps, ", ", 1 - eps,
            "] to preserve overlap. Increase `eps` to widen the window.",
            severity,
            call. = FALSE)
  }

  # ---- 6. refit causal forest with cv.glmnet nuisances --------------------
  set.seed(seed)
  tune_args <- .glmcf_build_tune_args(c.forest, tuning)
  cf_args <- c(
    list(
      X = X, Y = Y, W = W,
      Y.hat = Y.hat, W.hat = W.hat,
      num.trees                = c.forest$`_num_trees`,
      sample.weights           = c.forest$sample.weights,
      clusters                 = clusters,
      equalize.cluster.weights = c.forest$equalize.cluster.weights,
      ci.group.size            = c.forest$ci.group.size,
      seed                     = seed
    ),
    tune_args
  )
  user_args <- list(...)
  cf_args[names(user_args)] <- user_args

  out <- do.call(grf::causal_forest, cf_args)

  # ---- 7. annotate and return ---------------------------------------------
  attr(out, "glmcf_meta") <- list(
    K          = K,
    seed       = seed,
    eps        = eps,
    w_type     = w_type,
    clipped    = clipped,
    tuning     = tuning,
    alpha_grid = alpha,
    alpha_y    = alpha_y,
    alpha_w    = alpha_w,
    relax      = relax,
    s          = s
  )
  out
}


# -- helpers ------------------------------------------------------------------

#' @keywords internal
#' @noRd
.glmcf_build_tune_args <- function(c.forest, tuning) {
  tuning <- match.arg(tuning,
                      choices = c("orig", "cf.default", "cf.autotune"))
  if (tuning == "cf.default") {
    return(list())
  }
  if (tuning == "cf.autotune") {
    return(list(tune.parameters = "all"))
  }
  tp <- c.forest$tunable.params
  if (is.null(tp)) {
    return(list())
  }
  list(
    sample.fraction      = tp$sample.fraction,
    mtry                 = tp$mtry,
    min.node.size        = tp$min.node.size,
    honesty.fraction     = tp$honesty.fraction,
    honesty.prune.leaves = tp$honesty.prune.leaves,
    alpha                = tp$alpha,
    imbalance.penalty    = tp$imbalance.penalty
  )
}

#' @keywords internal
#' @noRd
.glmcf_w_type <- function(W) {
  if (is.factor(W)) {
    if (nlevels(W) == 2L) return("binary")
    stop("Factor W with != 2 levels is not supported by glmcf().",
         call. = FALSE)
  }
  if (is.logical(W)) return("binary")
  if (is.numeric(W) || is.integer(W)) {
    u <- unique(W[!is.na(W)])
    if (length(u) < 2L) {
      stop("`W` is constant (single unique value); nothing to estimate.",
           call. = FALSE)
    }
    if (length(u) == 2L) {
      if (all(sort(u) == c(0, 1))) return("binary")
      warning("W has 2 unique values {", paste(sort(u), collapse = ", "),
              "} not equal to {0, 1}; treating as continuous. ",
              "Encode as 0/1 to use the binomial glmnet.",
              call. = FALSE)
      return("continuous")
    }
    return("continuous")
  }
  stop("W must be logical, factor (2 levels), numeric, or integer.",
       call. = FALSE)
}

#' @keywords internal
#' @noRd
.glmcf_make_folds <- function(n, K, clusters = NULL) {
  if (is.null(clusters) || length(clusters) == 0L ||
      length(unique(clusters)) <= 1L) {
    return(sample(rep(seq_len(K), length.out = n)))
  }
  cl <- as.integer(as.factor(clusters))
  uniq_cl <- unique(cl)
  if (length(uniq_cl) < K) {
    stop("Cannot build ", K, " cluster-respecting folds from only ",
         length(uniq_cl), " distinct clusters. ",
         "Reduce K or supply finer-grained clusters.", call. = FALSE)
  }
  cl_fold <- sample(rep(seq_len(K), length.out = length(uniq_cl)))
  names(cl_fold) <- as.character(uniq_cl)
  unname(cl_fold[as.character(cl)])
}

#' @keywords internal
#' @noRd
.glmcf_clip_propensity <- function(W.hat, eps = 1e-3, active = TRUE) {
  if (!active) {
    return(list(W.hat = W.hat, clipped = 0L))
  }
  n_clip_lo <- sum(W.hat < eps,       na.rm = TRUE)
  n_clip_hi <- sum(W.hat > 1 - eps,   na.rm = TRUE)
  clipped   <- as.integer(n_clip_lo + n_clip_hi)
  if (clipped > 0L) {
    W.hat <- pmin(pmax(W.hat, eps), 1 - eps)
  }
  list(W.hat = W.hat, clipped = clipped)
}

#' @keywords internal
#' @noRd
#' @description
#' Coerce y_train to numeric 0/1 for binomial glmnet. Mirrors the
#' positive-class convention used in tabcf:
#' \itemize{
#'   \item factor: positive = second level (`levels(y)[2]`).
#'   \item logical: positive = `TRUE`.
#'   \item numeric/integer: returned as-is (must already be 0/1).
#' }
.glmcf_to_binary <- function(y) {
  if (is.factor(y)) {
    if (nlevels(y) != 2L) {
      stop("glmcf(): factor y for binomial must have 2 levels.",
           call. = FALSE)
    }
    return(as.integer(y) - 1L)
  }
  if (is.logical(y)) return(as.integer(y))
  if (is.numeric(y) || is.integer(y)) return(as.numeric(y))
  stop("Unsupported y type for binomial cv.glmnet: ",
       paste(class(y), collapse = "/"), call. = FALSE)
}

#' @keywords internal
#' @noRd
#' @description
#' Fit cv.glmnet (gaussian or binomial) on `(X_train, y_train)` with
#' optional alpha-grid tuning using a shared inner-CV `foldid`, then
#' predict on `X_test` at `s` (default `"lambda.min"`). Returns a numeric
#' vector with `attr(., "alpha")` set to the selected alpha.
.glmcf_fit_predict <- function(X_train, y_train, X_test, family,
                               alpha, s, relax, weights, glmnet_args,
                               seed) {
  if (family == "binomial") {
    y_train <- .glmcf_to_binary(y_train)
  } else {
    y_train <- as.numeric(y_train)
  }

  set.seed(seed)
  nfolds <- if (!is.null(glmnet_args$nfolds)) {
    as.integer(glmnet_args$nfolds)
  } else {
    10L
  }
  nfolds <- max(3L, min(nfolds, length(y_train)))
  foldid <- sample(rep(seq_len(nfolds), length.out = length(y_train)))

  # Strip nfolds/foldid from forwarded args; we control them.
  glmnet_args_local <- glmnet_args
  glmnet_args_local$nfolds <- NULL
  glmnet_args_local$foldid <- NULL
  glmnet_args_local$family <- NULL
  glmnet_args_local$alpha  <- NULL
  glmnet_args_local$relax  <- NULL
  glmnet_args_local$x      <- NULL
  glmnet_args_local$y      <- NULL
  glmnet_args_local$weights <- NULL

  best_alpha <- alpha[1]
  best_fit   <- NULL
  best_cvm   <- Inf
  last_err   <- NULL
  for (a in alpha) {
    cv_args <- c(
      list(x = X_train, y = y_train, alpha = a, family = family,
           relax = relax, foldid = foldid),
      if (!is.null(weights)) list(weights = weights),
      glmnet_args_local
    )
    fit <- tryCatch(do.call(glmnet::cv.glmnet, cv_args),
                    error = function(e) { last_err <<- e; NULL })
    if (is.null(fit)) next
    cvm <- min(fit$cvm, na.rm = TRUE)
    if (is.finite(cvm) && cvm < best_cvm) {
      best_cvm   <- cvm
      best_alpha <- a
      best_fit   <- fit
    }
  }
  if (is.null(best_fit)) {
    hint <- if (family == "binomial") {
      paste0(" Likely cause: severe class imbalance in the training fold ",
             "(min class count too small for cv.glmnet's inner CV). ",
             "Try a smaller `glmnet_args = list(nfolds = 3L)`, fewer outer ",
             "folds (`K`), or use a different nuisance estimator.")
    } else ""
    stop("glmcf(): all cv.glmnet alpha candidates failed.",
         if (!is.null(last_err)) paste0(" Last error: ",
                                        conditionMessage(last_err)) else "",
         hint, call. = FALSE)
  }

  pr_type <- if (family == "binomial") "response" else "link"
  preds <- as.numeric(stats::predict(best_fit, newx = X_test,
                                     s = s, type = pr_type))
  attr(preds, "alpha") <- best_alpha
  preds
}
