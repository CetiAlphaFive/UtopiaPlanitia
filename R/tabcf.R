#' Causal Forest with TabPFN-Estimated Nuisances
#'
#' Refits a `grf::causal_forest()` after replacing the conditional outcome
#' `m(x) = E[Y | X = x]` and propensity `e(x) = E[W | X = x]` with
#' cross-fitted predictions from TabPFN (Hollmann et al., 2023). The CATE
#' estimator and identification assumptions of `grf` are unchanged --- only
#' the Robinson-residualisation nuisances are swapped.
#'
#' @param c.forest A fitted causal forest object from the \pkg{grf} package,
#'   used both to recover `(X, Y, W)` (via `c.forest$X.orig`,
#'   `c.forest$Y.orig`, `c.forest$W.orig`) and to inherit tunable
#'   hyperparameters for the refit.
#' @param X,Y,W Optional overrides for the design matrix, outcome vector,
#'   and treatment vector. Useful when `c.forest` was stripped (no
#'   `X.orig` slot). If `NULL` (default) they are recovered from
#'   `c.forest`.
#' @param K Integer >= 2. Number of folds for cross-fitting the TabPFN
#'   nuisances. Default `5`.
#' @param seed Integer seed controlling fold assignment and the downstream
#'   `grf::causal_forest()` call. Default `1995`.
#' @param eps Numeric in `(0, 0.5)`. Propensity clipping bound for binary
#'   `W`: TabPFN classifier predictions are clipped to `[eps, 1 - eps]` to
#'   preserve overlap. Default `1e-3`.
#' @param tuning Character scalar controlling how `grf::causal_forest()`
#'   tunable hyperparameters (`min.node.size`, `mtry`, `sample.fraction`,
#'   `honesty.fraction`, `honesty.prune.leaves`, `alpha`,
#'   `imbalance.penalty`) are handled in the refit. One of:
#'   \itemize{
#'     \item `"orig"` (default): inherit tunable params verbatim from
#'       `c.forest$tunable.params`. No re-tuning.
#'     \item `"cf.default"`: do not pass any tunable params; let
#'       `grf::causal_forest()` use its hardcoded defaults. No re-tuning.
#'     \item `"cf.autotune"`: do not pass inherited tunable params and
#'       set `tune.parameters = "all"` so `grf::causal_forest()` re-tunes
#'       under the new TabPFN nuisances.
#'   }
#'   In all three modes, user-supplied arguments via `...` still override.
#' @param tabpfn_args Named list of additional arguments forwarded to
#'   `tabpfn::tab_pfn()` (e.g. `list(num_estimators = 4L,
#'   softmax_temperature = 0.9)`). To control the device or precision,
#'   pass a `control` element built with `tabpfn::control_tab_pfn()`
#'   (e.g. `list(control = tabpfn::control_tab_pfn(device = "cpu"))`);
#'   if no `control` is supplied, `tabcf()` builds one per fold with
#'   `random_state = seed + k`. Default `list()`.
#' @param verbose Logical. If `TRUE`, print fold-by-fold progress. Default
#'   `FALSE`.
#' @param ... Additional arguments forwarded to `grf::causal_forest()`,
#'   overriding the values inherited from `c.forest` and the values
#'   implied by `tuning`.
#'
#' @return A `grf::causal_forest` object with TabPFN-derived `Y.hat` and
#'   `W.hat`. The S3 class is unchanged, so `summary()`, `plot()`,
#'   [cf_loco()], and [omni_hetero()] work on it transparently. An extra
#'   attribute `"tabcf_meta"` records the cross-fit configuration:
#'   \describe{
#'     \item{K}{Number of folds.}
#'     \item{seed}{Seed used.}
#'     \item{eps}{Propensity clipping bound used (binary `W` only).}
#'     \item{w_type}{`"binary"` or `"continuous"`.}
#'     \item{clipped}{Number of propensity predictions clipped to the
#'       overlap region `[eps, 1 - eps]` (binary `W` only).}
#'     \item{tuning}{The `tuning` mode used (`"orig"`, `"cf.default"`,
#'       or `"cf.autotune"`).}
#'   }
#'
#' @details
#' **What changes vs. `grf::causal_forest()`?** Only the nuisance
#' predictions used to centre `Y` and `W`. The CATE estimator, honesty
#' splits, and identification assumptions are inherited from \pkg{grf}.
#'
#' **Why TabPFN?** TabPFN is a transformer pre-trained on synthetic
#' tabular generative processes and frequently outperforms gradient
#' boosting on small-to-medium tabular regression and binary
#' classification tasks. Better nuisance fits reduce bias in the Robinson
#' (1988) residualisation step that underlies `grf::causal_forest()`.
#'
#' **Cross-fitting.** TabPFN nuisance predictions are produced via K-fold
#' cross-fitting: for each fold, TabPFN is trained on the other `K - 1`
#' folds and predicts on the held-out fold. This avoids using a unit's
#' own outcome to predict its own residual. If `c.forest$clusters` is
#' non-trivial, folds are built so that all units in a cluster end up in
#' the same fold (preserving \pkg{grf}'s cluster-aware inference).
#'
#' **Treatment type.** `W` is treated as binary if it is `{0, 1}`-valued
#' (numeric or integer with both levels present), a logical, or a 2-level
#' factor; in that case TabPFN's classifier is fit and propensity scores
#' are clipped to `[eps, 1 - eps]` to preserve overlap. Otherwise `W` is
#' treated as continuous and TabPFN's regressor is fit. If `W` is numeric
#' with exactly 2 unique values that are not `{0, 1}` (e.g. `{1, 2}` or
#' `{-1, 1}`), `tabcf()` emits a warning and falls back to the regressor;
#' encode such treatments as `0/1` to use the classifier.
#'
#' **Tuning.** The `tuning` argument controls how `grf::causal_forest()`
#' tunable hyperparameters (`min.node.size`, `mtry`, `sample.fraction`,
#' `honesty.fraction`, `honesty.prune.leaves`, `alpha`,
#' `imbalance.penalty`) are handled in the refit:
#' \itemize{
#'   \item `"orig"` (default): inherit verbatim from
#'     `c.forest$tunable.params`. This preserves whatever \pkg{grf} chose
#'     (or the user supplied) for the original forest. No re-tuning.
#'   \item `"cf.default"`: omit those keys entirely so
#'     `grf::causal_forest()` falls back on its hardcoded defaults. Use
#'     this when the original forest's tuning was idiosyncratic and you
#'     want a clean baseline. No re-tuning.
#'   \item `"cf.autotune"`: omit the inherited keys and set
#'     `tune.parameters = "all"` so `grf::causal_forest()` re-tunes from
#'     scratch under the new TabPFN nuisances. The most expensive option
#'     but the only one that lets the tuner respond to the new nuisance
#'     fit. Pass `tune.num.trees`, `tune.num.reps`, `tune.num.draws` via
#'     `...` if you need non-default tuner controls.
#' }
#' In all three modes, anything the caller passes via `...` overrides
#' the value chosen by `tuning`.
#'
#' **TabPFN seed passthrough.** Reproducibility for TabPFN is governed by
#' `random_state` inside the `control` argument of
#' `tabpfn::tab_pfn()`. If `tabpfn_args` does not already contain a
#' `control` element, `tabcf()` builds one per fold via
#' `tabpfn::control_tab_pfn(random_state = seed + k)` so each fold gets a
#' deterministic but distinct TabPFN seed derived from the user-provided
#' `seed`. If the user supplies `tabpfn_args$control`, that control
#' object is passed through verbatim for every fold (so the user is
#' responsible for any seeding).
#'
#' **Dependencies and TabPFN backend.** The `tabpfn` R package
#' (tidymodels-style wrapper around the Python TabPFN backend, v0.1.0+)
#' is gated as a Suggests dependency. `tabcf()` calls
#' `rlang::check_installed("tabpfn")` at the top and raises an
#' informative error if missing. The Python backend additionally
#' requires a one-time license acceptance: register at
#' <https://ux.priorlabs.ai>, accept the license, copy your API key, and
#' set `TABPFN_TOKEN` in the environment before calling `tabcf()`.
#' Without it, the underlying Python call raises `TabPFNLicenseError`.
#' The easiest way to set the token from inside R is to call
#' [setup_tabpfn_token()], which checks for the variable, walks you
#' through registering at <https://ux.priorlabs.ai/account>, and
#' optionally appends `TABPFN_TOKEN=...` to your `~/.Renviron` so
#' future sessions pick it up automatically. `tabcf()` itself calls
#' [setup_tabpfn_token()] when the variable is missing in an
#' interactive session.
#'
#' @references
#' Hollmann, N., Müller, S., Eggensperger, K., and Hutter, F. (2023).
#' TabPFN: A Transformer That Solves Small Tabular Classification
#' Problems in a Second. *ICLR*.
#' \doi{10.48550/arXiv.2207.01848}
#'
#' Hollmann, N., Müller, S., Purucker, L., Krishnakumar, A., Körfer, M.,
#' Hoo, S. B., Schirrmeister, R. T., and Hutter, F. (2025). Accurate
#' Predictions on Small Data with a Tabular Foundation Model. *Nature*.
#'
#' Robinson, P. M. (1988). Root-N-Consistent Semiparametric Regression.
#' *Econometrica*, 56(4), 931--954.
#'
#' Athey, S., Tibshirani, J., and Wager, S. (2019). Generalized Random
#' Forests. *Annals of Statistics*, 47(2), 1148--1178.
#' \doi{10.1214/18-AOS1709}
#'
#' @seealso [grf::causal_forest()] for the underlying estimator,
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
#' # default: inherit grf's tuned params from cf
#' cf2 <- tabcf(cf, K = 5)
#'
#' # use grf's hardcoded defaults
#' cf3 <- tabcf(cf, K = 5, tuning = "cf.default")
#'
#' # re-tune from scratch under TabPFN nuisances
#' cf4 <- tabcf(cf, K = 5, tuning = "cf.autotune")
#' }
tabcf <- function(c.forest,
                  X = NULL, Y = NULL, W = NULL,
                  K = 5L,
                  seed = 1995L,
                  eps = 1e-3,
                  tuning = c("orig", "cf.default", "cf.autotune"),
                  tabpfn_args = list(),
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
    stop("`X` contains NAs. tabcf() does not support missing covariates.",
         call. = FALSE)
  }
  # Normalize colnames for tabpfn's parsnip-based predict, which requires
  # train and test to carry identical, non-empty, unique feature names.
  # Matrices without colnames break the underlying hardhat check.
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

  # ---- 2. detect treatment type -------------------------------------------
  w_type <- .tabcf_w_type(W)

  # ---- 3. require tabpfn ---------------------------------------------------
  rlang::check_installed("tabpfn",
                         reason = "to fit TabPFN nuisances in `tabcf()`.")

  # ---- 4a. require TABPFN_TOKEN -------------------------------------------
  if (!nzchar(Sys.getenv("TABPFN_TOKEN", unset = ""))) {
    if (interactive()) {
      message("TABPFN_TOKEN not set. Launching setup_tabpfn_token()...")
      ok <- setup_tabpfn_token()
      if (!isTRUE(ok)) {
        stop("TABPFN_TOKEN setup did not complete. Aborting tabcf().",
             call. = FALSE)
      }
    } else {
      stop("TABPFN_TOKEN env var not set. Run `setup_tabpfn_token()` interactively. ",
           "See https://ux.priorlabs.ai/account", call. = FALSE)
    }
  }

  # ---- 4. build folds (cluster-aware) -------------------------------------
  set.seed(seed)
  clusters <- c.forest$clusters
  fold <- .tabcf_make_folds(n = n, K = K, clusters = clusters)

  # ---- 5. cross-fit nuisances ---------------------------------------------
  Y.hat <- numeric(n)
  W.hat <- numeric(n)
  user_supplied_control <- "control" %in% names(tabpfn_args)
  for (k in seq_len(K)) {
    test  <- which(fold == k)
    train <- which(fold != k)
    if (verbose) message("[tabcf] fold ", k, "/", K,
                         " (train n = ", length(train),
                         ", test n = ", length(test), ")")

    # Build per-fold control unless the user already supplied one.
    fold_args <- tabpfn_args
    if (!user_supplied_control) {
      fold_args$control <- tabpfn::control_tab_pfn(
        random_state = as.integer(seed) + k
      )
    }

    Y.hat[test] <- .tabcf_fit_predict(
      X_train = X[train, , drop = FALSE], y_train = Y[train],
      X_test  = X[test,  , drop = FALSE],
      kind = "regressor", tabpfn_args = fold_args
    )
    W.hat[test] <- .tabcf_fit_predict(
      X_train = X[train, , drop = FALSE], y_train = W[train],
      X_test  = X[test,  , drop = FALSE],
      kind = if (w_type == "binary") "classifier" else "regressor",
      tabpfn_args = fold_args
    )
  }

  # ---- 5b. validate TabPFN postconditions ---------------------------------
  if (!all(is.finite(Y.hat))) {
    stop("tabcf(): TabPFN returned non-finite Y.hat. ",
         "Check tabpfn installation/output shape.", call. = FALSE)
  }
  if (w_type == "binary" &&
      !all(W.hat >= 0 & W.hat <= 1, na.rm = TRUE)) {
    stop("tabcf(): TabPFN classifier returned out-of-range W.hat ",
         "(not in [0,1]).", call. = FALSE)
  }
  if (!all(is.finite(W.hat))) {
    stop("tabcf(): TabPFN returned non-finite W.hat.", call. = FALSE)
  }

  # clip propensity to preserve overlap
  clip_res <- .tabcf_clip_propensity(W.hat, eps = eps,
                                     active = (w_type == "binary"))
  W.hat   <- clip_res$W.hat
  clipped <- clip_res$clipped
  if (clipped > 0L) {
    warning("tabcf(): clipped ", clipped, " of ", n,
            " TabPFN propensity predictions to [eps, 1 - eps] = [",
            eps, ", ", 1 - eps,
            "] to preserve overlap. Increase `eps` to widen the window.",
            call. = FALSE)
  }

  # ---- 6. refit causal forest with TabPFN nuisances -----------------------
  set.seed(seed)
  tune_args <- .tabcf_build_tune_args(c.forest, tuning)
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
  # caller overrides via ...
  user_args <- list(...)
  cf_args[names(user_args)] <- user_args

  out <- do.call(grf::causal_forest, cf_args)

  # ---- 7. annotate and return ---------------------------------------------
  attr(out, "tabcf_meta") <- list(
    K       = K,
    seed    = seed,
    eps     = eps,
    w_type  = w_type,
    clipped = clipped,
    tuning  = tuning
  )
  out
}


# -- helpers ------------------------------------------------------------------

#' @keywords internal
#' @noRd
#' @description
#' Build the tuning-related portion of the `cf_args` list passed to
#' `grf::causal_forest()`. Returns a named list whose contents depend on
#' `tuning`:
#' \itemize{
#'   \item `"orig"`: the seven tunable params copied from
#'     `c.forest$tunable.params`.
#'   \item `"cf.default"`: empty list (let grf use its hardcoded
#'     defaults).
#'   \item `"cf.autotune"`: `list(tune.parameters = "all")` so grf
#'     re-tunes under the new nuisances.
#' }
#' Splitting this out makes the three modes unit-testable without
#' touching tabpfn.
.tabcf_build_tune_args <- function(c.forest, tuning) {
  tuning <- match.arg(tuning,
                      choices = c("orig", "cf.default", "cf.autotune"))
  if (tuning == "cf.default") {
    return(list())
  }
  if (tuning == "cf.autotune") {
    return(list(tune.parameters = "all"))
  }
  # tuning == "orig"
  tp <- c.forest$tunable.params
  if (is.null(tp)) {
    # Defensive: nothing to inherit -> behave like cf.default.
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
#' @description
#' Resolve the user-facing `clip`/`eps` arguments into a normalized
#' `list(active, lo, hi)`. Owns the `eps` deprecation, validation, and the
#' eps/clip conflict error so `tabcf()` stays linear.
.tabcf_resolve_clip <- function(clip = FALSE, eps = NULL) {
  eps_given       <- !is.null(eps)
  clip_is_default <- isFALSE(clip)

  if (eps_given && !clip_is_default) {
    stop("Pass either `clip` or the deprecated `eps`, not both.",
         call. = FALSE)
  }

  if (eps_given) {
    warning("`eps` is deprecated; use `clip = c(lo, hi)`. ",
            "Mapping `eps` to `clip = c(eps, 1 - eps)`.", call. = FALSE)
    if (!is.numeric(eps) || length(eps) != 1L || is.na(eps) ||
        eps <= 0 || eps >= 0.5) {
      stop("`eps` must be a single numeric in (0, 0.5).", call. = FALSE)
    }
    return(list(active = TRUE, lo = eps, hi = 1 - eps))
  }

  if (is.logical(clip) && length(clip) == 1L && !is.na(clip)) {
    if (!clip) return(list(active = FALSE, lo = NA_real_, hi = NA_real_))
    return(list(active = TRUE, lo = 1e-3, hi = 1 - 1e-3))
  }

  if (is.numeric(clip) && length(clip) == 2L && !anyNA(clip)) {
    lo <- clip[1L]; hi <- clip[2L]
    if (!(lo > 0 && hi < 1 && lo < hi)) {
      stop("`clip` range must satisfy 0 < lo < hi < 1.", call. = FALSE)
    }
    return(list(active = TRUE, lo = lo, hi = hi))
  }

  stop("`clip` must be FALSE, TRUE, or a numeric c(lo, hi).", call. = FALSE)
}

#' @keywords internal
#' @noRd
.tabcf_w_type <- function(W) {
  if (is.factor(W)) {
    if (nlevels(W) == 2L) return("binary")
    stop("Factor W with != 2 levels is not supported by tabcf().",
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
      # Two unique numeric values but not {0, 1}: warn and fall back.
      warning("W has 2 unique values {", paste(sort(u), collapse = ", "),
              "} not equal to {0, 1}; treating as continuous. ",
              "Encode as 0/1 to use the TabPFN classifier.",
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
.tabcf_make_folds <- function(n, K, clusters = NULL) {
  if (is.null(clusters) || length(clusters) == 0L ||
      length(unique(clusters)) <= 1L) {
    return(sample(rep(seq_len(K), length.out = n)))
  }
  # cluster-aware folds: assign each cluster to a fold
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
#' @description
#' Clip propensity predictions to `[lo, hi]`. Returns the clipped vector and
#' the count of clipped entries. When `active = FALSE`, returns the input
#' unchanged with `clipped = 0`.
.tabcf_clip_propensity <- function(W.hat, lo = 1e-3, hi = 1 - 1e-3,
                                   active = TRUE) {
  if (!active) {
    return(list(W.hat = W.hat, clipped = 0L))
  }
  n_clip_lo <- sum(W.hat < lo, na.rm = TRUE)
  n_clip_hi <- sum(W.hat > hi, na.rm = TRUE)
  clipped   <- as.integer(n_clip_lo + n_clip_hi)
  if (clipped > 0L) {
    W.hat <- pmin(pmax(W.hat, lo), hi)
  }
  list(W.hat = W.hat, clipped = clipped)
}

#' @keywords internal
#' @noRd
#' @description
#' Decide which factor level of `y_train` corresponds to "treated" (W = 1)
#' so that the classifier's positive-class probability can be extracted.
#' Rules:
#' \itemize{
#'   \item factor: positive = second level (`levels(y)[2]`).
#'   \item logical: positive = `"TRUE"`.
#'   \item numeric/integer 0/1: positive = `"1"`.
#' }
#' Returns a length-1 character that matches a `.pred_<level>` column in
#' the tibble returned by `predict()` on a fitted `tab_pfn` classifier.
.tabcf_positive_class <- function(y_train) {
  if (is.factor(y_train))   return(as.character(levels(y_train)[2L]))
  if (is.logical(y_train))  return("TRUE")
  if (is.numeric(y_train) || is.integer(y_train)) return("1")
  stop("Unsupported y_train type for tabpfn classifier: ",
       paste(class(y_train), collapse = "/"), call. = FALSE)
}

#' @keywords internal
#' @noRd
#' @description
#' Fit a single TabPFN model (regressor or classifier) on `(X_train,
#' y_train)` and predict on `X_test`. Single clean code path against the
#' `tabpfn` 0.1.0 API: one S3 generic `tabpfn::tab_pfn(x, y, ...)` plus
#' `predict(fit, new_x)`. Per fold, `tabpfn_args` is spread as additional
#' arguments to `tab_pfn()`.
#'
#' Regression: `y_train` numeric -> `predict()` returns a tibble with a
#' `.pred` column; we extract `.pred`.
#'
#' Classification (binary): `y_train` is coerced to a factor whose second
#' level is the "positive" (treated) class as decided by
#' `.tabcf_positive_class()`. `predict()` returns a tibble with one
#' `.pred_<level>` column per class; we return the column for the
#' positive level.
.tabcf_fit_predict <- function(X_train, y_train, X_test, kind,
                               tabpfn_args = list()) {
  if (kind == "regressor") {
    y_train <- as.numeric(y_train)
  } else if (kind == "classifier") {
    pos <- .tabcf_positive_class(y_train)
    if (is.factor(y_train)) {
      # leave levels alone; pos is already levels(y)[2]
    } else if (is.logical(y_train)) {
      y_train <- factor(y_train, levels = c("FALSE", "TRUE"))
    } else {
      # numeric/integer 0/1 -> factor with "0","1"
      y_train <- factor(as.integer(y_train), levels = c("0", "1"))
    }
  } else {
    stop("Unknown TabPFN kind: ", kind, call. = FALSE)
  }

  fit <- do.call(
    tabpfn::tab_pfn,
    c(list(x = X_train, y = y_train), tabpfn_args)
  )

  pr <- stats::predict(fit, X_test)

  if (kind == "regressor") {
    if (is.data.frame(pr) && ".pred" %in% names(pr)) return(as.numeric(pr$.pred))
    if (is.numeric(pr)) return(as.numeric(pr))
    stop("tabcf(): unexpected regressor predict() return shape: ",
         paste(class(pr), collapse = "/"), call. = FALSE)
  }

  # classifier
  col <- paste0(".pred_", pos)
  if (!is.data.frame(pr) || !(col %in% names(pr))) {
    stop("tabcf(): expected classifier predict() to return a tibble with ",
         "column '", col, "'; got class ",
         paste(class(pr), collapse = "/"),
         if (is.data.frame(pr))
           paste0(" with names: ", paste(names(pr), collapse = ", "))
         else "",
         call. = FALSE)
  }
  as.numeric(pr[[col]])
}
