#' Auto-Selected Nuisance Causal Forest
#'
#' Refits a `grf::causal_forest()` after replacing the conditional outcome
#' `m(x) = E[Y | X = x]` and propensity `e(x) = E[W | X = x]` with the
#' best-performing nuisance estimator from a candidate pool, scored by
#' shared K-fold cross-fit weighted CV loss. The CATE estimator and
#' identification assumptions of `grf` are unchanged --- only the
#' Robinson-residualisation nuisances are swapped (and only if the winner
#' beats grf's own baseline by a configurable margin).
#'
#' @param c.forest A fitted causal forest object from the \pkg{grf}
#'   package, used both to recover `(X, Y, W)` (via `c.forest$X.orig`,
#'   `c.forest$Y.orig`, `c.forest$W.orig`) and to inherit tunable
#'   hyperparameters for the refit.
#' @param X,Y,W Optional overrides for the design matrix, outcome vector,
#'   and treatment vector. If `NULL` (default) they are recovered from
#'   `c.forest`.
#' @param K Integer >= 2. Number of cross-fit folds shared across all
#'   candidates. Default `5`.
#' @param seed Integer seed for fold assignment, per-candidate RNG, and
#'   the downstream `grf::causal_forest()` call. Default `1995`.
#' @param eps Numeric in `(0, 0.5)`. Propensity clipping bound for
#'   binary `W` (winner's predictions clipped to `[eps, 1 - eps]` to
#'   preserve overlap). Default `1e-3`.
#' @param tuning Character scalar controlling how `grf::causal_forest()`
#'   tunable hyperparameters are handled in the refit. One of `"orig"`
#'   (default, inherit verbatim from `c.forest$tunable.params`),
#'   `"cf.default"`, or `"cf.autotune"`. User `...` overrides win.
#' @param pool Character vector of candidate nuisance estimators to
#'   compare. Default `c("grf", "glmnet", "xgboost", "tabpfn")`. Any
#'   candidate whose required dependency is missing (or which is
#'   incompatible with non-trivial `sample.weights`) is dropped with a
#'   warning. The "grf" baseline is **always** refit under the same
#'   K-fold cross-fit protocol as the other candidates (using
#'   `grf::regression_forest()` on the held-out fold), so all CV losses
#'   are apples-to-apples.
#' @param min_improvement Either the character string `"1se"` (default),
#'   the character string `"0"`, or a non-negative numeric. Controls the
#'   margin by which a candidate must beat the grf baseline to trigger a
#'   nuisance swap. `"1se"` requires the mean fold-wise loss reduction
#'   to exceed the paired standard error of the fold differences (the
#'   1-SE rule). `"0"` swaps on any improvement. A numeric value swaps
#'   when the absolute loss reduction exceeds it.
#' @param term_evals Integer >= 0. Random-search budget for the xgboost
#'   AutoTuner (per outer fold per nuisance). `0` disables tuning and
#'   uses mlr3's default xgboost hyperparameters. Default `10L`.
#' @param tabpfn_args,glmnet_args,xgboost_args Optional named lists of
#'   per-candidate extra arguments. `tabpfn_args` is forwarded to
#'   `tabpfn::tab_pfn()`; `glmnet_args` to `glmnet::cv.glmnet()`;
#'   `xgboost_args` is forwarded to the mlr3 `xgboost` learner's
#'   `param_set$values` (use to set, e.g., `nthread = 4` for multi-core
#'   CPU or `device = "cuda"` to enable GPU on systems with a CUDA
#'   xgboost build; defaults to mlr3's reproducibility-oriented
#'   `nthread = 1` and CPU). Tuned hyperparameters override matching
#'   keys. All default `list()`.
#' @param verbose Logical. Print per-candidate / per-fold progress.
#'   Default `FALSE`.
#' @param ... Additional arguments forwarded to `grf::causal_forest()`
#'   in the refit, overriding the values inherited from `c.forest` and
#'   the values implied by `tuning`.
#'
#' @return A `grf::causal_forest` object with auto-selected `Y.hat` and
#'   `W.hat`. The S3 class is unchanged. An extra attribute
#'   `"autocf_meta"` records the comparison and selection:
#'   \describe{
#'     \item{K, seed, eps, w_type, clipped, tuning}{Same as
#'       [tabcf()] / [glmcf()].}
#'     \item{pool}{Pool requested by the caller.}
#'     \item{pool_run}{Pool actually compared after dependency
#'       filtering.}
#'     \item{scores}{Data frame of mean weighted CV losses per
#'       candidate (`y_loss`, `y_se`, `w_loss`, `w_se`).}
#'     \item{fold_losses}{Long-format data frame of per-fold per-candidate
#'       weighted losses for diagnostics and re-derivation of the 1-SE
#'       rule.}
#'     \item{winner_y, winner_w}{Selected nuisance estimator per role.}
#'     \item{swap_y, swap_w}{Logical flags indicating whether the swap
#'       fired (TRUE) or grf's baseline was retained (FALSE).}
#'     \item{gap_y, gap_w}{Mean fold-wise loss reduction (positive =
#'       candidate beats grf).}
#'     \item{min_improvement}{The threshold used.}
#'   }
#'
#' @details
#' **What this function is for.** A diagnostic + drop-in replacement.
#' grf's default nuisance estimators (regression forests on Y and W) are
#' usually fine but not always best. `autocf()` runs a small candidate
#' pool under a shared K-fold cross-fit protocol with weighted CV loss
#' (mean-squared error for Y and continuous W; Brier score for binary
#' W, which is identical to MSE on 0/1 outcomes), reports the comparison
#' table, and only swaps in a competitor if it beats the grf baseline by
#' a margin you choose.
#'
#' **Apples-to-apples grf baseline (R1 from review).** The "grf"
#' candidate in the pool is **not** scored using `c.forest$Y.hat` /
#' `W.hat`, because those come from grf's internal honest-split
#' subsampling protocol --- not strict K-fold cross-fitting --- and would
#' be unfair to compare against true K-fold OOF predictions from the
#' other candidates. Instead, `autocf()` re-trains a
#' `grf::regression_forest` on each held-out fold's training set and
#' predicts on the held-out fold, producing strict K-fold OOF
#' predictions for the "grf" candidate identically to every other
#' candidate.
#'
#' **1-SE rule for the swap (R2 from review).** With finite-sample CV
#' noise any non-trivial candidate will produce some random improvement
#' on a particular seed. Default `min_improvement = "1se"` requires the
#' mean fold-wise loss reduction to exceed the paired-fold standard
#' error of that reduction:
#' \deqn{\text{swap} \iff
#'   \bar d_K - \widehat{SE}(\bar d_K) > 0,
#'   \quad d_k = \text{loss}^{\text{grf}}_k - \text{loss}^{\text{cand}}_k.}
#' This is the same kind of conservativism that `glmnet`'s `lambda.1se`
#' uses, generalized to model selection. Use `min_improvement = "0"` to
#' force any improvement to fire, or supply a numeric absolute threshold.
#'
#' **Weighted CV losses (R3 from review).** When
#' `c.forest$sample.weights` is non-trivial, both training and per-fold
#' loss computation are weighted, so the loss being minimised matches
#' how the chosen nuisances will actually be used downstream by grf.
#' Candidates that cannot accept training weights (currently `tabpfn`)
#' are dropped from the pool with a warning when weights are
#' non-trivial.
#'
#' **Selection-induced inference.** Picking the best of M candidates by
#' CV loss is a model-selection step. It is benign here because
#' `grf::causal_forest()` uses a Neyman-orthogonal R-learner score for
#' the second stage; small perturbations in the chosen nuisance do not
#' invalidate sqrt(n) inference (Chernozhukov et al., 2018; Foster &
#' Syrgkanis, 2019). Without orthogonality this would not be safe.
#'
#' **Convergence-rate caveat.** CV-loss minimization picks the lowest
#' finite-sample loss, not the best convergence rate. For valid sqrt(n)
#' downstream inference (e.g. `grf::average_treatment_effect()`),
#' chosen nuisances must converge at `o(n^{-1/4})`. The default pool
#' members (grf reg-forest, glmnet under sparsity, xgboost under
#' boosting consistency) are believed adequate at typical n; tabpfn has
#' no formal rate proof and should be treated as exploratory when
#' downstream confidence intervals matter.
#'
#' **Reproducibility under `future` plans.** xgboost's mlr3 AutoTuner
#' inner CV runs through the `future` framework. To prevent
#' non-deterministic seeding under user-set `future::plan(multisession)`
#' or similar, `autocf()` forces `future::plan("sequential")` for the
#' duration of its call and restores the previous plan on exit.
#'
#' @references
#' Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
#' C., Newey, W., and Robins, J. (2018). Double/Debiased Machine
#' Learning for Treatment and Structural Parameters. *The Econometrics
#' Journal*, 21(1), C1--C68.
#'
#' Foster, D. J. and Syrgkanis, V. (2019). Orthogonal Statistical
#' Learning. \doi{10.48550/arXiv.1901.09036}
#'
#' Robinson, P. M. (1988). Root-N-Consistent Semiparametric Regression.
#' *Econometrica*, 56(4), 931--954.
#'
#' Athey, S., Tibshirani, J., and Wager, S. (2019). Generalized Random
#' Forests. *Annals of Statistics*, 47(2), 1148--1178.
#'
#' @seealso [grf::causal_forest()] for the underlying estimator,
#'   [tabcf()] / [glmcf()] for fixed-nuisance variants,
#'   [cf_loco()] and [omni_hetero()] for downstream analysis.
#'
#' @export
#' @examples
#' \dontrun{
#' library(grf)
#' set.seed(1995)
#' n <- 400; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", seq_len(p))
#' W <- rbinom(n, 1, 0.5)
#' Y <- X[, 1] * W + rnorm(n)
#' cf <- causal_forest(X, Y, W, num.trees = 200)
#'
#' # Default: compare grf, glmnet, xgboost, tabpfn (whatever is installed)
#' cf2 <- autocf(cf, K = 5)
#' attr(cf2, "autocf_meta")$scores
#' attr(cf2, "autocf_meta")$winner_y
#' attr(cf2, "autocf_meta")$winner_w
#'
#' # Smaller pool, force any improvement to swap
#' cf3 <- autocf(cf, pool = c("grf", "glmnet"), min_improvement = "0")
#' }
autocf <- function(c.forest,
                   X = NULL, Y = NULL, W = NULL,
                   K = 5L,
                   seed = 1995L,
                   eps = 1e-3,
                   tuning = c("orig", "cf.default", "cf.autotune"),
                   pool = c("grf", "glmnet", "xgboost", "tabpfn"),
                   min_improvement = "1se",
                   term_evals = 10L,
                   tabpfn_args = list(),
                   glmnet_args = list(),
                   xgboost_args = list(),
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
    stop("`X` contains NAs. autocf() does not support missing covariates.",
         call. = FALSE)
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
  if (!is.character(pool) || length(pool) < 1L) {
    stop("`pool` must be a non-empty character vector.", call. = FALSE)
  }
  pool <- unique(pool)
  bad <- setdiff(pool, c("grf", "glmnet", "xgboost", "tabpfn"))
  if (length(bad) > 0L) {
    stop("Unknown candidate(s) in `pool`: ",
         paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  if (!"grf" %in% pool) {
    stop("`pool` must include 'grf' as the baseline against which other ",
         "candidates are compared.", call. = FALSE)
  }
  if (!is.numeric(term_evals) || length(term_evals) != 1L ||
      is.na(term_evals) || term_evals < 0L ||
      term_evals != as.integer(term_evals)) {
    stop("`term_evals` must be a non-negative integer.", call. = FALSE)
  }
  term_evals <- as.integer(term_evals)
  min_improvement <- .autocf_validate_min_improvement(min_improvement)

  # ---- 2. detect treatment type -------------------------------------------
  w_type <- .autocf_w_type(W)

  # ---- 3. build folds and inspect weights ---------------------------------
  set.seed(seed)
  clusters <- c.forest$clusters
  fold <- .autocf_make_folds(n = n, K = K, clusters = clusters)
  sw <- c.forest$sample.weights
  weights_nontrivial <- !is.null(sw) && length(sw) == n &&
                        length(unique(sw)) > 1L

  # ---- 4. filter pool by available deps + weights compatibility -----------
  pool_run <- .autocf_filter_pool(pool, weights_nontrivial, verbose = verbose)
  if (length(pool_run) < 1L || !"grf" %in% pool_run) {
    stop("After dependency filtering, no usable candidates remain (need ",
         "at least the 'grf' baseline).", call. = FALSE)
  }

  # ---- 5. force sequential future plan for reproducibility ----------------
  if (requireNamespace("future", quietly = TRUE)) {
    oplan <- future::plan("sequential")
    on.exit(future::plan(oplan), add = TRUE)
  }

  # ---- 6. run each candidate's K-fold cross-fit ---------------------------
  fits <- list()
  for (cand in pool_run) {
    if (verbose) message("[autocf] running candidate: ", cand)
    fits[[cand]] <- .autocf_run_candidate(
      cand        = cand,
      X = X, Y = Y, W = W, fold = fold, K = K,
      w_type      = w_type,
      sw          = sw,
      seed        = as.integer(seed),
      tabpfn_args = tabpfn_args,
      glmnet_args = glmnet_args,
      xgboost_args = xgboost_args,
      term_evals  = term_evals,
      verbose     = verbose
    )
  }

  # ---- 7. score and select ------------------------------------------------
  scores <- .autocf_score(fits, pool_run)
  selection <- .autocf_select(scores, baseline = "grf",
                              min_improvement = min_improvement)

  Y.hat <- fits[[selection$winner_y]]$Y.hat
  W.hat <- fits[[selection$winner_w]]$W.hat

  # ---- 7b. validate winner postconditions + clip propensity ---------------
  if (!all(is.finite(Y.hat))) {
    stop("autocf(): selected Y.hat contains non-finite values from '",
         selection$winner_y, "'.", call. = FALSE)
  }
  if (!all(is.finite(W.hat))) {
    stop("autocf(): selected W.hat contains non-finite values from '",
         selection$winner_w, "'.", call. = FALSE)
  }
  if (w_type == "binary" && !all(W.hat >= 0 & W.hat <= 1, na.rm = TRUE)) {
    # glmnet/xgboost/tabpfn classifiers should produce probs in [0,1];
    # grf::regression_forest on 0/1 normally also stays inside [0,1] but
    # could undershoot/overshoot at boundaries — squash defensively before
    # clipping.
    W.hat <- pmin(pmax(W.hat, 0), 1)
  }
  clip_res <- .autocf_clip_propensity(W.hat, eps = eps,
                                      active = (w_type == "binary"))
  W.hat   <- clip_res$W.hat
  clipped <- clip_res$clipped
  if (clipped > 0L) {
    warning("autocf(): clipped ", clipped, " of ", n,
            " selected propensity predictions to [eps, 1 - eps] = [",
            eps, ", ", 1 - eps,
            "]. Selected W estimator: '", selection$winner_w, "'.",
            call. = FALSE)
  }

  # ---- 8. refit causal forest with selected nuisances ---------------------
  set.seed(seed)
  tune_args <- .autocf_build_tune_args(c.forest, tuning)
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

  # ---- 9. annotate and return ---------------------------------------------
  attr(out, "autocf_meta") <- list(
    K               = K,
    seed            = seed,
    eps             = eps,
    w_type          = w_type,
    clipped         = clipped,
    tuning          = tuning,
    pool            = pool,
    pool_run        = pool_run,
    scores          = scores$summary,
    fold_losses     = scores$per_fold,
    winner_y        = selection$winner_y,
    winner_w        = selection$winner_w,
    swap_y          = selection$swap_y,
    swap_w          = selection$swap_w,
    gap_y           = selection$gap_y,
    gap_w           = selection$gap_w,
    threshold_y     = selection$threshold_y,
    threshold_w     = selection$threshold_w,
    min_improvement = min_improvement
  )

  .autocf_print_stats(attr(out, "autocf_meta"))

  out
}

#' @keywords internal
#' @noRd
#' @description
#' Print a small fit-stats table at the end of `autocf()`: per-candidate
#' weighted CV losses (Y MSE; W Brier for binary, MSE for continuous),
#' chosen winner per nuisance, swap decision, and the threshold the swap
#' had to clear.
.autocf_print_stats <- function(meta) {
  s <- meta$scores
  s$Y_MSE   <- formatC(s$y_loss, format = "f", digits = 4)
  s$Y_SE    <- formatC(s$y_se,   format = "f", digits = 4)
  w_label   <- if (meta$w_type == "binary") "W_Brier" else "W_MSE"
  s$W_score <- formatC(s$w_loss, format = "f", digits = 4)
  s$W_SE    <- formatC(s$w_se,   format = "f", digits = 4)

  display <- data.frame(
    candidate = s$candidate,
    Y_MSE     = s$Y_MSE,   Y_SE = s$Y_SE,
    W_score   = s$W_score, W_SE = s$W_SE,
    win_Y     = ifelse(s$candidate == meta$winner_y, "*", ""),
    win_W     = ifelse(s$candidate == meta$winner_w, "*", ""),
    stringsAsFactors = FALSE
  )
  names(display)[names(display) == "W_score"] <- w_label

  cat("\n--- autocf nuisance comparison ---\n")
  print(display, row.names = FALSE)

  fmt_thr <- function(t) if (is.na(t)) "n/a" else formatC(t, format = "f", digits = 4)
  cat("\nWinner Y:", meta$winner_y,
      "  swap:", meta$swap_y,
      "  gap:",  formatC(meta$gap_y, format = "f", digits = 4),
      "  threshold:", fmt_thr(meta$threshold_y), "\n")
  cat("Winner W:", meta$winner_w,
      "  swap:", meta$swap_w,
      "  gap:",  formatC(meta$gap_w, format = "f", digits = 4),
      "  threshold:", fmt_thr(meta$threshold_w), "\n")
  cat("Rule: min_improvement = ", deparse(meta$min_improvement),
      "  (swap fires when gap > threshold)\n", sep = "")
  cat("----------------------------------\n\n")
  invisible(NULL)
}


# -- helpers ------------------------------------------------------------------

#' @keywords internal
#' @noRd
.autocf_validate_min_improvement <- function(min_improvement) {
  if (is.character(min_improvement) && length(min_improvement) == 1L) {
    if (!min_improvement %in% c("1se", "0")) {
      stop("`min_improvement` string must be '1se' or '0'.", call. = FALSE)
    }
    return(min_improvement)
  }
  if (is.numeric(min_improvement) && length(min_improvement) == 1L &&
      !is.na(min_improvement) && min_improvement >= 0) {
    return(as.numeric(min_improvement))
  }
  stop("`min_improvement` must be '1se', '0', or a non-negative numeric.",
       call. = FALSE)
}

#' @keywords internal
#' @noRd
.autocf_w_type <- function(W) {
  if (is.factor(W)) {
    if (nlevels(W) == 2L) return("binary")
    stop("Factor W with != 2 levels is not supported by autocf().",
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
              "Encode as 0/1 to use a binary classifier.",
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
.autocf_make_folds <- function(n, K, clusters = NULL) {
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
.autocf_clip_propensity <- function(W.hat, eps = 1e-3, active = TRUE) {
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
.autocf_build_tune_args <- function(c.forest, tuning) {
  tuning <- match.arg(tuning,
                      choices = c("orig", "cf.default", "cf.autotune"))
  if (tuning == "cf.default") return(list())
  if (tuning == "cf.autotune") return(list(tune.parameters = "all"))
  tp <- c.forest$tunable.params
  if (is.null(tp)) return(list())
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
.autocf_filter_pool <- function(pool, weights_nontrivial, verbose = FALSE) {
  out <- pool

  pkg_for <- list(
    grf     = "grf",
    glmnet  = "glmnet",
    tabpfn  = "tabpfn",
    xgboost = c("mlr3", "mlr3learners", "mlr3tuning", "paradox", "xgboost")
  )

  for (cand in pool) {
    need <- pkg_for[[cand]]
    missing <- need[!vapply(need, function(p) {
      requireNamespace(p, quietly = TRUE)
    }, logical(1))]
    if (length(missing) > 0L) {
      warning("autocf: dropping '", cand, "' (missing: ",
              paste(missing, collapse = ", "), ").", call. = FALSE)
      out <- setdiff(out, cand)
    }
  }

  if (weights_nontrivial && "tabpfn" %in% out) {
    warning("autocf: tabpfn cannot accept sample.weights; dropping ",
            "from pool because non-trivial weights are present.",
            call. = FALSE)
    out <- setdiff(out, "tabpfn")
  }

  if ("tabpfn" %in% out &&
      !nzchar(Sys.getenv("TABPFN_TOKEN", unset = ""))) {
    warning("autocf: TABPFN_TOKEN not set; dropping tabpfn from pool.",
            call. = FALSE)
    out <- setdiff(out, "tabpfn")
  }

  out
}

#' @keywords internal
#' @noRd
#' @description
#' Run K-fold cross-fit for a single candidate. Returns OOF Y.hat, W.hat
#' and per-fold weighted MSE for both nuisance roles.
.autocf_run_candidate <- function(cand, X, Y, W, fold, K, w_type, sw, seed,
                                  tabpfn_args, glmnet_args, xgboost_args,
                                  term_evals, verbose) {
  n <- length(Y)
  Y.hat <- numeric(n)
  W.hat <- numeric(n)
  y_fold_loss <- numeric(K)
  w_fold_loss <- numeric(K)

  fit_fn <- switch(cand,
    grf      = .autocf_fp_grf,
    glmnet   = .autocf_fp_glmnet,
    xgboost  = .autocf_fp_xgboost,
    tabpfn   = .autocf_fp_tabpfn,
    stop("Unknown candidate: ", cand, call. = FALSE)
  )

  W_num <- if (w_type == "binary") {
    if (is.factor(W))      as.integer(W) - 1L
    else if (is.logical(W)) as.integer(W)
    else                    as.numeric(W)
  } else {
    as.numeric(W)
  }

  for (k in seq_len(K)) {
    test  <- which(fold == k)
    train <- which(fold != k)
    weights_train <- if (!is.null(sw)) sw[train] else NULL
    if (verbose) message("[autocf:", cand, "] fold ", k, "/", K)

    Y.hat[test] <- fit_fn(
      X_train = X[train, , drop = FALSE], y_train = Y[train],
      X_test  = X[test,  , drop = FALSE],
      family  = "gaussian",
      weights = weights_train,
      tabpfn_args = tabpfn_args,
      glmnet_args = glmnet_args,
      xgboost_args = xgboost_args,
      term_evals  = term_evals,
      seed = seed + k
    )
    W.hat[test] <- fit_fn(
      X_train = X[train, , drop = FALSE], y_train = W[train],
      X_test  = X[test,  , drop = FALSE],
      family  = if (w_type == "binary") "binomial" else "gaussian",
      weights = weights_train,
      tabpfn_args = tabpfn_args,
      glmnet_args = glmnet_args,
      xgboost_args = xgboost_args,
      term_evals  = term_evals,
      seed = seed + K + k
    )

    w_test <- if (!is.null(sw)) sw[test] else rep(1, length(test))
    y_fold_loss[k] <- stats::weighted.mean((Y[test] - Y.hat[test])^2, w_test)
    w_fold_loss[k] <- stats::weighted.mean((W_num[test] - W.hat[test])^2,
                                            w_test)
  }

  list(Y.hat = Y.hat, W.hat = W.hat,
       y_fold_loss = y_fold_loss, w_fold_loss = w_fold_loss)
}

#' @keywords internal
#' @noRd
.autocf_score <- function(fits, pool_run) {
  per_fold_rows <- list()
  summary_rows  <- list()
  for (cand in pool_run) {
    f <- fits[[cand]]
    K <- length(f$y_fold_loss)
    per_fold_rows[[cand]] <- data.frame(
      candidate = cand,
      fold      = seq_len(K),
      y_loss    = f$y_fold_loss,
      w_loss    = f$w_fold_loss,
      stringsAsFactors = FALSE
    )
    summary_rows[[cand]] <- data.frame(
      candidate = cand,
      y_loss = mean(f$y_fold_loss),
      y_se   = if (K >= 2L) stats::sd(f$y_fold_loss) / sqrt(K) else 0,
      w_loss = mean(f$w_fold_loss),
      w_se   = if (K >= 2L) stats::sd(f$w_fold_loss) / sqrt(K) else 0,
      stringsAsFactors = FALSE
    )
  }
  list(
    summary  = do.call(rbind, summary_rows),
    per_fold = do.call(rbind, per_fold_rows)
  )
}

#' @keywords internal
#' @noRd
.autocf_select <- function(scores, baseline, min_improvement) {
  s  <- scores$summary
  pf <- scores$per_fold

  if (!baseline %in% s$candidate) {
    stop("autocf(): baseline '", baseline, "' missing from score table.",
         call. = FALSE)
  }

  decide <- function(target) {
    cand_idx <- which.min(s[[paste0(target, "_loss")]])
    cand <- s$candidate[cand_idx]
    base_loss <- s[s$candidate == baseline, paste0(target, "_loss")]
    cand_loss <- s[s$candidate == cand,    paste0(target, "_loss")]
    gap <- as.numeric(base_loss - cand_loss)

    if (cand == baseline) {
      return(list(winner = baseline, swap = FALSE,
                  gap = 0, threshold = NA_real_))
    }

    threshold <- if (identical(min_improvement, "1se")) {
      base_fold <- pf[pf$candidate == baseline, paste0(target, "_loss")]
      cand_fold <- pf[pf$candidate == cand,    paste0(target, "_loss")]
      diff_fold <- base_fold - cand_fold
      if (length(diff_fold) >= 2L) {
        stats::sd(diff_fold) / sqrt(length(diff_fold))
      } else {
        0
      }
    } else if (identical(min_improvement, "0")) {
      0
    } else {
      as.numeric(min_improvement)
    }

    swap <- isTRUE(gap > threshold)
    list(winner = if (swap) cand else baseline,
         swap = swap, gap = gap, threshold = threshold)
  }

  yres <- decide("y")
  wres <- decide("w")

  list(
    winner_y    = yres$winner, swap_y = yres$swap,
    gap_y       = yres$gap,    threshold_y = yres$threshold,
    winner_w    = wres$winner, swap_w = wres$swap,
    gap_w       = wres$gap,    threshold_w = wres$threshold
  )
}


# -- per-candidate fit/predict adapters --------------------------------------

#' @keywords internal
#' @noRd
.autocf_fp_grf <- function(X_train, y_train, X_test, family, weights = NULL,
                           seed = 1L, ...) {
  set.seed(seed)
  args <- list(X = X_train, Y = as.numeric(y_train), seed = seed)
  if (!is.null(weights)) args$sample.weights <- as.numeric(weights)
  fit <- do.call(grf::regression_forest, args)
  preds <- as.numeric(stats::predict(fit, X_test)$predictions)
  if (family == "binomial") preds <- pmin(pmax(preds, 0), 1)
  preds
}

#' @keywords internal
#' @noRd
.autocf_fp_glmnet <- function(X_train, y_train, X_test, family,
                              weights = NULL, glmnet_args = list(),
                              seed = 1L, ...) {
  if (family == "binomial") {
    if (is.factor(y_train))   y_train <- as.integer(y_train) - 1L
    else if (is.logical(y_train)) y_train <- as.integer(y_train)
    else                       y_train <- as.numeric(y_train)
  } else {
    y_train <- as.numeric(y_train)
  }
  set.seed(seed)
  alpha_grid <- glmnet_args$alpha %||% c(0, 0.25, 0.5, 0.75, 1)
  nfolds     <- glmnet_args$nfolds %||% 10L
  nfolds     <- max(3L, min(as.integer(nfolds), length(y_train)))
  foldid     <- sample(rep(seq_len(nfolds), length.out = length(y_train)))

  ga <- glmnet_args
  ga$alpha <- NULL; ga$nfolds <- NULL; ga$foldid <- NULL
  ga$family <- NULL; ga$relax <- NULL
  ga$x <- NULL; ga$y <- NULL; ga$weights <- NULL

  best_fit <- NULL; best_cvm <- Inf
  for (a in alpha_grid) {
    cv_args <- c(
      list(x = X_train, y = y_train, alpha = a, family = family,
           relax = TRUE, foldid = foldid),
      if (!is.null(weights)) list(weights = as.numeric(weights)),
      ga
    )
    fit <- tryCatch(do.call(glmnet::cv.glmnet, cv_args),
                    error = function(e) NULL)
    if (is.null(fit)) next
    cvm <- min(fit$cvm, na.rm = TRUE)
    if (is.finite(cvm) && cvm < best_cvm) {
      best_cvm <- cvm; best_fit <- fit
    }
  }
  if (is.null(best_fit)) {
    stop("autocf(): all cv.glmnet alpha candidates failed.", call. = FALSE)
  }

  pr_type <- if (family == "binomial") "response" else "link"
  as.numeric(stats::predict(best_fit, newx = X_test,
                            s = "lambda.min", type = pr_type))
}

#' @keywords internal
#' @noRd
.autocf_fp_xgboost <- function(X_train, y_train, X_test, family,
                               weights = NULL, xgboost_args = list(),
                               term_evals = 10L, seed = 1L, ...) {
  set.seed(seed)

  if (family == "binomial") {
    y_factor <- if (is.factor(y_train)) {
      y_train
    } else if (is.logical(y_train)) {
      factor(y_train, levels = c("FALSE", "TRUE"))
    } else {
      factor(as.integer(y_train), levels = c("0", "1"))
    }
    pos <- levels(y_factor)[2L]
    train_df <- data.frame(X_train, .y = y_factor)
    backend <- mlr3::as_data_backend(train_df)
    task <- mlr3::TaskClassif$new(id = "autocf_xgb", backend = backend,
                                   target = ".y", positive = pos)
    learner <- mlr3::lrn("classif.xgboost", predict_type = "prob",
                         nrounds = 200L)
    measure <- mlr3::msr("classif.bbrier")
  } else {
    train_df <- data.frame(X_train, .y = as.numeric(y_train))
    backend <- mlr3::as_data_backend(train_df)
    task <- mlr3::TaskRegr$new(id = "autocf_xgb", backend = backend,
                                target = ".y")
    learner <- mlr3::lrn("regr.xgboost", nrounds = 200L)
    measure <- mlr3::msr("regr.mse")
  }

  # Forward user-supplied xgboost params (e.g. nthread, device = "cuda",
  # tree_method = "hist") to the underlying xgboost call. Tuned params
  # below override matching keys.
  if (length(xgboost_args) > 0L) {
    for (nm in names(xgboost_args)) {
      learner$param_set$values[[nm]] <- xgboost_args[[nm]]
    }
  }

  if (!is.null(weights)) {
    wdf <- data.frame(.w = as.numeric(weights))
    task$cbind(wdf)
    if ("weights_learner" %in% mlr3::mlr_reflections$task_col_roles[[task$task_type]]) {
      task$set_col_roles(".w", roles = "weights_learner")
    } else {
      task$set_col_roles(".w", roles = "weight")
    }
  }

  if (term_evals > 0L) {
    search_space <- paradox::ps(
      eta        = paradox::p_dbl(0.01, 0.3),
      max_depth  = paradox::p_int(2L, 8L),
      nrounds    = paradox::p_int(50L, 500L),
      subsample  = paradox::p_dbl(0.5, 1.0)
    )
    at <- mlr3tuning::auto_tuner(
      tuner       = mlr3tuning::tnr("random_search"),
      learner     = learner,
      resampling  = mlr3::rsmp("cv", folds = 3L),
      measure     = measure,
      term_evals  = term_evals,
      search_space = search_space
    )
    at$train(task)
    fit <- at
  } else {
    learner$train(task)
    fit <- learner
  }

  test_df <- as.data.frame(X_test)
  pred <- fit$predict_newdata(test_df)

  if (family == "binomial") {
    as.numeric(pred$prob[, pos])
  } else {
    as.numeric(pred$response)
  }
}

#' @keywords internal
#' @noRd
.autocf_fp_tabpfn <- function(X_train, y_train, X_test, family,
                              weights = NULL, tabpfn_args = list(),
                              seed = 1L, ...) {
  if (!is.null(weights) && length(unique(weights)) > 1L) {
    warning("autocf: tabpfn ignores sample.weights; ",
            "fold trains unweighted.", call. = FALSE)
  }

  if (family == "binomial") {
    pos <- if (is.factor(y_train))      as.character(levels(y_train)[2L])
           else if (is.logical(y_train)) "TRUE"
           else                          "1"
    if (is.factor(y_train)) {
      # leave alone
    } else if (is.logical(y_train)) {
      y_train <- factor(y_train, levels = c("FALSE", "TRUE"))
    } else {
      y_train <- factor(as.integer(y_train), levels = c("0", "1"))
    }
  } else {
    y_train <- as.numeric(y_train)
  }

  fold_args <- tabpfn_args
  if (!"control" %in% names(fold_args)) {
    fold_args$control <- tabpfn::control_tab_pfn(
      random_state = as.integer(seed)
    )
  }

  fit <- do.call(tabpfn::tab_pfn,
                 c(list(x = X_train, y = y_train), fold_args))
  pr <- stats::predict(fit, X_test)

  if (family == "binomial") {
    col <- paste0(".pred_", pos)
    if (!is.data.frame(pr) || !(col %in% names(pr))) {
      stop("autocf(): tabpfn classifier predict() lacks column '", col,
           "'.", call. = FALSE)
    }
    return(as.numeric(pr[[col]]))
  }
  if (is.data.frame(pr) && ".pred" %in% names(pr)) return(as.numeric(pr$.pred))
  if (is.numeric(pr)) return(as.numeric(pr))
  stop("autocf(): unexpected tabpfn predict() return shape: ",
       paste(class(pr), collapse = "/"), call. = FALSE)
}

# Lightweight null-coalesce; defined locally so we don't rely on rlang's `%||%`
# being attached.
`%||%` <- function(a, b) if (is.null(a)) b else a
