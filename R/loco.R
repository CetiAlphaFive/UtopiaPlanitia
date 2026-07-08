#' LOCO Variable Importance for Outcome Models
#'
#' Measures how much each covariate contributes to an outcome
#' (non-causal) forest by dropping it and seeing how much prediction
#' error grows. Works with `ranger` regression, probability, and
#' classification forests, and with `grf` regression, boosted
#' regression, and probability forests. For a `grf::causal_forest()`
#' use [cf_loco()] instead; other model types are rejected.
#'
#' Two modes:
#'
#' * **Split-sample** (`split = TRUE`, default): splits the data,
#'   refits, and reports importance scores with confidence intervals
#'   and one-sided p-values. Slower, but gives valid inference.
#' * **OOB** (`split = FALSE`): refits once per covariate (or group)
#'   and compares out-of-bag error to the full model. Faster, but
#'   returns point estimates only -- no confidence intervals or
#'   p-values.
#'
#' @param model A fitted outcome forest. Supported classes:
#'   [ranger::ranger()] (regression, probability, or classification),
#'   [grf::regression_forest()],
#'   [grf::boosted_regression_forest()], or
#'   [grf::probability_forest()]. Survival, causal, quantile,
#'   instrumental, multi-arm, and `lm_forest` objects are rejected.
#' @param data Optional. A data frame containing the variables used to
#'   fit `model`. Only meaningful for ranger models. If `NULL`
#'   (default), `loco()` tries to recover the training data from
#'   `model$call` by evaluating in `parent.frame()`. Pass `data`
#'   explicitly when calling `loco()` from a different scope than the
#'   original fit (e.g. inside another function). For grf models the
#'   training data is read from `model$X.orig` / `model$Y.orig` (or,
#'   for boosted forests, `model$forests[[1]]$X.orig` /
#'   `model$forests[[1]]$Y.orig`); any user-supplied `data` is ignored
#'   with a warning.
#' @param alpha Significance level for split-mode confidence intervals.
#'   Default is `0.1` (90% intervals). Only used when `split = TRUE`.
#' @param split Logical. If `TRUE` (default), uses split-sample LOCO. If
#'   `FALSE`, uses OOB prediction-error differences (no inference).
#' @param method One of `"z"` (default) or `"wilcox"`. Only used when
#'   `split = TRUE`. `"z"` uses a normal-theory Z-test on the mean of
#'   loss-residual differences; `"wilcox"` uses a Wilcoxon signed-rank
#'   test on the same differences.
#' @param loss One of `"auto"` (default), `"abs"`, `"mse"`, `"brier"`,
#'   `"zero_one"`, or `"log"`. Loss function used to define the
#'   per-observation residual. `"auto"` resolves at runtime based on
#'   the forest type:
#'   * Regression (ranger or grf) -> `"abs"` (absolute deviation).
#'   * Probability estimation (ranger or grf) -> `"brier"` (multi-class
#'     Brier score).
#'   * Classification (ranger only) -> `"zero_one"` (misclassification).
#'
#'   Disallowed combinations raise an error. In particular, `"brier"`
#'   and `"log"` require a probability forest.
#' @param groups Optional. Specifies group-LOCO. If `NULL` (default),
#'   per-variable LOCO is performed (one row per predictor). If a
#'   character vector, treated as a single (unnamed) group dropped
#'   jointly. If a named list of character vectors, each element is
#'   one group dropped jointly. Groups must not overlap, must contain
#'   only known predictor names, must be non-empty, and must leave at
#'   least one predictor remaining after the drop.
#' @param bonf.correct Logical. If `TRUE` (default), p-values and
#'   confidence intervals are Bonferroni-corrected across all tested
#'   variables / groups.
#' @param seed Integer seed for reproducibility. Default is `1995`.
#'   Honored in both split and OOB modes.
#' @param verbose Logical. Retained for backward compatibility; no
#'   longer used. Default is `FALSE`. Formerly controlled progress
#'   printing from a conformal-inference code path that has been
#'   removed; any value is accepted and has no effect on the
#'   computation or the return value.
#' @param cross.fit Logical. If `TRUE`, uses K-fold cross-fitting
#'   instead of a single 50/50 split, so every observation contributes
#'   to the importance estimate exactly once (across the `num.folds`
#'   held-out folds) rather than only the held-out half. Default
#'   `FALSE` (single split, exactly as before). Only used when
#'   `split = TRUE`; requires `method = "z"` (errors otherwise --
#'   Wilcoxon signed-rank validity assumes independent,
#'   symmetric-under-null differences, which cross-fitted folds
#'   violate) and errors if `split = FALSE` (cross-fitting only applies
#'   to split-sample LOCO, not OOB mode). Inference uses the
#'   Nadeau-Bengio corrected t-test at `num.folds - 1` degrees of
#'   freedom (mirroring [cf_perm()]'s `cross.fit = TRUE` path), because
#'   the `num.folds` per-fold statistics are not independent (models
#'   share overlapping training folds). Note the extra compute cost:
#'   `num.folds * (p + 1)` model refits instead of `p + 1`.
#' @param num.folds Integer. Number of cross-fit folds used when
#'   `cross.fit = TRUE`; ignored otherwise. Default `5`. Must satisfy
#'   `2 <= num.folds < n`; in probability/classification mode, folds
#'   are additionally stratified by class and `num.folds` must not
#'   exceed the smallest class count (plain random folds in regression
#'   mode).
#'
#' @return An object of class `"loco_vimp"` with components:
#'   \describe{
#'     \item{vimp}{Data frame sorted by descending importance, with columns
#'       `Variable` (covariate or group name), `Importance` (LOCO importance
#'       score), `CI.lower`, `CI.upper` (confidence interval bounds; `NA` in
#'       OOB mode), `p.value` (one-sided p-value testing H0: importance
#'       \eqn{\le} 0; `NA` in OOB mode), and, only when `groups` is supplied,
#'       `Members` (list-column of character vectors naming each group's
#'       members).}
#'     \item{n}{Sample size.}
#'     \item{p}{Number of covariates in the fitted model (not the number of
#'       groups, when `groups` is supplied).}
#'     \item{method}{`"z"`, `"wilcox"`, or `"oob"`.}
#'     \item{loss}{Loss function used (`"abs"`, `"mse"`, `"brier"`,
#'       `"zero_one"`, or `"log"`).}
#'     \item{split}{Logical; whether split-sample mode was used.}
#'     \item{alpha}{The `alpha` argument value.}
#'     \item{bonf.correct}{The `bonf.correct` argument value.}
#'     \item{backend}{The detected model backend
#'       (`"ranger"`, `"grf_reg"`, `"grf_brf"`, or `"grf_prob"`).}
#'     \item{group}{Logical; whether group-LOCO was used.}
#'     \item{cross.fit}{Logical; whether K-fold cross-fitting was used
#'       (always `FALSE` unless explicitly requested via
#'       `cross.fit = TRUE`).}
#'     \item{num.folds}{Integer number of cross-fit folds, or `NA` when
#'       `cross.fit` is `FALSE`.}
#'   }
#'
#' @details
#' **One-sided p-values.** The split-mode tests are one-sided against
#' the null that the covariate / group has no incremental predictive
#' value. Small p-values indicate the covariate helps prediction.
#'
#' **Loss / forest-type compatibility.**
#' \describe{
#'   \item{Regression (ranger or grf)}{`"abs"` (default) or `"mse"`.}
#'   \item{Probability estimation (ranger or grf)}{`"brier"` (default),
#'     `"zero_one"` (argmax then 0/1), or `"log"` (negative
#'     log-likelihood, clipped at 1e-12).}
#'   \item{Classification (ranger only)}{`"zero_one"` only. To use
#'     Brier or log-loss, refit with `probability = TRUE` (ranger) or
#'     use [grf::probability_forest()].}
#' }
#'
#' **Cross-fit LOCO (`cross.fit = TRUE`).** Only applies to split-sample
#' mode (`split = TRUE`); `split = FALSE` (OOB) errors, since
#' cross-fitting is a refinement of split-sample inference, not of OOB
#' scoring. `method = "wilcox"` also errors under `cross.fit = TRUE`:
#' the Wilcoxon signed-rank test's validity assumes independent,
#' symmetric-under-null differences, an assumption cross-fitted folds
#' (which share overlapping training data) violate. Use
#' `method = "z"` (the default) instead.
#'
#' **Group LOCO.** When `groups` is supplied, importance is computed
#' jointly for each group: the reduced model omits all members of the
#' group simultaneously. Useful when (a) factor predictors have been
#' one-hot-encoded into multiple columns whose individual importances
#' are not the quantity of interest, (b) an index or scale is
#' represented by several items, or (c) interest is in the contribution
#' of a theoretically motivated block. Group names appear in the
#' `variable` column; member lists appear in the `members` column.
#'
#' **Why no OOB inference?** A naive Wald-style Z-test on
#' per-observation OOB error differences is anti-conservative: OOB
#' residuals are positively dependent across trees because each unit
#' appears out-of-bag for an overlapping set of trees. Small simulation
#' checks (n=200, p=4, 20 reps) at alpha=0.10 give roughly 30% Type-I
#' error.
#'
#' **Hyperparameter replay (ranger).** Core hyperparameters
#' (`num.trees`, `mtry`, `min.node.size`, `splitrule`, `replace`,
#' `max.depth`) are read directly from the fitted model object.
#' Less-common arguments (e.g. `sample.fraction`,
#' `respect.unordered.factors`) are pulled from the original call by
#' evaluating in `parent.frame()`; if any cannot be resolved they are
#' silently dropped with a single warning and ranger's defaults are
#' used in their place.
#'
#' **Hyperparameter replay (grf).** Replay reads from
#' `model$tunable.params` plus stored scalars: `num.trees`,
#' `sample.fraction`, `mtry`, `min.node.size`, `honesty.fraction`,
#' `honesty.prune.leaves`, `alpha`, `imbalance.penalty`,
#' `ci.group.size`, `clusters`, and `equalize.cluster.weights`. The
#' `honesty` flag itself is not stored on a fitted grf forest; refits
#' use grf's default (`TRUE`). `tune.parameters` is forced to `"none"`
#' on refit (no re-tuning). For
#' [grf::boosted_regression_forest()], `boost.steps` is replayed as
#' `length(model$forests)` and `boost.error.reduction` reverts to
#' grf's default (`0.97`); exact replay is therefore approximate.
#' `sample.weights` are not preserved.
#'
#' **Edge cases.**
#' * Survival forests are rejected (both backends).
#' * Causal / quantile / instrumental / multi-arm / lm_forest
#'   (grf) are rejected; use [cf_loco()] for `causal_forest`.
#' * Factor predictors are allowed in ranger OOB mode but rejected in
#'   split mode because split-LOCO requires a numeric matrix. grf
#'   forests already require numeric X.
#' * Single-predictor models are rejected: LOCO requires \eqn{p \ge 2}.
#' * Groups that would leave zero remaining predictors are rejected.
#'
#' @references
#' Lei, J., G'Sell, M., Rinaldo, A., Tibshirani, R. J., & Wasserman, L.
#' (2018). Distribution-Free Predictive Inference for Regression.
#' *Journal of the American Statistical Association*, 113(523),
#' 1094--1111. \doi{10.1080/01621459.2017.1307116}
#'
#' Rinaldo, A., Wasserman, L., & G'Sell, M. (2019). Bootstrapping and
#' Sample Splitting for High-Dimensional, Assumption-Lean Inference.
#' *Annals of Statistics*, 47(6), 3438--3469.
#' \doi{10.1214/18-AOS1820}
#'
#' Williamson, B. D., Gilbert, P. B., Carone, M., & Simon, N. (2021).
#' Nonparametric variable importance assessment based on
#' generalizations of R^2. *Journal of the American Statistical
#' Association*, 116(536), 1574--1587.
#' \doi{10.1080/01621459.2020.1812596}
#'
#' @seealso [cf_loco()] for LOCO importance tailored to causal forests;
#'   [print.loco_vimp()], [summary.loco_vimp()], [plot.loco_vimp()] for the
#'   object's S3 methods; [ranger::ranger()], [grf::regression_forest()],
#'   [grf::boosted_regression_forest()],
#'   [grf::probability_forest()] for supported model fitters.
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("ranger", quietly = TRUE)) {
#'   set.seed(1995)
#'   dat <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100),
#'                     x3 = rnorm(100))
#'   mod <- ranger::ranger(y ~ ., data = dat, num.trees = 50)
#'   loco(mod, split = FALSE)
#'   loco(mod, split = FALSE,
#'        groups = list(g1 = c("x1","x2"), g2 = "x3"))
#'   set.seed(2026)
#'   dat2 <- data.frame(y = factor(sample(0:1, 100, replace = TRUE)),
#'                      x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
#'   mod_prob <- ranger::ranger(y ~ ., data = dat2, num.trees = 50,
#'                              probability = TRUE)
#'   loco(mod_prob, split = FALSE)            # auto -> "brier"
#'   loco(mod_prob, split = FALSE, loss = "log")
#' }
#' if (requireNamespace("grf", quietly = TRUE)) {
#'   set.seed(1995)
#'   X <- matrix(rnorm(100 * 3), 100, 3)
#'   colnames(X) <- c("x1", "x2", "x3")
#'   Y <- X[, 1] + 0.5 * X[, 2] + rnorm(100, sd = 0.5)
#'   rf <- grf::regression_forest(X, Y, num.trees = 100)
#'   vi <- loco(rf, split = FALSE)            # auto -> "abs"
#'   summary(vi)
#'   if (requireNamespace("ggplot2", quietly = TRUE)) plot(vi)
#'   Yf <- factor(rbinom(100, 1, plogis(X[, 1])))
#'   pf <- grf::probability_forest(X, Yf, num.trees = 100)
#'   loco(pf, split = FALSE)                  # auto -> "brier"
#'
#'   # K-fold cross-fit split-sample LOCO: uses every observation once
#'   # (across folds) instead of only the held-out half of a single
#'   # split, at the cost of num.folds * (p + 1) model refits.
#'   loco(rf, split = TRUE, cross.fit = TRUE, num.folds = 5)
#' }
#' }
loco <- function(model,
                 data = NULL,
                 alpha = 0.1,
                 split = TRUE,
                 method = c("z", "wilcox"),
                 loss = c("auto", "abs", "mse", "brier", "zero_one", "log"),
                 groups = NULL,
                 bonf.correct = TRUE,
                 seed = 1995,
                 verbose = FALSE,
                 cross.fit = FALSE,
                 num.folds = 5L) {

  ## -- backend dispatch --------------------------------------------------
  backend <- detect_backend(model)

  method <- match.arg(method)
  if (cross.fit && identical(method, "wilcox")) {
    stop("loco(): cross.fit = TRUE does not support method = \"wilcox\" ",
         "(Wilcoxon signed-rank validity assumes independent, symmetric-under-null ",
         "differences, which cross-fitted folds violate). Use method = \"z\".",
         call. = FALSE)
  }
  if (cross.fit && !split) {
    stop("loco(): cross.fit = TRUE requires split = TRUE ",
         "(cross-fitting only applies to split-sample LOCO, not OOB mode). ",
         "Use split = TRUE, or cross.fit = FALSE for OOB mode.",
         call. = FALSE)
  }
  loss   <- match.arg(loss)
  stopifnot(is.logical(split), length(split) == 1L, !is.na(split))
  stopifnot(is.logical(bonf.correct), length(bonf.correct) == 1L,
            !is.na(bonf.correct))
  stopifnot(is.numeric(alpha), length(alpha) == 1L,
            alpha > 0, alpha < 1)
  stopifnot(is.numeric(seed), length(seed) == 1L, !is.na(seed))
  stopifnot(is.logical(cross.fit), length(cross.fit) == 1L, !is.na(cross.fit))
  stopifnot(is.numeric(num.folds), length(num.folds) == 1L, !is.na(num.folds))

  if (backend == "ranger") {
    rlang::check_installed("ranger",
                           reason = "to fit / refit ranger models in `loco()`.")
  } else {
    rlang::check_installed("grf",
                           reason = "to refit grf forests in `loco()`.")
  }

  ## -- treetype + loss compatibility -------------------------------------
  tt <- treetype_for_backend(model, backend)
  if (identical(tt, "Survival")) {
    stop("loco() does not support survival forests.", call. = FALSE)
  }
  if (!tt %in% c("Regression", "Probability estimation", "Classification")) {
    stop("loco(): unrecognized treetype '", tt, "'.", call. = FALSE)
  }

  if (identical(loss, "auto")) {
    loss <- switch(tt,
                   "Regression"             = "abs",
                   "Probability estimation" = "brier",
                   "Classification"         = "zero_one")
  }

  reg_losses  <- c("abs", "mse")
  prob_losses <- c("brier", "zero_one", "log")
  clf_losses  <- c("zero_one")
  if (tt == "Regression" && !loss %in% reg_losses) {
    stop("loss = '", loss, "' is not valid for a regression forest. ",
         "Use one of: ", paste(reg_losses, collapse = ", "), ".",
         call. = FALSE)
  }
  if (tt == "Probability estimation" && !loss %in% prob_losses) {
    stop("loss = '", loss, "' is not valid for a probability forest. ",
         "Use one of: ", paste(prob_losses, collapse = ", "), ".",
         call. = FALSE)
  }
  if (tt == "Classification" && !loss %in% clf_losses) {
    stop("loss = '", loss, "' is not valid for a (hard) classification ",
         "forest. Only 'zero_one' is supported. ",
         "To use 'brier' or 'log', refit with `probability = TRUE` ",
         "(ranger) or use grf::probability_forest().",
         call. = FALSE)
  }

  ## -- variable names ----------------------------------------------------
  pred.names <- pred_names_for(model, backend)
  if (length(pred.names) < 2L) {
    stop("loco() requires at least two predictors; the supplied model has ",
         length(pred.names), ".", call. = FALSE)
  }

  ## -- recover training data ---------------------------------------------
  if (backend == "ranger") {
    resp.name  <- model$dependent.variable.name
    if (is.null(resp.name) || !nzchar(resp.name)) {
      stop("Cannot determine the response variable name from `model`. ",
           "This typically happens when the original model was fit with ",
           "`ranger(x = X, y = y)` (no formula / no `dependent.variable.name`). ",
           "Re-fit using the formula interface, or via ",
           "`ranger(x = X, y = y, dependent.variable.name = \"<name>\")` ",
           "and ensure a column of that name is present in `data`.",
           call. = FALSE)
    }
    cl <- model$call
    if (is.null(data)) {
      train.data <- tryCatch(
        eval(cl$data, envir = parent.frame()),
        error = function(e) NULL
      )
      if (is.null(train.data)) {
        stop("Cannot recover training data from `model$call`. ",
             "Pass the original data frame via the `data` argument, e.g. ",
             "`loco(model, data = my_training_df)`.",
             call. = FALSE)
      }
    } else {
      train.data <- as.data.frame(data)
    }
    missing.cols <- setdiff(c(pred.names, resp.name), names(train.data))
    if (length(missing.cols) > 0L) {
      stop("Supplied `data` is missing required columns: ",
           paste(missing.cols, collapse = ", "),
           ". `data` must contain the response and all predictor columns.",
           call. = FALSE)
    }
  } else {
    ## grf: read X / Y from the fitted object; warn if user passed data.
    if (!is.null(data)) {
      warning(
        "`data` is ignored for grf models; using model$X.orig / model$Y.orig.",
        call. = FALSE
      )
    }
    X.grf <- X_orig_for(model, backend)
    Y.grf <- Y_orig_for(model, backend)
    if (is.data.frame(X.grf)) {
      non_num <- !vapply(X.grf, is.numeric, logical(1))
      if (any(non_num)) {
        stop("loco(): grf model's X.orig has non-numeric column(s): ",
             paste(names(X.grf)[non_num], collapse = ", "),
             ". grf does not support non-numeric features; one-hot-encode ",
             "or numerically code these before fitting.",
             call. = FALSE)
      }
    } else if (!is.matrix(X.grf) || !is.numeric(X.grf)) {
      stop("loco(): grf model's stored X.orig is not a numeric matrix or ",
           "data frame. This should not happen with a normally-fitted grf forest.",
           call. = FALSE)
    }
    resp.name  <- "Y"
    if (resp.name %in% pred.names) resp.name <- ".Y_loco_internal"
    train.data <- as.data.frame(X.grf, stringsAsFactors = FALSE)
    names(train.data) <- pred.names
    train.data[[resp.name]] <- Y.grf
  }

  ## -- validate groups ---------------------------------------------------
  group_mode <- !is.null(groups)
  if (group_mode) {
    if (is.character(groups)) {
      groups <- list(group1 = groups)
    }
    if (!is.list(groups)) {
      stop("`groups` must be NULL, a character vector, or a named list ",
           "of character vectors.", call. = FALSE)
    }
    gn <- names(groups)
    if (is.null(gn)) gn <- rep("", length(groups))
    blank <- !nzchar(gn)
    if (any(blank)) gn[blank] <- paste0("group", seq_along(groups)[blank])
    if (anyDuplicated(gn)) {
      stop("`groups` has duplicated names: ",
           paste(unique(gn[duplicated(gn)]), collapse = ", "), ".",
           call. = FALSE)
    }
    names(groups) <- gn

    if (any(vapply(groups, function(g) !is.character(g) || length(g) == 0L,
                   logical(1)))) {
      stop("Every group must be a non-empty character vector.",
           call. = FALSE)
    }
    all_members <- unlist(groups, use.names = FALSE)
    unknown <- setdiff(all_members, pred.names)
    if (length(unknown) > 0L) {
      stop("Group(s) reference predictor name(s) not in the fitted model: ",
           paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    if (anyDuplicated(all_members)) {
      dups <- unique(all_members[duplicated(all_members)])
      stop("Group members must not overlap. Duplicated: ",
           paste(dups, collapse = ", "), ".", call. = FALSE)
    }
    leaves_some <- vapply(groups,
                          function(g) length(setdiff(pred.names, g)) >= 1L,
                          logical(1))
    if (!all(leaves_some)) {
      bad <- names(groups)[!leaves_some]
      stop("Group(s) ", paste(bad, collapse = ", "),
           " would leave zero predictors; cannot drop all predictors.",
           call. = FALSE)
    }
  }

  ## -- recover hyperparameters -------------------------------------------
  if (backend == "ranger") {
    hp.args <- list(
      num.trees      = model$num.trees,
      mtry           = model$mtry,
      min.node.size  = model$min.node.size,
      splitrule      = model$splitrule,
      replace        = model$replace,
      max.depth      = model$max.depth,
      importance     = "none"
    )
    hp.args <- hp.args[!vapply(hp.args, is.null, logical(1))]
    if (tt == "Probability estimation") hp.args$probability <- TRUE

    cl <- model$call
    cl.canon <- tryCatch(
      match.call(definition = ranger::ranger, call = cl),
      error = function(e) cl
    )
    raw.args <- as.list(cl.canon)[-1L]
    arg.names <- names(raw.args)
    if (is.null(arg.names)) arg.names <- rep("", length(raw.args))
    drop <- c(
      "formula", "data", "x", "y",
      "dependent.variable.name", "case.weights",
      "class.weights", "status.variable.name",
      "num.trees", "mtry", "min.node.size", "splitrule",
      "replace", "max.depth", "importance",
      "write.forest", "verbose", "num.threads", "seed",
      "keep.inbag", "inbag", "holdout", "oob.error",
      "save.memory", "probability", "quantreg",
      "node.stats", "local.importance", "classification"
    )
    keep.idx <- which(!arg.names %in% drop & arg.names != "")
    extra.args <- raw.args[keep.idx]
    caller.env <- parent.frame()
    if (length(extra.args) > 0L) {
      extra.eval <- lapply(extra.args, function(a) {
        tryCatch(list(ok = TRUE,  value = eval(a, envir = caller.env)),
                 error = function(e) list(ok = FALSE, value = NULL,
                                          msg = conditionMessage(e)))
      })
      ok <- vapply(extra.eval, `[[`, logical(1), "ok")
      if (!all(ok)) {
        bad <- names(extra.args)[!ok]
        warning(
          "loco(): ignoring ranger argument(s) that could not be resolved ",
          "from the fitted model or the caller environment: ",
          paste(bad, collapse = ", "),
          ". The reduced-model refits will use ranger defaults for these. ",
          "Pass literal values when fitting if this matters.",
          call. = FALSE
        )
      }
      for (nm in names(extra.args)[ok]) {
        hp.args[[nm]] <- extra.eval[[nm]]$value
      }
    }
  } else {
    hp.args <- extract_grf_hp(model, backend)
  }

  ## Build the units we iterate over.
  if (group_mode) {
    targets       <- groups
    target.names  <- names(groups)
  } else {
    targets       <- as.list(pred.names)
    names(targets) <- pred.names
    target.names  <- pred.names
  }
  G <- length(targets)

  ## -- split mode --------------------------------------------------------
  if (split) {
    if (backend == "ranger") {
      factor.cols <- pred.names[vapply(train.data[, pred.names, drop = FALSE],
                                       is.factor, logical(1))]
      if (length(factor.cols) > 0L) {
        stop("split = TRUE does not support factor predictors (",
             paste(factor.cols, collapse = ", "),
             "); split-LOCO requires a numeric matrix. ",
             "Use split = FALSE, or one-hot-encode factors before fitting.",
             call. = FALSE)
      }
      x <- as.matrix(train.data[, pred.names, drop = FALSE])
      storage.mode(x) <- "double"

      y_train <- train.data[[resp.name]]
      if (tt %in% c("Probability estimation", "Classification") &&
          !is.factor(y_train)) {
        y_train <- as.factor(y_train)
      }
      y_levels <- if (is.factor(y_train)) levels(y_train) else NULL

      if (cross.fit) {
        n <- nrow(x)
        if (num.folds < 2L || num.folds >= n) {
          stop("loco(): num.folds must be between 2 and n - 1 when cross.fit = TRUE ",
               "(n = ", n, ", num.folds = ", num.folds, ").", call. = FALSE)
        }
        if (tt %in% c("Probability estimation", "Classification")) {
          class_tab <- table(y_train)
          if (num.folds > min(class_tab)) {
            stop("loco(): num.folds (", num.folds, ") cannot exceed the smallest class ",
                 "size (", min(class_tab), ") for stratified cross-fitting.",
                 call. = FALSE)
          }
        }
        return(.loco_cv(
          x = x, y_train = y_train, y_levels = y_levels,
          pred.names = pred.names, targets = targets,
          target.names = target.names, group_mode = group_mode,
          hp.args = hp.args, loss = loss, tt = tt,
          alpha = alpha, bonf.correct = bonf.correct,
          seed = seed, backend = backend, num.folds = num.folds
        ))
      }
      ## All ranger split-mode cases use the custom split loop.
      return(loco_custom_split(
        x = x, y_train = y_train, y_levels = y_levels,
        pred.names = pred.names, targets = targets,
        target.names = target.names, group_mode = group_mode,
        hp.args = hp.args, loss = loss, tt = tt,
        method = method, alpha = alpha, bonf.correct = bonf.correct,
        seed = seed, backend = backend
      ))
    }

    ## grf split mode: always use loco_custom_split with grf fit/predict.
    x <- as.matrix(train.data[, pred.names, drop = FALSE])
    storage.mode(x) <- "double"
    y_train <- train.data[[resp.name]]
    if (tt == "Probability estimation" && !is.factor(y_train)) {
      y_train <- as.factor(y_train)
    }
    y_levels <- if (is.factor(y_train)) levels(y_train) else NULL

    if (cross.fit) {
      n <- nrow(x)
      if (num.folds < 2L || num.folds >= n) {
        stop("loco(): num.folds must be between 2 and n - 1 when cross.fit = TRUE ",
             "(n = ", n, ", num.folds = ", num.folds, ").", call. = FALSE)
      }
      if (tt %in% c("Probability estimation", "Classification")) {
        class_tab <- table(y_train)
        if (num.folds > min(class_tab)) {
          stop("loco(): num.folds (", num.folds, ") cannot exceed the smallest class ",
               "size (", min(class_tab), ") for stratified cross-fitting.",
               call. = FALSE)
        }
      }
      return(.loco_cv(
        x = x, y_train = y_train, y_levels = y_levels,
        pred.names = pred.names, targets = targets,
        target.names = target.names, group_mode = group_mode,
        hp.args = hp.args, loss = loss, tt = tt,
        alpha = alpha, bonf.correct = bonf.correct,
        seed = seed, backend = backend, num.folds = num.folds
      ))
    }
    return(loco_custom_split(
      x = x, y_train = y_train, y_levels = y_levels,
      pred.names = pred.names, targets = targets,
      target.names = target.names, group_mode = group_mode,
      hp.args = hp.args, loss = loss, tt = tt,
      method = method, alpha = alpha, bonf.correct = bonf.correct,
      seed = seed, backend = backend
    ))
  }

  ## -- OOB mode ----------------------------------------------------------
  baseline.error <- oob_loss(model, train.data, resp.name, pred.names,
                             loss = loss, tt = tt, backend = backend)
  x.full <- train.data[, pred.names, drop = FALSE]
  y.full <- train.data[[resp.name]]
  if (backend == "ranger") {
    if (tt %in% c("Probability estimation", "Classification") &&
        !is.factor(y.full)) {
      y.full <- as.factor(y.full)
    }
  } else {
    if (tt == "Probability estimation" && !is.factor(y.full)) {
      y.full <- as.factor(y.full)
    }
  }

  old.seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old.seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old.seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  importance <- numeric(G)
  names(importance) <- target.names

  for (i in seq_len(G)) {
    drop_members <- as.character(targets[[i]])
    keep <- setdiff(pred.names, drop_members)

    if (backend == "ranger") {
      args <- hp.args
      args$x <- x.full[, keep, drop = FALSE]
      args$y <- y.full
      if (!is.null(args$mtry)) {
        args$mtry <- min(as.integer(args$mtry), length(keep))
      }
      reduced.model <- do.call(ranger::ranger, args)
    } else {
      reduced.model <- grf_refit(backend, hp.args,
                                 X = as.matrix(x.full[, keep, drop = FALSE]),
                                 Y = y.full,
                                 seed = seed + i)
    }
    importance[i] <- oob_loss(reduced.model, train.data, resp.name, keep,
                              loss = loss, tt = tt, backend = backend) -
      baseline.error
  }

  .new_loco(
    variable = target.names,
    importance = as.numeric(importance),
    members = if (group_mode) lapply(targets, as.character) else NULL,
    n = nrow(train.data), p = length(pred.names),
    method = "oob", loss = loss,
    split = FALSE, alpha = alpha, bonf.correct = bonf.correct,
    backend = backend, group = group_mode
  )
}


## --- Internal helpers ---------------------------------------------------

## Build the classed "loco_vimp" return object. Single funnel point for both
## return sites (OOB loop and loco_custom_split()) so the object shape only
## has to be defined once.
.new_loco <- function(variable, importance,
                      ci.lower = NA_real_, ci.upper = NA_real_,
                      p.value = NA_real_, members = NULL,
                      n, p, method, loss, split, alpha, bonf.correct,
                      backend, group,
                      cross.fit = FALSE, num.folds = NA_integer_) {
  vimp <- data.frame(
    Variable   = variable,
    Importance = as.numeric(importance),
    CI.lower   = as.numeric(ci.lower),
    CI.upper   = as.numeric(ci.upper),
    p.value    = as.numeric(p.value),
    stringsAsFactors = FALSE
  )
  if (isTRUE(group)) {
    vimp$Members <- I(members)
  }
  vimp <- vimp[order(-vimp$Importance), , drop = FALSE]
  row.names(vimp) <- NULL

  structure(
    list(
      vimp = vimp,
      n = as.integer(n), p = as.integer(p),
      method = method, loss = loss,
      split = isTRUE(split), alpha = alpha, bonf.correct = isTRUE(bonf.correct),
      backend = backend, group = isTRUE(group),
      cross.fit = isTRUE(cross.fit),
      num.folds = if (isTRUE(cross.fit)) as.integer(num.folds) else NA_integer_
    ),
    class = "loco_vimp"
  )
}

## Detect supported backend or stop with a clear message.
detect_backend <- function(model) {
  ## Order matters: boosted_regression_forest does NOT inherit "grf".
  if (inherits(model, "ranger")) return("ranger")
  if (inherits(model, "boosted_regression_forest")) return("grf_brf")
  if (inherits(model, "causal_forest") ||
      inherits(model, "causal_survival_forest")) {
    stop("loco() does not support causal forests; use cf_loco() instead.",
         call. = FALSE)
  }
  rejected <- c("survival_forest", "quantile_forest",
                "instrumental_forest", "multi_arm_causal_forest",
                "lm_forest")
  for (cls in rejected) {
    if (inherits(model, cls)) {
      stop("loco() does not support grf::", cls, "().",
           call. = FALSE)
    }
  }
  if (inherits(model, "regression_forest")) return("grf_reg")
  if (inherits(model, "probability_forest")) return("grf_prob")
  stop("loco(): unsupported `model` class (", paste(class(model), collapse = "/"),
       "). Expected a ranger or grf outcome forest.",
       call. = FALSE)
}

## Map (model, backend) to a ranger-style treetype string.
treetype_for_backend <- function(model, backend) {
  switch(backend,
         ranger   = model$treetype,
         grf_reg  = "Regression",
         grf_brf  = "Regression",
         grf_prob = "Probability estimation",
         stop("internal: unknown backend ", backend, call. = FALSE))
}

## Predictor names for a fitted forest.
pred_names_for <- function(model, backend) {
  if (backend == "ranger") {
    return(model$forest$independent.variable.names)
  }
  X <- X_orig_for(model, backend)
  nm <- colnames(X)
  if (is.null(nm) || any(!nzchar(nm))) {
    nm <- paste0("V", seq_len(ncol(X)))
  }
  nm
}

X_orig_for <- function(model, backend) {
  if (backend == "grf_brf") return(model$forests[[1L]]$X.orig)
  model$X.orig
}

Y_orig_for <- function(model, backend) {
  if (backend == "grf_brf") return(model$forests[[1L]]$Y.orig)
  model$Y.orig
}

## Pull replay-able hyperparameters off a fitted grf forest.
extract_grf_hp <- function(model, backend) {
  src <- if (backend == "grf_brf") model$forests[[1L]] else model
  tp  <- src$tunable.params
  out <- list()
  ## scalars stored at top level
  nt <- src[["_num_trees"]]
  if (!is.null(nt)) out$num.trees <- as.integer(nt)
  if (!is.null(src$ci.group.size)) out$ci.group.size <- src$ci.group.size
  if (!is.null(src$equalize.cluster.weights))
    out$equalize.cluster.weights <- isTRUE(src$equalize.cluster.weights)
  if (!is.null(src$clusters) && length(src$clusters) > 0L)
    out$clusters <- src$clusters
  ## tunable.params (named list)
  for (k in c("sample.fraction", "mtry", "min.node.size",
              "honesty.fraction", "honesty.prune.leaves",
              "alpha", "imbalance.penalty")) {
    v <- tp[[k]]
    if (!is.null(v)) out[[k]] <- v
  }
  ## boosted: replay boost.steps as length(forests); error.reduction not stored.
  if (backend == "grf_brf") {
    out$boost.steps <- length(model$forests)
  }
  ## never re-tune on refit
  out$tune.parameters <- "none"
  ## suppress threading nondeterminism noise on refit
  out
}

## Refit a grf forest with given hp.args on a subset of X.
## hp.args is filtered to the target fitter's formals to avoid spurious
## "unused argument" errors when API surfaces differ across grf
## forest types (e.g. probability_forest has no tune.parameters /
## boost.steps).
grf_refit <- function(backend, hp.args, X, Y, seed = NULL) {
  fitter <- switch(backend,
                   grf_reg  = grf::regression_forest,
                   grf_brf  = grf::boosted_regression_forest,
                   grf_prob = grf::probability_forest,
                   stop("internal: grf_refit unknown backend ", backend,
                        call. = FALSE))
  allowed <- names(formals(fitter))
  args <- hp.args[intersect(names(hp.args), allowed)]
  args$X <- X
  args$Y <- Y
  if (!is.null(args$mtry)) {
    args$mtry <- min(as.integer(args$mtry), ncol(X))
  }
  if (!is.null(seed) && "seed" %in% allowed) args$seed <- as.integer(seed)
  do.call(fitter, args)
}


## OOB loss for a fitted forest. For ranger we use model$predictions; for
## grf we call predict(model)$predictions (OOB for the training data).
oob_loss <- function(model, train.data, resp.name, pred.names, loss, tt,
                     backend = "ranger") {

  if (backend == "ranger") {
    y     <- train.data[[resp.name]]
    preds <- model$predictions
    this.tt <- model$treetype
  } else {
    ## resp.name / train.data carry the original data, but for OOB
    ## predictions of a grf refit we use the model itself.
    y <- if (backend == "grf_brf") model$forests[[1L]]$Y.orig else model$Y.orig
    preds <- stats::predict(model)$predictions
    this.tt <- if (inherits(model, "probability_forest")) "Probability estimation"
               else "Regression"
  }

  if (this.tt == "Regression") {
    yhat <- as.numeric(preds)
    yv   <- as.numeric(y)
    if (loss == "abs") return(mean(abs(yv - yhat), na.rm = TRUE))
    if (loss == "mse") return(mean((yv - yhat)^2,  na.rm = TRUE))
    stop("internal: unreachable loss in oob_loss/regression: ", loss,
         call. = FALSE)
  }

  if (this.tt == "Probability estimation") {
    P <- preds
    if (is.null(dim(P))) {
      stop("Probability forest returned non-matrix OOB predictions.",
           call. = FALSE)
    }
    K <- ncol(P)
    classes <- colnames(P)
    if (is.null(classes) && backend != "ranger" &&
        inherits(model, "probability_forest")) {
      classes <- model$class.names
    }
    if (!is.factor(y)) y <- factor(y, levels = classes)
    Y_oh <- matrix(0, nrow = nrow(P), ncol = K)
    for (k in seq_len(K)) Y_oh[, k] <- as.integer(y == classes[k])
    if (loss == "brier") {
      ok <- stats::complete.cases(P)
      d  <- (Y_oh - P)
      return(mean(rowSums(d^2)[ok], na.rm = TRUE))
    }
    if (loss == "zero_one") {
      argmax <- max.col(P, ties.method = "first")
      yhat_lab <- classes[argmax]
      return(mean(yhat_lab != as.character(y), na.rm = TRUE))
    }
    if (loss == "log") {
      eps <- 1e-12
      P_clipped <- pmax(P, eps)
      idx <- cbind(seq_len(nrow(P)), match(as.character(y), classes))
      ok  <- stats::complete.cases(P) & !is.na(idx[, 2L])
      return(mean(-log(P_clipped[idx])[ok], na.rm = TRUE))
    }
    stop("internal: unreachable loss in oob_loss/probability: ", loss,
         call. = FALSE)
  }

  if (this.tt == "Classification") {
    yhat <- as.character(preds)
    y_ch <- as.character(y)
    if (loss == "zero_one") return(mean(yhat != y_ch, na.rm = TRUE))
    stop("internal: unreachable loss in oob_loss/classification: ", loss,
         call. = FALSE)
  }

  stop("internal: unreachable treetype in oob_loss: ", this.tt, call. = FALSE)
}


## --- Cross-fit (K-fold) internal helpers ---------------------------------
##
## `.loco_fit_one()` / `.loco_predict_one()` / `.loco_residuals_one()` are
## index-parameterized versions of `loco_custom_split()`'s original
## `fit_one()` / `predict_one()` / `residuals_one()` closures (which closed
## over the single split's `i1`/`i2`). Taking `train_idx`/`test_idx`
## explicitly lets both `loco_custom_split()` (single split) and `.loco_cv()`
## (K-fold cross-fit) share the identical fit/predict/residual arithmetic;
## substituting explicit arguments for closure variables does not change
## floating-point evaluation order, so this refactor is bit-identical to the
## original closures for `loco_custom_split()`.

## Fit the full or a reduced model on `train_idx`, keeping only `keep_cols`.
.loco_fit_one <- function(x, y_train, keep_cols, train_idx, hp.args, backend,
                          seed) {
  if (backend == "ranger") {
    args <- hp.args
    args$x <- x[train_idx, keep_cols, drop = FALSE]
    args$y <- y_train[train_idx]
    if (!is.null(args$mtry)) {
      args$mtry <- min(as.integer(args$mtry), length(keep_cols))
    }
    return(do.call(ranger::ranger, args))
  }
  grf_refit(backend, hp.args,
            X = x[train_idx, keep_cols, drop = FALSE],
            Y = y_train[train_idx],
            seed = seed)
}

## Predict on `test_idx` from a fitted `.loco_fit_one()` model.
.loco_predict_one <- function(fit, x, keep_cols, test_idx, backend) {
  if (backend == "ranger") {
    x_test <- as.data.frame(x[test_idx, keep_cols, drop = FALSE])
    return(stats::predict(fit, data = x_test)$predictions)
  }
  ## grf predict returns a list with $predictions
  stats::predict(fit, newdata = x[test_idx, keep_cols, drop = FALSE])$predictions
}

## Per-observation loss residual on `test_idx` for a fitted model. Body is
## copied verbatim from loco_custom_split()'s original residuals_one(),
## substituting the explicit predict()/index arguments for the
## closure-captured predict_one()/y_train[i2].
.loco_residuals_one <- function(fit, x, y_train, keep_cols, test_idx,
                                y_levels, tt, loss, backend) {
  p    <- .loco_predict_one(fit, x, keep_cols, test_idx, backend)
  y_te <- y_train[test_idx]

  if (tt == "Regression") {
    yhat <- as.numeric(p)
    yv   <- as.numeric(y_te)
    if (loss == "abs") return(abs(yv - yhat))
    if (loss == "mse") return((yv - yhat)^2)
  }
  if (tt == "Probability estimation") {
    P <- p
    if (is.null(dim(P))) {
      stop("Probability forest predict() did not return a matrix.",
           call. = FALSE)
    }
    classes <- colnames(P)
    if (is.null(classes)) classes <- y_levels
    y_ch <- as.character(y_te)
    Y_oh <- matrix(0, nrow = nrow(P), ncol = ncol(P))
    for (k in seq_along(classes)) Y_oh[, k] <- as.integer(y_ch == classes[k])
    if (loss == "brier") {
      return(rowSums((Y_oh - P)^2))
    }
    if (loss == "zero_one") {
      argmax <- max.col(P, ties.method = "first")
      yhat_lab <- classes[argmax]
      return(as.numeric(yhat_lab != y_ch))
    }
    if (loss == "log") {
      eps <- 1e-12
      P_clipped <- pmax(P, eps)
      idx <- cbind(seq_len(nrow(P)), match(y_ch, classes))
      return(-log(P_clipped[idx]))
    }
  }
  if (tt == "Classification") {
    yhat <- as.character(p)
    y_ch <- as.character(y_te)
    if (loss == "zero_one") return(as.numeric(yhat != y_ch))
  }
  stop("internal: unreachable (tt=", tt, ", loss=", loss, ")",
       call. = FALSE)
}

## Fold assignment for K-fold cross-fit LOCO. Mirrors `.cf_perm_folds()`'s
## per-level stratified-sampling algorithm exactly
## (`sample(rep_len(seq_len(K), length(idx)))`), gated on `tt` (Probability
## estimation / Classification stratify by class; Regression is plain
## random) rather than a cardinality heuristic, per Jack's locked
## stratification rule.
.loco_assign_folds <- function(y, num.folds, tt) {
  n <- length(y)
  fold <- integer(n)
  if (tt %in% c("Probability estimation", "Classification")) {
    for (lev in levels(as.factor(y))) {
      idx <- which(as.character(y) == lev)
      fold[idx] <- sample(rep_len(seq_len(num.folds), length(idx)))
    }
  } else {
    fold <- sample(rep_len(seq_len(num.folds), n))
  }
  fold
}

## Nadeau-Bengio corrected t-test aggregation core, shared by `.loco_cv()`
## and the refactored `.cf_perm_cv()`. Pure arithmetic only: no CI
## construction, no warnings, no degenerate-covariate zeroing -- callers
## build CIs and emit warnings themselves (their conventions differ).
##
## `Psi` is a `num.folds x G` matrix of per-fold mean statistics; `n1`/`n2`
## are per-fold train/test set sizes (length `num.folds`).
.nb_ttest <- function(Psi, n1, n2) {
  K    <- nrow(Psi)
  imp  <- colMeans(Psi, na.rm = TRUE)
  rho  <- mean(n2) / mean(n1)
  s2   <- apply(Psi, 2L, stats::var, na.rm = TRUE)
  se   <- sqrt((1 / K + rho) * s2)
  t    <- imp / se
  zero.se <- !is.na(se) & se == 0
  t[zero.se] <- NA_real_
  df   <- K - 1L
  pval <- stats::pt(t, df = df, lower.tail = FALSE)
  list(imp = imp, se = se, t = t, pval = pval, df = df, zero.se = zero.se)
}

## K-fold cross-fit split-sample LOCO (`cross.fit = TRUE`). For each fold,
## the full and each reduced model are trained on the other K-1 folds and
## evaluated on the held-out fold, so every observation contributes to the
## importance estimate exactly once (across all folds combined), at the
## cost of `num.folds * (p + 1)` model refits instead of `p + 1`. The K
## per-fold statistics are not independent (models share overlapping
## training folds), so inference uses the Nadeau-Bengio corrected t-test at
## `num.folds - 1` degrees of freedom (mirroring `cf_perm()`'s
## `cross.fit = TRUE` path) rather than the single-split Z-test.
.loco_cv <- function(x, y_train, y_levels, pred.names, targets, target.names,
                     group_mode, hp.args, loss, tt, alpha, bonf.correct, seed,
                     backend, num.folds) {
  n <- nrow(x)
  set.seed(seed)
  fold <- .loco_assign_folds(y_train, num.folds, tt)

  full_keep <- seq_along(pred.names)
  G <- length(targets)
  Psi <- matrix(NA_real_, nrow = num.folds, ncol = G,
                dimnames = list(NULL, target.names))
  n1 <- n2 <- numeric(num.folds)

  for (f in seq_len(num.folds)) {
    tr <- which(fold != f)
    te <- which(fold == f)
    n1[f] <- length(tr)
    n2[f] <- length(te)

    full_fit_f <- .loco_fit_one(x, y_train, full_keep, tr, hp.args, backend,
                                seed + f * 10000L + length(full_keep))
    res_full_f <- .loco_residuals_one(full_fit_f, x, y_train, full_keep, te,
                                      y_levels, tt, loss, backend)

    for (g in seq_len(G)) {
      members  <- as.character(targets[[g]])
      drop_idx <- match(members, pred.names)
      keep_idx <- setdiff(full_keep, drop_idx)

      fit_g_f <- .loco_fit_one(x, y_train, keep_idx, tr, hp.args, backend,
                               seed + f * 10000L + length(keep_idx))
      res_g_f <- .loco_residuals_one(fit_g_f, x, y_train, keep_idx, te,
                                     y_levels, tt, loss, backend)

      d_f_g <- res_g_f - res_full_f
      Psi[f, g] <- mean(d_f_g)
    }
  }

  agg <- .nb_ttest(Psi, n1, n2)
  if (any(agg$zero.se)) {
    warning("loco(): zero cross-fit standard error for some variable(s)/group(s) ",
            "(identical fold importances); their p-values are set to NA. Use more ",
            "folds or a larger sample.", call. = FALSE)
  }

  alpha_eff <- if (bonf.correct) alpha / G else alpha
  half <- stats::qt(1 - alpha_eff / 2, df = agg$df) * agg$se
  ci.lower <- agg$imp - half
  ci.upper <- agg$imp + half
  p.value  <- if (bonf.correct) pmin(1, agg$pval * G) else agg$pval

  .new_loco(
    variable = target.names,
    importance = as.numeric(agg$imp),
    ci.lower = as.numeric(ci.lower),
    ci.upper = as.numeric(ci.upper),
    p.value = as.numeric(p.value),
    members = if (group_mode) lapply(targets, as.character) else NULL,
    n = n, p = length(pred.names),
    method = "z", loss = loss,
    split = TRUE, alpha = alpha, bonf.correct = bonf.correct,
    backend = backend, group = group_mode,
    cross.fit = TRUE, num.folds = as.integer(num.folds)
  )
}


## Custom split-sample LOCO loop used for every ranger split-mode case
## (including per-variable regression with `loss = "abs"`) and for ALL
## grf split-mode cases. Implements sample-splitting + one-sided Z /
## Wilcoxon inference (Lei et al. 2018; Rinaldo et al. 2019),
## generalized to drop SETS of columns and to operate on user-defined
## per-observation loss residuals.
loco_custom_split <- function(x, y_train, y_levels, pred.names,
                              targets, target.names, group_mode,
                              hp.args, loss, tt,
                              method, alpha, bonf.correct, seed,
                              backend = "ranger") {
  n <- nrow(x)
  set.seed(seed)
  i1 <- sample(seq_len(n), floor(n / 2))
  i2 <- setdiff(seq_len(n), i1)

  fit_one <- function(keep_cols) {
    .loco_fit_one(x, y_train, keep_cols, i1, hp.args, backend,
                  seed + length(keep_cols))
  }

  predict_one <- function(fit, keep_cols) {
    .loco_predict_one(fit, x, keep_cols, i2, backend)
  }

  residuals_one <- function(fit, keep_cols) {
    .loco_residuals_one(fit, x, y_train, keep_cols, i2,
                        y_levels, tt, loss, backend)
  }

  full_keep <- seq_along(pred.names)
  full_fit  <- fit_one(full_keep)
  res_full  <- residuals_one(full_fit, full_keep)

  G <- length(targets)
  results <- matrix(NA_real_, nrow = G, ncol = 3,
                    dimnames = list(target.names,
                                    c("P-value", "LowConfPt", "UpConfPt")))

  alpha_eff <- if (bonf.correct) alpha / G else alpha

  for (g in seq_len(G)) {
    members  <- as.character(targets[[g]])
    drop_idx <- match(members, pred.names)
    keep_idx <- setdiff(full_keep, drop_idx)
    fit_g    <- fit_one(keep_idx)
    res_g    <- residuals_one(fit_g, keep_idx)
    d <- res_g - res_full

    if (method == "z") {
      mu  <- mean(d, na.rm = TRUE)
      sdv <- stats::sd(d, na.rm = TRUE)
      n2  <- sum(!is.na(d))
      se  <- if (n2 > 1 && is.finite(sdv) && sdv > 0) sdv / sqrt(n2) else NA_real_
      pval <- if (is.finite(se)) stats::pnorm(mu / se, lower.tail = FALSE) else NA_real_
      pval_corr <- if (bonf.correct) min(1, pval * G) else pval
      half <- if (is.finite(se)) stats::qnorm(1 - alpha_eff / 2) * se else NA_real_
      results[g, ] <- c(pval_corr, mu - half, mu + half)
    } else {
      ok <- !is.na(d)
      d2 <- d[ok]
      if (length(d2) < 2L) {
        results[g, ] <- c(NA_real_, NA_real_, NA_real_)
        next
      }
      pval <- tryCatch(
        suppressWarnings(stats::wilcox.test(
          d2, alternative = "greater", exact = FALSE)$p.value),
        error = function(e) NA_real_)
      pval_corr <- if (bonf.correct) min(1, pval * G) else pval
      ci <- tryCatch(
        suppressWarnings(stats::wilcox.test(
          d2, alternative = "two.sided",
          conf.int = TRUE, conf.level = 1 - alpha_eff,
          exact = FALSE)),
        error = function(e) NULL)
      if (is.null(ci) || is.null(ci$conf.int)) {
        results[g, ] <- c(pval_corr, NA_real_, NA_real_)
      } else {
        results[g, ] <- c(pval_corr, ci$conf.int[1L], ci$conf.int[2L])
      }
    }
  }

  importance <- (results[, "LowConfPt"] + results[, "UpConfPt"]) / 2

  .new_loco(
    variable = target.names,
    importance = as.numeric(importance),
    ci.lower = as.numeric(results[, "LowConfPt"]),
    ci.upper = as.numeric(results[, "UpConfPt"]),
    p.value = as.numeric(results[, "P-value"]),
    members = if (group_mode) lapply(targets, as.character) else NULL,
    n = n, p = length(pred.names),
    method = method, loss = loss,
    split = TRUE, alpha = alpha, bonf.correct = bonf.correct,
    backend = backend, group = group_mode
  )
}
