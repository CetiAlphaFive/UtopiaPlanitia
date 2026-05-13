#' LOCO Variable Importance for Outcome Models
#'
#' Computes leave-one-covariate-out (LOCO) variable importance for
#' `ranger` random-forest regression, probability, and classification
#' models. Two modes are available:
#'
#' * **Split-sample** ([conformalInference::loco()] for per-variable
#'   LOCO on regression forests with absolute-deviation loss; a custom
#'   split loop in all other cases): valid asymptotic confidence
#'   intervals and p-values from either a normal-theory Z-test or a
#'   Wilcoxon signed-rank test on loss-residual differences. Slower
#'   (data is split in half).
#' * **OOB**: refits the original ranger model once per variable (or
#'   per group), dropping the relevant predictors each time, and
#'   compares OOB prediction error to the full-model baseline. Faster,
#'   point estimates only. **No formal inference.** Naive Wald-style
#'   tests on per-observation OOB error differences are
#'   anti-conservative because OOB residuals are dependent across
#'   trees; we therefore do not provide them. For valid p-values use
#'   `split = TRUE`.
#'
#' @param model A fitted [ranger::ranger()] regression, probability, or
#'   classification model. Survival forests are rejected.
#' @param data Optional. A data frame containing the variables used to
#'   fit `model`. If `NULL` (default), `loco()` tries to recover the
#'   training data from `model$call` by evaluating in `parent.frame()`.
#'   Pass `data` explicitly when calling `loco()` from a different scope
#'   than the original fit (e.g. inside another function).
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
#'   `model$treetype`:
#'   * Regression -> `"abs"` (absolute deviation, historic conformal-LOCO
#'     default).
#'   * Probability estimation -> `"brier"` (multi-class Brier score).
#'   * Classification -> `"zero_one"` (misclassification).
#'
#'   Disallowed combinations raise an error. In particular, `"brier"`
#'   and `"log"` require a probability forest (refit with
#'   `probability = TRUE`).
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
#' @param verbose Logical. Print progress from conformal inference?
#'   Default is `FALSE`. Only meaningful when `split = TRUE` with
#'   `groups = NULL` on a regression forest with `loss = "abs"` (the
#'   path that delegates to [conformalInference::loco()]).
#'
#' @return A data frame sorted by descending importance with columns:
#'   \describe{
#'     \item{variable}{Covariate name, or group name when `groups` is
#'       supplied.}
#'     \item{importance}{LOCO importance score. Split mode: mean (or
#'       Hodges-Lehmann pseudo-median for Wilcoxon) of loss-residual
#'       differences `L(reduced) - L(full)`. OOB mode: reduced-model
#'       OOB error minus full-model OOB error.}
#'     \item{ci.lower, ci.upper}{Confidence interval bounds (split
#'       mode only).}
#'     \item{p.value}{**One-sided** p-value testing
#'       H0: importance \eqn{\le} 0 vs H1: importance \eqn{>} 0
#'       (split mode only). Bonferroni-corrected when
#'       `bonf.correct = TRUE`.}
#'     \item{method}{`"z"`, `"wilcox"`, or `"oob"`.}
#'     \item{loss}{Loss function used (`"abs"`, `"mse"`, `"brier"`,
#'       `"zero_one"`, or `"log"`).}
#'     \item{members}{(group mode only) list-column of character
#'       vectors naming the members of each group.}
#'   }
#'
#' @details
#' **One-sided p-values.** The split-mode tests are one-sided against
#' the null that the covariate / group has no incremental predictive
#' value. Small p-values indicate the covariate helps prediction.
#'
#' **Loss / forest-type compatibility.**
#' \describe{
#'   \item{Regression}{`"abs"` (default) or `"mse"`.}
#'   \item{Probability estimation}{`"brier"` (default), `"zero_one"`
#'     (argmax then 0/1), or `"log"` (negative log-likelihood, clipped
#'     at 1e-12).}
#'   \item{Classification}{`"zero_one"` only. To use Brier or log-loss,
#'     refit with `probability = TRUE`.}
#' }
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
#' **Hyperparameter replay.** Core hyperparameters (`num.trees`,
#' `mtry`, `min.node.size`, `splitrule`, `replace`, `max.depth`) are
#' read directly from the fitted model object. Less-common arguments
#' (e.g. `sample.fraction`, `respect.unordered.factors`) are pulled
#' from the original call by evaluating in `parent.frame()`; if any
#' cannot be resolved they are silently dropped with a single warning
#' and ranger's defaults are used in their place.
#'
#' **Edge cases.**
#' * Survival forests are rejected.
#' * Factor predictors are allowed in OOB mode but rejected in split
#'   mode because split-LOCO requires a numeric matrix.
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
#' @seealso [cf_loco()] for LOCO importance tailored to causal forests.
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
                 verbose = FALSE) {

  rlang::check_installed("ranger",
                         reason = "to fit / refit ranger models in `loco()`.")
  stopifnot(inherits(model, "ranger"))
  method <- match.arg(method)
  loss   <- match.arg(loss)
  stopifnot(is.logical(split), length(split) == 1L, !is.na(split))
  stopifnot(is.logical(bonf.correct), length(bonf.correct) == 1L,
            !is.na(bonf.correct))
  stopifnot(is.numeric(alpha), length(alpha) == 1L,
            alpha > 0, alpha < 1)
  stopifnot(is.numeric(seed), length(seed) == 1L, !is.na(seed))

  ## -- treetype + loss compatibility -------------------------------------
  tt <- model$treetype
  if (identical(tt, "Survival")) {
    stop("loco() does not support survival forests.", call. = FALSE)
  }
  if (!tt %in% c("Regression", "Probability estimation", "Classification")) {
    stop("loco(): unrecognized ranger treetype '", tt, "'.", call. = FALSE)
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
         "To use 'brier' or 'log', refit with `probability = TRUE`.",
         call. = FALSE)
  }

  ## -- variable names & sanity check -------------------------------------
  pred.names <- model$forest$independent.variable.names
  resp.name  <- model$dependent.variable.name
  if (length(pred.names) < 2L) {
    stop("loco() requires at least two predictors; the supplied model has ",
         length(pred.names), ".", call. = FALSE)
  }
  if (is.null(resp.name) || !nzchar(resp.name)) {
    stop("Cannot determine the response variable name from `model`. ",
         "This typically happens when the original model was fit with ",
         "`ranger(x = X, y = y)` (no formula / no `dependent.variable.name`). ",
         "Re-fit using the formula interface, or via ",
         "`ranger(x = X, y = y, dependent.variable.name = \"<name>\")` ",
         "and ensure a column of that name is present in `data`.",
         call. = FALSE)
  }

  ## -- recover training data ---------------------------------------------
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

    ## Per-variable, regression, abs loss -> historic conformalInference path.
    use_conformal <- (!group_mode) && tt == "Regression" && loss == "abs"

    if (use_conformal) {
      if (!requireNamespace("conformalInference", quietly = TRUE)) {
        stop("Install conformalInference: ",
             "devtools::install_github('ryantibs/conformal', ",
             "subdir = 'conformalInference')",
             call. = FALSE)
      }
      y_for_ci <- as.numeric(train.data[[resp.name]])
      train.fun <- function(x, y, out = NULL) {
        args <- hp.args
        args$x <- x
        args$y <- y
        if (!is.null(args$mtry)) args$mtry <- min(as.integer(args$mtry), ncol(x))
        do.call(ranger::ranger, args)
      }
      predict.fun <- function(out, newx) {
        stats::predict(out, data = as.data.frame(newx))$predictions
      }
      active.fun <- function(out) {
        list(seq_len(length(out$forest$independent.variable.names)))
      }

      lo <- conformalInference::loco(
        x, y_for_ci,
        train.fun    = train.fun,
        predict.fun  = predict.fun,
        active.fun   = active.fun,
        alpha        = alpha,
        bonf.correct = bonf.correct,
        seed         = seed,
        verbose      = verbose
      )

      inf <- switch(method,
                    z      = lo$inf.z[[1L]],
                    wilcox = lo$inf.wilcox[[1L]])
      vars <- lo$active[[1L]]

      importance <- (inf[, "LowConfPt"] + inf[, "UpConfPt"]) / 2

      out <- data.frame(
        variable   = pred.names[vars],
        importance = as.numeric(importance),
        ci.lower   = as.numeric(inf[, "LowConfPt"]),
        ci.upper   = as.numeric(inf[, "UpConfPt"]),
        p.value    = as.numeric(inf[, "P-value"]),
        method     = method,
        loss       = loss,
        row.names  = NULL,
        stringsAsFactors = FALSE
      )
      out <- out[order(-out$importance), , drop = FALSE]
      row.names(out) <- NULL
      return(out)
    }

    ## All other split-mode cases use the custom split loop.
    return(loco_custom_split(
      x = x, y_train = y_train, y_levels = y_levels,
      pred.names = pred.names, targets = targets,
      target.names = target.names, group_mode = group_mode,
      hp.args = hp.args, loss = loss, tt = tt,
      method = method, alpha = alpha, bonf.correct = bonf.correct,
      seed = seed
    ))
  }

  ## -- OOB mode ----------------------------------------------------------
  baseline.error <- oob_loss(model, train.data, resp.name, pred.names,
                             loss = loss, tt = tt)
  x.full <- train.data[, pred.names, drop = FALSE]
  y.full <- train.data[[resp.name]]
  if (tt %in% c("Probability estimation", "Classification") &&
      !is.factor(y.full)) {
    y.full <- as.factor(y.full)
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
    args <- hp.args
    args$x <- x.full[, keep, drop = FALSE]
    args$y <- y.full
    if (!is.null(args$mtry)) {
      args$mtry <- min(as.integer(args$mtry), length(keep))
    }
    reduced.model <- do.call(ranger::ranger, args)
    importance[i] <- oob_loss(reduced.model, train.data, resp.name, keep,
                              loss = loss, tt = tt) - baseline.error
  }

  out <- data.frame(
    variable   = target.names,
    importance = as.numeric(importance),
    method     = "oob",
    loss       = loss,
    row.names  = NULL,
    stringsAsFactors = FALSE
  )
  if (group_mode) {
    out$members <- I(lapply(targets, as.character))
  }
  out <- out[order(-out$importance), , drop = FALSE]
  row.names(out) <- NULL
  out
}


## --- Internal helpers ---------------------------------------------------

## OOB loss for a fitted ranger model, using model$predictions.
oob_loss <- function(model, train.data, resp.name, pred.names, loss, tt) {
  y <- train.data[[resp.name]]
  preds <- model$predictions
  this.tt <- model$treetype

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


## Custom split-sample LOCO loop used for any non-(regression+abs+per-var)
## case in split mode. Mirrors conformalInference::loco()'s sample-splitting
## + one-sided inference logic, generalized to drop SETS of columns and to
## operate on user-defined per-observation loss residuals.
loco_custom_split <- function(x, y_train, y_levels, pred.names,
                              targets, target.names, group_mode,
                              hp.args, loss, tt,
                              method, alpha, bonf.correct, seed) {
  n <- nrow(x)
  set.seed(seed)
  i1 <- sample(seq_len(n), floor(n / 2))
  i2 <- setdiff(seq_len(n), i1)

  fit_one <- function(keep_cols) {
    args <- hp.args
    args$x <- x[i1, keep_cols, drop = FALSE]
    args$y <- y_train[i1]
    if (!is.null(args$mtry)) {
      args$mtry <- min(as.integer(args$mtry), length(keep_cols))
    }
    do.call(ranger::ranger, args)
  }

  residuals_one <- function(fit, keep_cols) {
    x_test <- as.data.frame(x[i2, keep_cols, drop = FALSE])
    pr <- stats::predict(fit, data = x_test)
    p <- pr$predictions
    y_te <- y_train[i2]

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

  out <- data.frame(
    variable   = target.names,
    importance = as.numeric(importance),
    ci.lower   = as.numeric(results[, "LowConfPt"]),
    ci.upper   = as.numeric(results[, "UpConfPt"]),
    p.value    = as.numeric(results[, "P-value"]),
    method     = method,
    loss       = loss,
    row.names  = NULL,
    stringsAsFactors = FALSE
  )
  if (group_mode) {
    out$members <- I(lapply(targets, as.character))
  }
  out <- out[order(-out$importance), , drop = FALSE]
  row.names(out) <- NULL
  out
}
