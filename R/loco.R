#' LOCO Variable Importance for Outcome Models
#'
#' Computes leave-one-covariate-out (LOCO) variable importance for ranger
#' random forests. Two modes are available: split-sample LOCO with conformal
#' inference (confidence intervals and p-values), or OOB-based LOCO (point
#' estimates only).
#'
#' @param model A fitted `ranger` object (must have been fit with `keep.inbag = TRUE`
#'   if using OOB mode, and the training data must be recoverable from the call).
#' @param alpha Significance level for conformal inference intervals. Default is
#'   `0.1` (90% intervals). Only used when `split = TRUE`.
#' @param split Logical. If `TRUE` (default), uses split-sample LOCO via
#'   [conformalInference::loco()]. If `FALSE`, uses OOB prediction error
#'   differences.
#' @param seed Integer seed for reproducibility. Default is `1995`.
#' @param verbose Logical. Print progress from conformal inference? Default is
#'   `FALSE`. Only used when `split = TRUE`.
#'
#' @return A data frame sorted by descending importance with columns:
#'   \describe{
#'     \item{variable}{Covariate name.}
#'     \item{importance}{LOCO importance score (midpoint of CI when `split = TRUE`,
#'       increase in OOB error when `split = FALSE`).}
#'     \item{ci.lower}{Lower confidence bound (only when `split = TRUE`).}
#'     \item{ci.upper}{Upper confidence bound (only when `split = TRUE`).}
#'     \item{p.value}{P-value from conformal Z-test (only when `split = TRUE`).}
#'   }
#'
#' @details
#' The split-sample method requires the `conformalInference` package, which is
#' available only from GitHub:
#' ```
#' devtools::install_github("ryantibs/conformal", subdir = "conformalInference")
#' ```
#'
#' The OOB method refits the original ranger model once per covariate, each time
#' dropping one predictor, and compares OOB prediction error to the full-model
#' baseline.
#'
#' @references
#' Lei, J., G'Sell, M., Rinaldo, A., Tibshirani, R. J., & Wasserman, L. (2018).
#' Distribution-Free Predictive Inference for Regression.
#' *Journal of the American Statistical Association*, 113(523), 1094--1111.
#'
#' @export
loco <- function(model, alpha = 0.1, split = TRUE, seed = 1995,
                 verbose = FALSE) {
  stopifnot(inherits(model, "ranger"))

  ## recover training data
  cl <- model$call
  train.data <- tryCatch(
    eval(cl$data, envir = parent.frame()),
    error = function(e) {
      stop("Cannot recover training data from model call. ",
           "Make sure the data object used to fit the model is available ",
           "in the calling environment.",
           call. = FALSE)
    }
  )

  ## variable names
  pred.names <- model$forest$independent.variable.names
  resp.name  <- model$dependent.variable.name

  ## evaluate mtry once upfront
  orig.mtry <- if (!is.null(cl$mtry)) {
    eval(cl$mtry, envir = parent.frame())
  } else {
    NULL
  }

  ## ── split-sample LOCO with conformal inference ──
  if (split) {
    if (!requireNamespace("conformalInference", quietly = TRUE))
      stop("Install conformalInference: ",
           "devtools::install_github('ryantibs/conformal', ",
           "subdir = 'conformalInference')",
           call. = FALSE)

    x <- as.matrix(train.data[, pred.names, drop = FALSE])
    y <- train.data[[resp.name]]

    ## build ranger wrappers that replay original hyperparameters
    ## strip formula/data; keep only hyperparameters
    ranger.args <- as.list(cl)[-1]
    ranger.args[[1]] <- NULL
    ranger.args$data     <- NULL
    ranger.args$formula  <- NULL
    ranger.args$dependent.variable.name <- NULL
    ranger.args$importance <- "none"

    train.fun <- function(x, y, out = NULL) {
      args <- ranger.args
      args$x <- x
      args$y <- y
      ## cap mtry at ncol(x) — matters for reduced models
      if (!is.null(args$mtry))
        args$mtry <- min(eval(args$mtry), ncol(x))
      do.call(ranger::ranger, args)
    }

    predict.fun <- function(out, newx) {
      predict(out, data = as.data.frame(newx))$predictions
    }

    active.fun <- function(out) {
      list(seq_len(length(out$forest$independent.variable.names)))
    }

    lo <- conformalInference::loco(
      x, y,
      train.fun   = train.fun,
      predict.fun = predict.fun,
      active.fun  = active.fun,
      alpha       = alpha,
      seed        = seed,
      verbose     = verbose
    )

    ## extract results from Z-test intervals (first tuning step)
    inf  <- lo$inf.z[[1]]
    vars <- lo$active[[1]]

    out <- data.frame(
      variable   = pred.names[vars],
      importance = (inf[, "LowConfPt"] + inf[, "UpConfPt"]) / 2,
      ci.lower   = inf[, "LowConfPt"],
      ci.upper   = inf[, "UpConfPt"],
      p.value    = inf[, "P-value"],
      row.names  = NULL
    )
    out <- out[order(-out$importance), ]
    row.names(out) <- NULL
    return(out)
  }

  ## ── OOB-based LOCO (no inference) ──
  baseline.error <- model$prediction.error

  refit.call <- cl
  refit.call$importance <- "none"
  refit.call[[1]] <- quote(ranger::ranger)

  importance <- numeric(length(pred.names))
  names(importance) <- pred.names

  for (i in seq_along(pred.names)) {
    var <- pred.names[i]
    reduced.data <- train.data[, setdiff(names(train.data), var), drop = FALSE]

    refit.call$data <- quote(reduced.data)

    if (!is.null(orig.mtry)) {
      p.reduced <- length(pred.names) - 1L
      refit.call$mtry <- min(orig.mtry, p.reduced)
    }

    reduced.model <- eval(refit.call)
    importance[i] <- reduced.model$prediction.error - baseline.error
  }

  out <- data.frame(
    variable   = pred.names,
    importance = importance,
    row.names  = NULL
  )
  out <- out[order(-out$importance), ]
  row.names(out) <- NULL
  out
}
