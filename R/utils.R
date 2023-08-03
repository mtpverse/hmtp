#' Create Folds for Cross-fitting
#'
#' @param data
#' @param id
#' @param V
#'
#' @return
#' @export
#'
#' @examples
setup_cv <- function(data, id, V = 10) {
  out <- origami::make_folds(data, cluster_ids = id, V = V)
  if (V > 1) {
    return(out)
  }
  out[[1]]$training_set <- out[[1]]$validation_set
  out
}

get_folded_data <- function(data, folds, index) {
  out <- list()
  out[["train"]] <- data[folds[[index]]$training_set, , drop = FALSE]
  out[["valid"]] <- data[folds[[index]]$validation_set, , drop = FALSE]
  out
}

fix_censoring_ind <- function(data, cens) {
  if (is.null(cens)) {
    return(data)
  }

  data <- data.table::copy(data)
  for (cen in cens) {
    data.table::set(data, j = cen, value = ifelse(is.na(data[[cen]]), 0, data[[cen]]))
  }
  data
}

bound <- function(x, p = 1e-05) {
  pmax(pmin(x, 1 - p), p)
}

scale_y <- function(y, bounds) {
  if (is.null(bounds)) {
    return(y)
  }
  (y - bounds[1]) / (bounds[2] - bounds[1])
}

y_bounds <- function(y, upper_bound = NULL) {
  if (is.null(upper_bound)) {
    return(c(0, max(y, na.rm = T)))
  }
  c(0, upper_bound)
}

rescale_y <- function(scaled, bounds) {
  (scaled*(bounds[2] - 0)) + 0
}

censored <- function(data, cens) {
  # when no censoring return TRUE for all obs
  if (is.null(cens)) {
    return(list(i = rep(TRUE, nrow(data)), j = rep(TRUE, nrow(data))))
  }

  # other wise find censored observations
  i <- data[[cens]] == 1
  list(i = i, j = rep(TRUE, nrow(data)))
}

followed_rule <- function(obs_trt, shifted_trt, mtp) {
  if (mtp) {
    return(rep(TRUE, length(obs_trt)))
  }

  mapply(function(x, y) isTRUE(all.equal(x, y)), as.list(obs_trt), as.list(shifted_trt))
}

recombine_ratios <- function(x, folds) {
  ind <- Reduce(c, lapply(folds, function(x) x[["validation_set"]]))

  returns <- list()

  returns$ratios <- Reduce(
    rbind,
    lapply(x, function(x) x[["ratios"]])
  )[order(ind), ]

  if (is.null(dim(returns[["ratios"]]))) {
    returns[["ratios"]] <- as.matrix(
      returns[["ratios"]],
      nrow = length(returns[["ratios"]]),
      ncol = 1
    )
  }

  returns$fits <- lapply(x, function(x) x[["fits"]])
  returns
}

trim_ratios <- function(x, trim) {
  x[["ratios"]] <- pmin(x[["ratios"]], quantile(x[["ratios"]], trim))
  x
}

recombine_outcome <- function(x, part, folds) {
  ind <- Reduce(c, lapply(folds, function(x) x[["validation_set"]]))
  Reduce(rbind, lapply(x, function(x) x[[part]]))[order(ind), , drop = FALSE]
}

is.hmtp <- function(x) {
  class(x) == "hmtp"
}

sw <- function(x) {
  suppressWarnings(x)
}

extract_sl_weights <- function(fit) {
	if (inherits(fit, "mlr3superlearner")) {
		return(cbind(Risk = fit$risk))
	}
	fit$coef
}

create_ids <- function(data, id) {
  if (is.null(id)) {
    return(1:nrow(data))
  }
  data[[id]]
}

missing_outcome <- function(x) {
  ifelse(is.na(x), 0, x)
}
