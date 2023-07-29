shift_data <- function(data, trt, cens, shift) {
  if (is.null(shift)) {
    return(shift_cens(data, cens))
  }
  shift_trt(shift_cens(data, cens), trt, shift)
}

shift_cens <- function(data, cens) {
  out <- as.list(data)
  for (ce in cens) {
    out[[ce]] <- 1
  }
  as.data.frame(out, check.names = FALSE)
}

shift_trt <- function(data, trt, .f) {
  for (a in trt) {
    data[[a]] <- .f(data, a)
  }
  data
}

#' Turn All Treatment Nodes On
#'
#' A pre-packaged shift function for use with provided estimators when the exposure is binary.
#' Used to estimate the population intervention effect when the treatment variable i set to 1.
#
#' @param data A A \code{data.frame} containing the treatment variables.
#' @param trt The name of the current treatment variable.
#'
#' @seealso [hmtp_tmle()]
#' @return A \code{data.frame} with all treatment nodes set to 1.
#' @export
#'
#' @examples
static_binary_on <- function(data, trt) {
  rep(1, length(data[[trt]]))
}

#' Turn All Treatment Nodes Off
#'
#' A pre-packaged shift function for use with provided estimators when the exposure is binary.
#' Used to estimate the population intervention effect when the treatment variable is set to 0.
#'
#' @param data A \code{data.frame} containing the treatment variables.
#' @param trt The name of the current treatment variable.

#' @seealso [hmtp_tmle()]
#' @return A A \code{data.frame} with all treatment nodes set to 0.
#' @export
#'
#' @examples
static_binary_off <- function(data, trt) {
  rep(0, length(data[[trt]]))
}
