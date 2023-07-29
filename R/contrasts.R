#' Perform Contrasts of HMTP Fits
#'
#' Estimates contrasts of multiple HMTP fits compared to either a known reference value
#' or a reference HMTP fit.
#'
#' @param ... One or more objects of class 'hmtp'.
#' @param ref A reference value or another 'hmtp' fit to compare all other fits against.
#'
#' @return A list of class \code{hmtp_contrast} containing the following components:
#'
#' \item{type}{The type of contrast performed.}
#' \item{null}{The null hypothesis.}
#' \item{vals}{A dataframe containing the contrasts estimates, standard errors, and confidence intervals.}
#' \item{eifs}{Un-centered estimated influence functions for contrasts estimated.}
#' @export
#'
#' @example
hmtp_contrast <- function(..., ref) {
  fits <- list(...)
  assertHmtpList(fits)
  assertRefClass(ref)
  contrast_additive(fits = fits, ref = ref)
}

contrast_additive <- function(fits, ref) {
  res <- lapply(fits, function(x) contrast_additive_single(x, ref))
  vals <- Reduce(rbind, lapply(res, function(x) x[["vals"]]))
  eifs <- Reduce(cbind, lapply(res, function(x) x[["eif"]]))

  out <- list(
    type = "additive",
    null = 0,
    vals = vals,
    eifs = eifs
  )
  class(out) <- "hmtp_contrast"
  return(out)
}

contrast_additive_single <- function(fit, ref) {
  if (is.hmtp(ref)) {
    theta <- fit$theta - ref$theta
    eif <- fit$eif - ref$eif
  }

  if (isFALSE(is.hmtp(ref))) {
    theta <- fit$theta - ref
    eif <- fit$eif
  }

  if (is.null(fit$id)) {
    fit$id <- 1:length(eif)
  }

  clusters <- split(eif, fit$id)
  j <- length(clusters)
  std.error <- sqrt(var(vapply(clusters, function(x) mean(x), 1)) / j)
  conf.low <- theta - qnorm(0.975) * std.error
  conf.high <- theta + qnorm(0.975) * std.error
  p.value <- pnorm(abs(theta) / std.error, lower.tail = FALSE) * 2

  list(
    vals = data.frame(
      theta = theta,
      shift = fit$theta,
      ref = ifelse(is.hmtp(ref), ref$theta, ref),
      std.error = std.error,
      conf.low = conf.low,
      conf.high = conf.high,
      p.value = p.value
    ),
    eif = eif
  )
}
