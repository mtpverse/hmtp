#' HMTP Targeted Maximum Likelihood Estimator
#'
#' @param data \[\code{data.frame}\]\cr
#'  A \code{data.frame} in wide format containing all necessary variables
#'  for the estimation problem. Must not be a \code{data.table}.
#' @param trt \[\code{character}\]\cr
#'  The column name of the treatment variable.
#' @param outcome \[\code{character}\]\cr
#'  The column name of the outcome variable.
#' @param baseline \[\code{character}\]\cr
#'  An optional vector containing the column names of baseline covariates to be
#'  included for adjustment.
#' @param cens \[\code{character}\]\cr
#'  An optional vector for the column name of a censoring indicator. If missingness in the outcome is
#'  present this must be provided.
#' @param shift \[\code{closure}\]\cr
#'  A two argument function that specifies how the treatment variable should be shifted.
#'  See examples for how to specify shift functions for continuous, binary, and categorical exposures.
#' @param shifted \[\code{data.frame}\]\cr
#'  An optional data frame, the same as in \code{data}, but modified according
#'  to the treatment policy of interest. If specified, \code{shift} is ignored.
#' @param density_ratios
#' @param id \[\code{character(1)}\]\cr
#'  An optional column name containing cluster level identifiers.
#' @param upper_bound \[\code{numeric(1)}\]\cr
#' @param learners_zero \[\code{character}\]\cr
#' @param learners_positive \[\code{character}\]\cr
#' @param folds \[\code{integer(1)}\]\cr
#' @param weights \[\code{numeric(nrow(data))}\]\cr
#'  An optional vector containing sampling weights.
#' @param log
#' @param control
#' @param ... Extra arguments. Exists for backwards compatibility.
#'
#' @details
#' ## Should \code{mtp = TRUE}?
#' A modified treatment policy (MTP) is an intervention that depends on the natural value of the exposure (the value that the treatment would have taken under no intervention). This differs from other causal effects, such as the average treatment effect (ATE), where an exposure would be increased (or decreased) deterministically. \bold{If your intervention of interest adds, subtracts, or multiplies the observed treatment values by some amount, use \code{mtp = TRUE}}.
#'
#' @return A list of class \code{hmtp} containing the following components:
#'
#' @example
#' @export
hmtp_tmle <- function(data, trt, outcome, baseline = NULL,
                      cens = NULL, shift = NULL, shifted = NULL,
                      density_ratios, id = NULL, upper_bound = NULL,
											learners_zero = c("mean", "glm"),
                      learners_positive = c("mean", "glm"),
                      folds = 10, weights = NULL, log = TRUE,
											control = hmtp_control(), ...) {
  assertNotDataTable(data)
	checkmate::assertNumeric(data[[outcome]], lower = 0)
  checkmate::assertCharacter(baseline, null.ok = TRUE)
  checkmate::assertCharacter(trt, len = 1)
  checkmate::assertCharacter(cens, len = 1, null.ok = !checkmate::anyMissing(data[, outcome, drop = FALSE]))
  checkmate::assertCharacter(id, len = 1, null.ok = TRUE)
  checkmate::assertSubset(c(trt, outcome, baseline, cens, id), names(data))
  assertHmtpData(data, trt, outcome, baseline, cens, id)
  assertReservedNames(data)
  checkmate::assertFunction(shift, nargs = 2, null.ok = TRUE)
  assertShiftedData(shifted, data, c(outcome, baseline, id), cens)
  checkmate::assertNumeric(upper_bound, len = 1, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  checkmate::assertNumeric(weights, len = nrow(data), finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  checkmate::assertList(folds)

  task <- hmtp_task$new(data = data,
  											trt = trt,
  											outcome = outcome,
  											baseline = baseline,
  											cens = cens,
  											shift = shift,
  											shifted = shifted,
  											id = id,
  											folds = folds,
  											weights = weights,
  											log = log,
  											upper_bound = upper_bound)

  pb <- progressr::progressor(length(folds)*2)

  d <- cf_delta(task, learners_zero, control, pb)
  m <- cf_m(task, learners_positive, control, pb)
  eps <- cf_tmle(task, density_ratios, d, m)

  theta(y = data[[outcome]],
  			r = density_ratios,
  			q = list(natural = eps$psi$qn, shifted = eps$psi$qs),
  			m = list(natural = eps$psi$mn, shifted = eps$psi$ms),
  			boots = eps$booted,
  			id = task$natural$hmtp_id,
  			weights = task$weights,
  			shift = if (is.null(shifted)) deparse(substitute((shift))) else NULL,
  			fits_q = d$fits,
  			fits_m = m$fits)
}
