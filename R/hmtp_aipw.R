hmtp_aipw <- function(
  data,
  trt,
  outcome,
  baseline = NULL,
  cens = NULL,
  shift = NULL,
  shifted = NULL,
  mtp = FALSE,
  id = NULL,
  upper_bound = NULL,
  learners_trt = "glm",
  learners_zero = "glm",
  learners_positive = "glm",
  boot = TRUE,
  folds = 10,
  weights = NULL,
  log = TRUE,
  control = hmtp_control(),
  ...
) {
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
checkmate::assertNumber(folds, lower = 1, upper = nrow(data) - 1)

task <- hmtp_task$new(
  data = data,
  trt = trt,
  outcome = outcome,
  baseline = baseline,
  cens = cens,
  shift = shift,
  shifted = shifted,
  id = id,
  V = folds,
  weights = weights,
  log = log,
  upper_bound = upper_bound
)

pb <- progressr::progressor(folds * 3)

r <- cf_r(task, learners_trt, mtp, control, pb)
d <- cf_delta(task, learners_zero, control, pb)
m <- cf_m(task, learners_positive, control, pb)

theta(
  y = data[[outcome]],
  r = r$ratios,
  q = list(natural = d$dn, shifted = d$ds),
  m = list(natural = m$mn, shifted = m$ms),
  aipw = TRUE,
  boots = NULL,
  id = task$natural$hmtp_id,
  weights = task$weights,
  shift = if (is.null(shifted)) deparse(substitute((shift))) else NULL,
  fits_r = r$fits,
  fits_q = d$fits,
  fits_m = m$fits
)
}