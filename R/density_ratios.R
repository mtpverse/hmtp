cf_r <- function(task, pred_m, pred_q, learners, mtp, control, pb) {
  out <- list()

  for (fold in seq_along(task$folds)) {
    out[[fold]] <- future::future({
      estimate_r(get_folded_data(cbind("tmp_hmtp_pred_mq" = pred_m*pred_q, task$natural),
      													 task$folds, fold),
      					 get_folded_data(cbind("tmp_hmtp_pred_mq" = pred_m*pred_q, task$shifted),
      					 								task$folds, fold),
      					 task$trt,
      					 task$cens,
      					 learners,
      					 pb,
      					 mtp,
      					 control)
    },
    seed = TRUE)
  }

  trim_ratios(recombine_ratios(future::value(out), task$folds), control$.trim)
}

estimate_r <- function(natural, shifted, trt, cens, learners, pb, mtp, control) {
	on.exit(pb())
  densratios <- matrix(nrow = nrow(natural$valid), ncol = 1)

  jt <- rep(censored(natural$train, cens)$j, 2)
  iv <- censored(natural$valid, cens)$i
  jv <- censored(natural$valid, cens)$j
  fv <- followed_rule(natural$valid[[trt]], shifted$valid[[trt]], mtp)

	vars <- c("tmp_hmtp_pred_mq", trt, cens)
  stacked <- stack_data(natural$train, shifted$train)

  fit <- run_ensemble(stacked[jt, c("hmtp_id", vars, "tmp_hmtp_stack_indicator")],
  										"tmp_hmtp_stack_indicator",
  										learners,
  										"binomial",
  										"hmtp_id",
  										control$.learners_trt_folds)

  if (control$.return_full_fits) {
  	fits <- fit
  } else {
  	fits <- extract_sl_weights(fit)
  }

  pred <- matrix(-999L, nrow = nrow(natural$valid), ncol = 1)
  pred[jv, ] <- bound(predict(fit, natural$valid[jv, c("hmtp_id", vars)]), .Machine$double.eps)

  densratios[, 1] <- density_ratios(pred, iv, fv, mtp)
  list(ratios = densratios, fits = fits)
}

stack_data <- function(natural, shifted) {
	out <- rbind(natural, shifted)
  out[["tmp_hmtp_stack_indicator"]] <- rep(c(0, 1), each = nrow(natural))
  out
}

density_ratios <- function(pred, cens, followed, mtp) {
  pred <- ifelse(followed & isFALSE(mtp), pmax(pred, 0.5), pred)
  (pred * cens * followed) / (1 - pmin(pred, 0.999))
}
