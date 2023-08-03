cf_delta <- function(task, learners, control, pb) {
	out <- list()

	for (fold in seq_along(task$folds)) {
		out[[fold]] <- future::future({
			estimate_delta(get_folded_data(task$natural, task$folds, fold),
										 get_folded_data(task$shifted, task$folds, fold),
										 task$node_list$outcome,
										 task$cens,
										 learners,
										 pb,
										 control)
		},
		seed = TRUE)
	}

	out <- future::value(out)

	list(dn = recombine_outcome(out, "dn", task$folds),
			 ds = recombine_outcome(out, "ds", task$folds),
			 fits = lapply(out, function(x) x[["fits"]]))
}

estimate_delta <- function(natural, shifted, node_list, cens, learners, pb, control) {
	on.exit(pb())
	dn <- ds <- matrix(nrow = nrow(natural$valid), ncol = 1)

	i  <- censored(natural$train, cens)$i
	jt <- censored(natural$train, cens)$j
	jv <- censored(natural$valid, cens)$j

	vars <- node_list[[1]]

	fit <- run_ensemble(natural$train[i, c("hmtp_id", vars, "tmp_hmtp_delta")],
											"tmp_hmtp_delta",
											learners,
											"binomial",
											"hmtp_id",
											control$.metalearner_zero,
											control$.learners_delta_folds)

	if (control$.return_full_fits) {
		fits <- fit
	} else {
		fits <- extract_sl_weights(fit)
	}

	dn[jv, 1] <- bound(predict(fit, natural$valid[jv, c("hmtp_id", vars)]), 1e-05)
	ds[jv, 1] <- bound(predict(fit, shifted$valid[jv, c("hmtp_id", vars)]), 1e-05)

	list(dn = dn,
			 ds = ds,
			 fits = fits)
}
