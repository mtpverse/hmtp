cf_m <- function(task, learners, control, pb) {
	out <- list()

	for (fold in seq_along(task$folds)) {
		out[[fold]] <- future::future({
			estimate_m(get_folded_data(task$natural, task$folds, fold),
								 get_folded_data(task$shifted, task$folds, fold),
								 task$node_list$outcome,
								 task$cens,
								 task$id,
								 learners,
								 task$log,
								 pb,
								 control)
		},
		seed = TRUE)
	}

	out <- future::value(out)

	list(mn = recombine_outcome(out, "mn", task$folds),
			 ms = recombine_outcome(out, "ms", task$folds),
			 fits = lapply(out, function(x) x[["fits"]]))
}

estimate_m <- function(natural, shifted, node_list, cens, id, learners, log, pb, control) {
	on.exit(pb())
	mn <- ms <- matrix(nrow = nrow(natural$valid), ncol = 1)

	i  <- censored(natural$train, cens)$i
	jt <- censored(natural$train, cens)$j
	jv <- censored(natural$valid, cens)$j
	delta <- natural$train$tmp_hmtp_delta == 1

	vars <- node_list[[1]]
	if (log) {
		natural$train[i & delta, "tmp_hmtp_ystar"] <-
			log(natural$train[i & delta, ]$tmp_hmtp_ystar)
	}

	fit <- run_ensemble(natural$train[i & delta, c(id, vars, "tmp_hmtp_ystar")],
											"tmp_hmtp_ystar",
											learners,
											"continuous",
											id,
											control$.learners_positive_folds)

	if (control$.return_full_fits) {
		fits <- fit
	} else {
		fits <- extract_sl_weights(fit)
	}

	retransform <- function(log) {
		if (log) {
			return(function(x) exp(x))
		}
		function(x) x
	}

	.f <- retransform(log)

	mn[jv, 1] <- bound(.f(predict(fit, natural$valid[jv, c(id, vars)])), 1e-05)
	ms[jv, 1] <- bound(.f(predict(fit, shifted$valid[jv, c(id, vars)])), 1e-05)

	list(mn = mn,
			 ms = ms,
			 fits = fits)
}
