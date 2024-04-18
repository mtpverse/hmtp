cf_r <- function(task, learners, mtp, control, pb) {
	out <- vector("list", length(task$folds))

	if (is.null(task$delta)) {
		for (fold in seq_along(task$folds)) {
			out[[fold]] <- future::future({
				estimate_r(
					get_folded_data(task$natural, task$folds, fold),
					get_folded_data(task$shifted, task$folds, fold),
					task$trt,
					task$cens,
					task$node_list$trt,
					learners,
					pb,
					mtp,
					control
				)
			},
			seed = TRUE)
		}
	} else {
		for (fold in seq_along(task$folds)) {
			out[[fold]] <- future::future({
				estimate_r_ipsi(
					get_folded_data(task$natural, task$folds, fold),
					get_folded_data(task$natural, task$folds, fold),
					task$delta,
					task$trt,
					task$cens,
					task$node_list$trt,
					learners,
					pb,
					control
				)
			},
			seed = TRUE)
		}
	}

	trim_ratios(recombine_ratios(future::value(out), task$folds, !is.null(task$delta)), control$.trim)
}

estimate_r <- function(natural, shifted, trt, cens, node_list, learners, pb, mtp, control) {
	on.exit(pb())
	densratios <- matrix(nrow = nrow(natural$valid), ncol = 1)

	jt <- rep(censored(natural$train, cens)$j, 2)
	iv <- censored(natural$valid, cens)$i
	jv <- censored(natural$valid, cens)$j
	fv <- followed_rule(natural$valid[[trt]], shifted$valid[[trt]], mtp)

	vars <- c(node_list[[1]], cens)
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

estimate_r_ipsi <- function(natural, shifted, delta, trt, cens, node_list, learners, pb, control) {
	on.exit(pb())
	d <- matrix(-999L, nrow = nrow(natural$valid), ncol = 1)
	densratios <- matrix(nrow = nrow(natural$valid), ncol = 1)

	jt <- censored(natural$train, cens)$j
	iv <- censored(natural$valid, cens)$i
	jv <- censored(natural$valid, cens)$j

	vars <- setdiff(node_list[[1]], trt)

	fit <- run_ensemble(natural$train[jt, c("hmtp_id", vars, trt)],
											trt,
											learners,
											"binomial",
											"hmtp_id",
											control$.learners_trt_folds)

	if (control$.return_full_fits) {
		fits <- fit
	} else {
		fits <- extract_sl_weights(fit)
	}

	eps <- runif(nrow(natural$valid))
	pi <- pi_d <- matrix(-999L, nrow = nrow(natural$valid), ncol = 1)
	pi[jv, ] <- bound(predict(fit, natural$valid[jv, c("hmtp_id", vars)]), .Machine$double.eps)
	pi_d[jv, ] <- g_d_ipsi(natural$train[[trt]][jv], pi[jv, ], delta)
	shifted$valid[jv, trt] <- d_ipsi(natural$train[[trt]][jv], eps[jv], delta)

	if (!is.null(cens)) {
		fit_cens <- run_ensemble(natural$train[jt, c("hmtp_id", vars, trt, cens)],
														 cens,
														 learners,
														 "binomial",
														 "hmtp_id",
														 control$.learners_trt_folds)

		pred_cens <- matrix(-999L, nrow = nrow(shifted$valid), ncol = 1)
		# Should the prediction for censoring be under the interventon on A???
		pred_cens[jv, ] <- bound(predict(fit_cens, shifted$valid[jv, c("hmtp_id", vars, trt)]), .Machine$double.eps)

		if (control$.return_full_fits) {
			fits_cens <- fit_cens
		} else {
			fits_cens <- extract_sl_weights(fit_cens)
		}
	} else {
		fits_cens <- NULL
		pred_cens <- 1
	}

	densratios[, 1] <- (pi_d / pi)*(1 / pred_cens)*iv
	list(shifted = shifted$valid, ratios = densratios, fits = fits, fits_cens = fits_cens)
}

g_d_ipsi <- function(trt, g, delta) {
	direction <- sign(delta)
	delta <- 1 - abs(delta)
	if (direction == -1) {
		return(trt*delta*g + (1 - trt)*(1 - delta*g))
	}
	trt*(1 - delta*g) + (1 - trt)*delta*g
}

d_ipsi <- function(trt, eps, delta) {
	direction <- sign(delta)
	delta <- 1 - abs(delta)
	if (direction == -1) {
		return(ifelse(eps < delta, trt, 0))
	}
	ifelse(eps < delta, trt, 1)
}
