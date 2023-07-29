cf_tmle <- function(task, ratios, delta, positive, control) {
  out <- list()

  ratios <- matrix(t(apply(ratios, 1, cumprod)),
                   nrow = nrow(ratios),
                   ncol = ncol(ratios))

  for (fold in seq_along(task$folds)) {
    out[[fold]] <- future::future({
      estimate_tmle(
        get_folded_data(task$natural, task$folds, fold),
        get_folded_data(ratios, task$folds, fold)$train,
        lapply(delta[c("dn", "ds")],
        			 function(x) get_folded_data(x, task$folds, fold)),
        lapply(positive[c("mn", "ms")],
        			 function(x) get_folded_data(x, task$folds, fold)),
        task$weights[task$folds[[fold]]$training_set],
        task$cens,
        task$bounds
      )
    },
    seed = TRUE)
  }

  out <- future::value(out)

  list(ms = recombine_outcome(out, "ms", task$folds),
  		 qs = recombine_outcome(out, "qs", task$folds),
  		 mn = recombine_outcome(out, "mn", task$folds),
  		 qn = recombine_outcome(out, "qn", task$folds))
}

estimate_tmle <- function(natural, ratios, delta, positive, weights, cens, bounds) {
	i  <- censored(natural$train, cens)$i
	jt <- censored(natural$train, cens)$j
	jv <- censored(natural$valid, cens)$j
	d <- natural$train$tmp_hmtp_delta == 1

	msv_eps <- dsv_eps <- dnv_eps <- mnv_eps <- matrix(nrow = nrow(natural$valid), ncol = 1)

	wts <- {
		if (is.null(weights))
			ratios[i, 1]
		else
			ratios[i, 1] * weights[i]
	}

	fit1 <- sw(
		glm(
			natural$train[i & d, ]$tmp_hmtp_ystar ~ offset(qlogis(positive$mn$train[i & d, 1])),
			weights = wts[d],
			family = "binomial"
		)
	)

	mnt_eps <- rescale_y(plogis(qlogis(positive$mn$train[jt, 1]) + coef(fit1)), bounds)
	msv_eps[, 1] <- rescale_y(plogis(qlogis(positive$ms$valid[jv, 1]) + coef(fit1)), bounds)
	mnv_eps[, 1] <- rescale_y(plogis(qlogis(positive$mn$valid[jv, 1]) + coef(fit1)), bounds)

	fit2 <- sw(
		glm(natural$train[i, ]$tmp_hmtp_delta ~ offset(qlogis(delta$dn$train[i, 1])),
				weights = (wts*mnt_eps),
				family = "binomial")
	)

	dsv_eps[, 1] <- plogis(qlogis(delta$ds$valid[jv, 1]) + coef(fit2))
	dnv_eps[, 1] <- plogis(qlogis(delta$dn$valid[jv, 1]) + coef(fit2))

	list(qs = dsv_eps,
			 qn = dnv_eps,
			 ms = msv_eps,
			 mn = mnv_eps)
}
