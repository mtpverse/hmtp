cf_tmle <- function(task, ratios, delta, positive, control) {
  psi <- estimate_tmle(task$natural,
  										 ratios,
  										 delta[c("dn", "ds")],
  										 positive[c("mn", "ms")],
  										 task$weights,
  										 task$cens,
  										 task$bounds)

  if (!is.null(control$.boot_seed)) set.seed(control$.boot_seed)

  boots <- replicate(control$.B,
  									 sample(1:nrow(task$natural), nrow(task$natural), replace = TRUE),
  									 simplify = FALSE)

  Qnb <- lapply(boots, function(i) {
  	estimate_tmle(task$natural[i, ],
  								ratios[i, , drop = FALSE],
  								lapply(delta[c("dn", "ds")], function(x) x[i, , drop = FALSE]),
  								lapply(positive[c("mn", "ms")], function(x) x[i, , drop = FALSE]),
  								task$weights[i, , drop = FALSE],
  								task$cens,
  								task$bounds)
  })

  booted <- sapply(Qnb, function(x) {
  	if (is.null(task$weights))
  		mean(x$qs*x$ms)
  	else
  		weighted.mean(x$qs*x$ms, weights)
  })

  list(psi = psi, booted = booted)
}

estimate_tmle <- function(natural, ratios, delta, positive, weights, cens, bounds) {
	i  <- censored(natural, cens)$i
	j <- censored(natural, cens)$j
	d <- natural$tmp_hmtp_delta == 1

	ms_eps <- ds_eps <- dn_eps <- mn_eps <- matrix(nrow = nrow(natural), ncol = 1)

	wts <- {
		if (is.null(weights))
			ratios[i, 1]
		else
			ratios[i, 1] * weights[i]
	}

	fit1 <- sw(
		glm(
			natural[i & d, ]$tmp_hmtp_ystar ~ offset(qlogis(positive$mn[i & d, 1])),
			family = "binomial",
			weights = wts[d]
		)
	)

	eps1 <- coef(fit1)

	ms_eps[, 1] <- rescale_y(plogis(qlogis(positive$ms[j, 1]) + eps1[1]), bounds)
	mn_eps[, 1] <- rescale_y(plogis(qlogis(positive$mn[j, 1]) + eps1[1]), bounds)

	wts2 <- wts*mn_eps[, 1]

	fit2 <- sw(
		glm(natural[i, ]$tmp_hmtp_delta ~ offset(qlogis(delta$dn[i, 1])),
				family = "binomial",
				weights = wts2)
	)

	eps2 <- coef(fit2)

	ds_eps[, 1] <- plogis(qlogis(delta$ds[j, 1]) + eps2[1])
	dn_eps[, 1] <- plogis(qlogis(delta$dn[j, 1]) + eps2[1])

	list(qs = ds_eps,
			 qn = dn_eps,
			 ms = ms_eps,
			 mn = mn_eps)
}
