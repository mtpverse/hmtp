theta <- function(y, r, q, m, aipw = FALSE, boots, id = NULL, weights, shift, fits_r, fits_q, fits_m) {
	if (is.null(weights)) weights <- rep(1, length(y))

	if (!is.null(boots)) {
		se <- sqrt(var(boots))
	} else {
		inflnce <- eif(y, r, q$natural, q$shifted, m$natural, m$shifted)
		clusters <- split(inflnce*weights, 1:length(y))
		j <- length(clusters)
		se <- sqrt(var(vapply(clusters, function(x) mean(x), 1)) / j)
	}
	
	if (isFALSE(aipw)) {
		theta <- weighted.mean(q$shifted*m$shifted, weights)
	} else {
		theta <- weighted.mean(inflnce, weights)
	}

	ci_low  <- theta - (qnorm(0.975) * se)
	ci_high <- theta + (qnorm(0.975) * se)

	out <- list(
		estimator = ifelse(aipw, "AIPW", "TMLE"),
		theta = theta,
		standard_error = se,
		low = ci_low,
		high = ci_high,
		id = id,
		shift = shift,
		density_ratios = r,
		zero_reg = q$shifted,
		positive_reg = m$shifted,
		boots = boots,
		fits_r = fits_r,
		fits_q = fits_q,
		fits_m = fits_m
	)

	class(out) <- "hmtp"
	out
}

eif <- function(y, r, qn, qs, mn, ms) {
	r * (y - qn*mn) + qs*ms
}
