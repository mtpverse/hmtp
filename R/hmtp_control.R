#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
hmtp_control <- function(...) {
	change <- list(...)
	control <- list(.learners_trt_folds = 10,
									.learners_delta_folds = 10,
									.learners_positive_folds = 10,
									.trim = 0.99,
									.return_full_fits = FALSE)
	if (length(change) == 0) return(control)

	for (arg in names(change)) {
		control[[arg]] <- change[[arg]]
	}

	control
}
