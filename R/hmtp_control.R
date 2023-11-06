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
	control <- list(.learners_trt_folds = NULL,
									.learners_delta_folds = NULL,
									.learners_positive_folds = NULL,
									.trim = 0.99,
									.return_full_fits = FALSE,
									.B = 1000,
									.boot_seed = NULL)
	if (length(change) == 0) return(control)
	change <- change[names(change) %in% names(control)]
	control[names(change)] <- change
	control
}
