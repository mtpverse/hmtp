hmtp_control <- function() {
	list(.learners_trt_folds = 10,
			 .learners_delta_folds = 10,
			 .learners_positive_folds = 10,
			 .trim = 0.99,
			 .return_full_fits = FALSE)
}
