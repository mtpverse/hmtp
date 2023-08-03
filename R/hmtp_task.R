#' @importFrom R6 R6Class
hmtp_task <- R6::R6Class(
  "hmtp_task",
  public = list(
    natural = NULL,
    shifted = NULL,
    trt = NULL,
    cens = NULL,
    node_list = NULL,
    n = NULL,
    id = NULL,
    bounds = NULL,
    folds = NULL,
    weights = NULL,
    log = NULL,
    initialize = function(data, trt, outcome, baseline, cens, shift, shifted,
    											id, folds, weights = NULL, log, upper_bound = NULL) {
      self$n <- nrow(data)
      self$trt <- trt
      self$cens <- cens
      self$node_list <- create_node_list(trt, baseline)
      self$bounds <- y_bounds(data[[outcome]], upper_bound)
      data$hmtp_id <- create_ids(data, id)
      self$id <- data$hmtp_id
      self$folds <- folds
      self$log <- log

      shifted <- {
        if (is.null(shifted) && !is.null(shift))
          shift_data(data, trt, cens, shift)
        else if (is.null(shifted) && is.null(shift))
          shift_data(data, trt, cens, shift)
        else {
          tmp <- shifted
          tmp$lmtp_id <- data$lmtp_id
          tmp
        }
      }

      data <- data.table::copy(data)
      shifted <- data.table::copy(shifted)

      data <- fix_censoring_ind(data, cens)
      shifted <- fix_censoring_ind(shifted, cens)

      data$tmp_hmtp_ystar <- scale_y(data[[outcome]], self$bounds)
      shifted$tmp_hmtp_ystar <- data$tmp_hmtp_ystar

      data$tmp_hmtp_delta <- as.numeric(data[[outcome]] > 0)
      shifted$tmp_hmtp_delta <- data$tmp_hmtp_delta

      self$natural <- data
      self$shifted <- shifted

      if (!is.null(weights)) {
        self$weights <- weights
      }
    }
  )
)
