#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a(n) hmtp object
#'
#' @param x A `hmtp` object produced by a call to [hmtp::hmtp_tmle()].
#' @param ... Unused, included for generic consistency only.
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
tidy.hmtp <- function(x, ...) {
  out <- data.frame(estimator = x$estimator,
                    estimate = x$theta,
                    std.error = x$standard_error,
                    conf.low = x$low,
                    conf.high = x$high)
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}
