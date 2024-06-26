#' @export
print.hmtp <- function(x, ...) {
  cat("\n")
  cli::cli_text("{.strong HMTP Estimator}: {x$estimator}")
  cli::cli_text(cat("   "), "{.strong Trt. Policy}: ", cli::col_blue(cli::style_italic("{x$shift}")))
  cat("\n")
  cli::cli_text("{.strong Population intervention estimate}")
  cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$theta, 4)}")
  cli::cli_text(cat("    "), "{.strong Std. error}: {round(x$standard_error, 4)}")
  cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(x$low, 4)}, {round(x$high, 4)})")
  if (x$estimator %in% c("substitution", "IPW")) no_stderr_warning(x$estimator)
  cat("\n")
}

#' @export
print.hmtp_contrast <- function(x, ...) {
  cat("\n")
  cli::cli_text(cat("  "), "{.strong HMTP Contrast}: {x$type}")
  cli::cli_text("{.strong Null hypothesis}: theta == {x$null}")
  cat("\n")
  x$vals$p.value <- format.pval(x$vals$p.value, digits = 3, eps = 0.001)
  print(format(x$vals, digits = 3))
}
