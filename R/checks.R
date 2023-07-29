check_hmtp_data <- function(x, trt, outcome, baseline, cens, id) {
	ci <- censored(x, cens)$j

	if (any(is.na(x[ci, c(trt, baseline), drop = FALSE]))) {
		return("Missing data found in treatment and/or covariate nodes for uncensored observations")
	}
	TRUE
}

assertHmtpData <- checkmate::makeAssertionFunction(check_hmtp_data)

check_reserved_names <- function(x) {
  bad_names <- c("hmtp_id", "tmp_hmtp_stack_indicator", "tmp_hmtp_ystar", "tmp_hmtp_delta") %in% names(x)
  if (!any(bad_names)) {
    return(TRUE)
  }
  "'hmtp_id', 'tmp_hmtp_stack_indicator', 'tmp_hmtp_delta', and 'tmp_hmtp_ystar' are reserved variable names"
}

assertReservedNames <- checkmate::makeAssertionFunction(check_reserved_names)

check_shifted_data <- function(x, natural, doesnt_change, cens, null.ok = TRUE) {
  if (is.null(x)) {
    if (null.ok)
      return(TRUE)
    return("Can't be 'NULL'")
  }

  if (!(identical(natural[doesnt_change], x[doesnt_change]))) {
    return("The only columns that can be different between `data` and `shifted` are those indicated in `trt` and `cens`")
  }

  if (is.null(cens)) {
    return(TRUE)
  }

  if (!all(x[cens] == 1)) {
    return("Censoring variables should be 1 in 'shifted'")
  }

  TRUE
}

assertShiftedData <- checkmate::makeAssertionFunction(check_shifted_data)

check_not_data_table <- function(x) {
  is_data_frame <- checkmate::checkDataFrame(x)
  if (!isTRUE(is_data_frame)) {
    return(is_data_frame)
  }

  is_data_table <- data.table::is.data.table(x)
  if (is_data_table) {
    return("Must be a 'data.frame', not a 'data.table'")
  }
  TRUE
}

assert_not_data_table <- assertNotDataTable <- checkmate::makeAssertionFunction(check_not_data_table)

check_hmtp_list <- function(x) {
  all_hmtp <- all(unlist(lapply(x, is.hmtp)))
  if (!all_hmtp) {
    return("Objects must be of type 'hmtp'")
  }
  TRUE
}

assertHmtpList <- checkmate::makeAssertionFunction(check_hmtp_list)

check_ref_class <- function(x) {
  if (!is.hmtp(x)) {
    is_num <- checkmate::check_number(x)
    if (!isTRUE(is_num)) {
      return("Must either be a single numeric value or another hmtp object")
    }
  }
  TRUE
}

assertRefClass <- checkmate::makeAssertionFunction(check_ref_class)
