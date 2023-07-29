create_node_list <- function(trt, baseline = NULL) {
	list(trt = trt_node_list(trt, NULL, baseline, Inf, 1),
  		 outcome = outcome_node_list(trt, NULL, baseline, Inf, 1))
}

trt_node_list <- function(trt, time_vary, baseline = NULL, k, tau) {
  out <- list()
  if (!is.null(baseline)) {
    for (i in 1:tau) {
      out[[i]] <- c(baseline)
    }
  }

  if (length(out) == 0) {
    if (length(trt) == tau) {
      for (i in 1:tau) {
        if (i > 1) {
          out[[i]] <- c(time_vary[[i]], trt[i - 1])
        } else {
          out[[i]] <- c(time_vary[[i]])
        }
      }
    }

    if (length(trt) != tau) {
      for (i in 1:tau) {
        out[[i]] <- c(time_vary[[i]], trt)
      }
    }
  } else {
    if (length(trt) == tau) {
      for (i in 1:tau) {
        if (i > 1) {
          out[[i]] <- c(out[[i]], time_vary[[i]], trt[i - 1])
        } else {
          out[[i]] <- c(out[[i]], time_vary[[i]])
        }
      }
    }

    if (length(trt) != tau) {
      for (i in 1:tau) {
        out[[i]] <- c(out[[i]], time_vary[[i]], trt)
      }
    }
  }

  out <- slide(out, k)

  if (length(trt) != tau) {
    return(out)
  }

  for (i in 1:tau) {
    out[[i]] <- c(out[[i]], trt[[i]])
  }

  out
}

outcome_node_list <- function(trt, time_vary, baseline = NULL, k, tau) {
  out <- list()

  if (length(trt) == tau) {
    for (i in 1:tau) {
      out[[i]] <- c(time_vary[[i]], trt[i])
    }
  }

  if (length(trt) != tau) {
    for (i in 1:tau) {
      out[[i]] <- c(time_vary[[i]], trt)
    }
  }

  out <- slide(out, k)
  if (is.null(baseline)) {
    return(out)
  }

  for (i in 1:tau) {
    out[[i]] <- c(baseline, out[[i]])
  }
  out
}

slide <- function(x, k) {
  if (k == 0) {
    return(x)
  }

  t <- length(x)
  if (k == Inf) {
    k <- t - 1
  }
  lapply(1:t, Lag, x = x, k = k)
}

Lag <- function(x, t, k) {
  if (t == 1) {
    return(x[[1]])
  }
  tk <- max(1, t - k)
  unique(do.call(c, x[tk:t]))
}
