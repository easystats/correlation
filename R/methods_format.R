# Correlation table ---------------------------------------------------------

#' @export
format.easycorrelation <- function(x,
                                   digits = NULL,
                                   p_digits = NULL,
                                   stars = NULL,
                                   format = NULL,
                                   ...) {
  if (nrow(x) == 0) {
    warning("The table is empty, no rows left to print.", call. = FALSE)
    return(as.data.frame(x))
  }

  attri <- attributes(x)

  out <- insight::format_table(x,
    digits = .retrieve_arg_from_attr(attri, digits, default = 2),
    stars = .retrieve_arg_from_attr(attri, stars, default = TRUE),
    p_digits = .retrieve_arg_from_attr(attri, p_digits, default = "apa"),
    ...
  )

  out$Method <- NULL
  out$n_Obs <- NULL

  attr(out, "table_footer") <- .format_easycorrelation_footer(x, format = format)
  attr(out, "table_caption") <- .format_easycorrelation_caption(x, format = format)
  out
}


# Correlation matrix -----------------------------------------------------------


#' @export
format.easycormatrix <- function(x,
                                 digits = NULL,
                                 p_digits = NULL,
                                 stars = NULL,
                                 include_significance = NULL,
                                 format = NULL,
                                 bf_exact = TRUE,
                                 ...) {
  # If it's a real matrix
  if (!"Parameter" %in% colnames(x)) {
    m <- as.data.frame(x)
    return(cbind(data.frame(Variables = row.names(x)), m))
  }

  # Find attributes
  attri <- attributes(x)

  # Retrieve arguments from attributes (or assign default)
  digits <- .retrieve_arg_from_attr(attri, digits, default = 2)
  stars <- .retrieve_arg_from_attr(attri, stars, default = TRUE)
  include_significance <- .retrieve_arg_from_attr(attri, include_significance, default = FALSE)
  p_digits <- .retrieve_arg_from_attr(attri, p_digits, default = "apa")


  # Round and format values
  nums <- sapply(as.data.frame(x), is.numeric)
  x[, nums] <- sapply(as.data.frame(x)[, nums], insight::format_value, digits = digits)


  # Deduct if stars only
  stars_only <- FALSE
  if (!include_significance && stars) {
    stars_only <- TRUE
  }


  # Significance
  type <- names(attri)[names(attri) %in% c("BF", "pd", "p")][1]
  sig <- attri[[type]]

  if (!is.null(sig)) {
    if (type == "p") {
      sig[, nums] <- sapply(
        sig[, nums],
        insight::format_p,
        stars = stars,
        digits = p_digits,
        stars_only = stars_only
      )
    }

    if (type == "pd") {
      sig[, nums] <- sapply(
        sig[, nums],
        insight::format_pd,
        stars = stars,
        stars_only = stars_only
      )
    }

    if (type == "BF") {
      sig[, nums] <- sapply(
        sig[, nums],
        insight::format_bf,
        exact = bf_exact,
        stars = stars,
        stars_only = stars_only
      )
    }

    if (!stars_only) {
      sig[, nums] <- sapply(sig[, nums], function(x) ifelse(x != "", paste0(" (", x, ")"), "")) # nolint
    }

    if (include_significance || stars) {
      x[, nums] <- paste0(as.matrix(as.data.frame(x)[, nums]), as.matrix(sig[, nums]))
    }
  }

  # Prepare output
  out <- as.data.frame(x)

  # remove redundant diagonal
  if (isTRUE(attri$redundant)) {
    for (i in colnames(out)) {
      out[[i]][out$Parameter == i] <- ""
    }
  }

  attr(out, "table_footer") <- .format_easycorrelation_footer(x, format = format)
  attr(out, "table_caption") <- .format_easycorrelation_caption(x, format = format)
  out
}


# Footers and Captions ----------------------------------------------------

#' @keywords internal
.format_easycorrelation_footer <- function(x, format = NULL) {
  footer <- ""

  # P-adjust
  if (isFALSE(attributes(x)$bayesian) && !isTRUE(attributes(x)$smoothed)) {
    footer <- paste0(
      "\np-value adjustment method: ",
      parameters::format_p_adjust(attributes(x)$p_adjust)
    )
  }

  # N-obs
  if (!is.null(x$n_Obs)) {
    if (length(unique(x$n_Obs)) == 1) {
      number_obs <- unique(x$n_Obs)
    } else {
      number_obs <- paste0(min(x$n_Obs), "-", max(x$n_Obs))
    }
    footer <- paste0(footer, "\nObservations: ", number_obs)
  }

  # final new line
  footer <- paste0(footer, "\n")

  # for html/markdown, modify footer format
  if (!is.null(format) && format != "text") {
    # no line break if not text format
    footer <- unlist(strsplit(footer, "\n", fixed = TRUE))
    # remove empty elements
    footer <- footer[nzchar(footer, keepNA = TRUE)]
    # create list or separate by ";"
    footer <- switch(format,
      "html" = paste(footer, collapse = "; "),
      as.list(footer)
    )
  }

  footer
}


#' @keywords internal
.format_easycorrelation_caption <- function(x, format = NULL) {
  if (!is.null(attributes(x)$method)) {
    if (isTRUE(attributes(x)$smoothed)) {
      prefix <- "Smoothed Correlation Matrix ("
    } else {
      prefix <- "Correlation Matrix ("
    }
    if (is.null(format) || format == "text") {
      caption <- c(paste0("# ", prefix, unique(attributes(x)$method), "-method)"), "blue")
    } else {
      caption <- paste0(prefix, unique(attributes(x)$method), "-method)")
    }
  } else {
    caption <- NULL
  }

  caption
}

# Arguments retrieving ----------------------------------------------------


#' @keywords internal
.retrieve_arg_from_attr <- function(attri, arg, default) {
  arg_name <- deparse(substitute(arg))

  if (is.null(arg)) {
    if (arg_name %in% names(attri)) {
      arg <- attri[[arg_name]]
    } else if ("additional_arguments" %in% names(attri) && arg_name %in% names(attri$additional_arguments)) {
      arg <- attri$additional_arguments[[arg_name]]
    } else {
      arg <- default # That's the real default
    }
  }

  arg
}
