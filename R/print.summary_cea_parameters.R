
#' Print CEA Parameter Summary
#'
#' Displays deterministic, probabilistic and scenario
#' parameter tables in a readable console format.
#'
#' Probabilistic parameters are displayed using a compact
#' distribution specification rather than separate columns
#' for each possible distribution argument.
#'
#' @param x An object of class \code{summary_cea_parameters}.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' For probabilistic parameters, the original distribution
#' arguments are combined into a single specification string,
#' such as Normal(mean = 65, sd = 10) or
#' Gamma(shape = 25, rate = 0.25).
#'
#' This avoids displaying non-applicable arguments as missing
#' values when different distribution families are combined.
#'
#' The underlying summary tables remain unchanged.
#' Deterministic and scenario parameters retain their
#' original presentation.
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.summary_cea_parameters <- function(x, ...) {

  # ============================================================
  # 1. Validate input
  # ============================================================

  if (!inherits(x, "summary_cea_parameters")) {
    stop("'x' must be a 'summary_cea_parameters' object.")
  }

  titles <- c(
    deterministic = "DETERMINISTIC PARAMETERS",
    probabilistic = "PROBABILISTIC PARAMETERS",
    scenarios = "SCENARIO PARAMETERS"
  )

  # ============================================================
  # 2. Helper: format distribution argument values
  # ============================================================

  format_value <- function(value) {

    if (length(value) == 0L || all(is.na(value))) {
      return(NA_character_)
    }

    if (is.numeric(value)) {
      return(
        paste(
          format(
            value,
            trim = TRUE,
            scientific = FALSE
          ),
          collapse = ", "
        )
      )
    }

    paste(as.character(value), collapse = ", ")
  }

  # ============================================================
  # 3. Helper: build probabilistic display table
  # ============================================================

  format_probabilistic_table <- function(table) {

    common_cols <- c(
      "parameter",
      "type",
      "resource",
      "comparison"
    )

    distribution_col <- "distribution"

    if (!distribution_col %in% names(table)) {
      stop(
        "The probabilistic parameter table does not ",
        "contain a 'distribution' column."
      )
    }

    argument_cols <- setdiff(
      names(table),
      c(common_cols, distribution_col)
    )

    distribution_names <- c(
      normal = "Normal",
      beta = "Beta",
      gamma = "Gamma",
      lognormal = "Lognormal",
      uniform = "Uniform"
    )

    specifications <- vapply(
      seq_len(nrow(table)),
      function(i) {

        distribution <- as.character(
          table[[distribution_col]][i]
        )

        distribution_key <- tolower(distribution)

        if (
          !is.na(distribution_key) &&
          distribution_key %in% names(distribution_names)
        ) {
          display_name <- unname(
            distribution_names[[distribution_key]]
          )
        } else {
          display_name <- distribution
        }

        arguments <- vapply(
          argument_cols,
          function(argument) {

            value <- table[[argument]][i]

            if (
              length(value) == 0L ||
              all(is.na(value))
            ) {
              return(NA_character_)
            }

            paste0(
              argument,
              " = ",
              format_value(value)
            )
          },
          character(1)
        )

        arguments <- arguments[!is.na(arguments)]

        paste0(
          display_name,
          "(",
          paste(arguments, collapse = ", "),
          ")"
        )
      },
      character(1)
    )

    # Preserve common parameter information
    available_common_cols <- intersect(
      common_cols,
      names(table)
    )

    display_table <- table[
      ,
      available_common_cols,
      drop = FALSE
    ]

    # Add formatted distribution specification
    display_table$Specification <- specifications

    # Format column names
    names(display_table) <- c(
      tools::toTitleCase(available_common_cols),
      "Specification"
    )

    # Replace missing metadata for console display
    for (column in names(display_table)) {

      if (is.factor(display_table[[column]])) {
        display_table[[column]] <- as.character(
          display_table[[column]]
        )
      }

      if (is.character(display_table[[column]])) {

        missing_values <- is.na(
          display_table[[column]]
        ) | display_table[[column]] == ""

        display_table[[column]][missing_values] <- "-"
      }
    }

    rownames(display_table) <- NULL

    display_table
  }

  # ============================================================
  # 4. Print parameter summary
  # ============================================================

  cat("\nCEA PARAMETER SUMMARY\n")
  cat(strrep("=", 60), "\n", sep = "")

  for (specification in names(titles)) {

    table <- x[[specification]]

    if (is.null(table)) {
      next
    }

    cat(
      "\n",
      titles[[specification]],
      "\n",
      sep = ""
    )

    cat(strrep("-", 60), "\n", sep = "")

    if (specification == "probabilistic") {

      # Compact probabilistic presentation
      display_table <- format_probabilistic_table(table)

      print(
        display_table,
        row.names = FALSE,
        right = FALSE,
        quote = FALSE
      )

    } else {

      # Preserve deterministic and scenario presentation
      print(
        table,
        row.names = FALSE,
        na.print = "-"
      )
    }

    cat("\n")
  }

  invisible(x)
}
