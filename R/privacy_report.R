#' Generate a privacy report comparing real and synthetic data
#'
#' @param real A data frame of real data.
#' @param synthetic A data frame of synthetic data.
#' @param sensitive_col Optional. Column name for attribute disclosure risk.
#' @param known_cols Optional. Column names known to an adversary (required if
#'   `sensitive_col` is supplied).
#' @return An `rsdv_privacy_report` object.
#' @export
#' @examples
#' \donttest{
#' syn   <- gaussian_copula_synthesizer(metadata(adult_income)) |>
#'   fit(adult_income)
#' synth <- sample(syn, n = 500)
#' pr    <- privacy_report(adult_income, synth)
#' print(pr)
#' }
privacy_report <- function(real, synthetic,
                            sensitive_col = NULL, known_cols = NULL) {
  nndr_sc <- nndr(real, synthetic)

  disclosure <- if (!is.null(sensitive_col) && !is.null(known_cols)) {
    attribute_disclosure_risk(real, synthetic,
                              sensitive_col = sensitive_col,
                              known_cols    = known_cols)
  } else {
    NULL
  }

  structure(
    list(
      nndr_score      = nndr_sc,
      disclosure_risk = disclosure
    ),
    class = "rsdv_privacy_report"
  )
}

#' Print method for rsdv_privacy_report
#'
#' @param x An `rsdv_privacy_report` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @examples
#' \donttest{
#' syn <- gaussian_copula_synthesizer(metadata(adult_income)) |> fit(adult_income)
#' synth <- sample(syn, n = 500)
#' pr <- privacy_report(adult_income, synth)
#' print(pr)
#' }
#' @export
print.rsdv_privacy_report <- function(x, ...) {
  cat("== rsdv Privacy Report ==\n\n")
  cat(sprintf("NNDR Score (higher = more private):  %.3f\n", x$nndr_score))
  if (!is.null(x$disclosure_risk)) {
    cat(sprintf("Attribute Disclosure Risk (lower = better): %.3f\n",
                x$disclosure_risk))
  }
  invisible(x)
}
