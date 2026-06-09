# Re-export generics::fit so users get fit() without loading generics separately.
#' @importFrom generics fit
#' @export
generics::fit

#' Sample synthetic rows from a fitted synthesizer
#'
#' Dispatches to the synthesizer-specific method when `x` is an
#' `rsdv_synthesizer`. For plain R vectors, integers, or characters it
#' falls back to [base::sample()], preserving backward compatibility.
#'
#' @param x A fitted synthesizer object, or a vector for [base::sample()] compat.
#' @param n Number of synthetic rows to generate (synthesizer path), or
#'   sample size (base::sample path).
#' @param ... Additional arguments passed to the method or to [base::sample()].
#' @return When `x` inherits from `rsdv_synthesizer`, a data frame of `n`
#'   synthetic rows whose columns match the metadata. When `x` is any other
#'   object, the value returned by [base::sample()] — typically a vector of
#'   the same type as `x` and length `n`.
#' @export
#' @examples
#' # Falls back to base::sample for non-synthesizer objects:
#' sample(1:10, 3)
#'
#' \donttest{
#' meta  <- metadata(adult_income) |>
#'   set_column_type("age",    "numerical") |>
#'   set_column_type("income", "categorical")
#' syn   <- gaussian_copula_synthesizer(meta) |> fit(adult_income)
#' synth <- sample(syn, n = 100)
#' head(synth)
#' }
sample <- function(x, n = NULL, ...) {
  if (inherits(x, "rsdv_synthesizer")) {
    UseMethod("sample")
  } else if (is.null(n)) {
    base::sample(x, ...)
  } else {
    base::sample(x, size = n, ...)
  }
}

#' Check whether a synthesizer has been fitted
#'
#' @param x A synthesizer object.
#' @return `TRUE` if [fit()] has been called; `FALSE` otherwise.
#' @export
#' @examples
#' syn <- gaussian_copula_synthesizer(metadata())
#' is_fitted(syn)  # FALSE before fitting
is_fitted <- function(x) {
  isTRUE(x$fitted)
}

#' Validate that a data frame is compatible with metadata
#'
#' Checks that all columns registered in `meta` are present in `data`.
#'
#' @param data A data frame.
#' @param meta An `rsdv_metadata` object.
#' @return Invisibly `TRUE`; throws an error if validation fails.
#' @export
#' @examples
#' meta <- metadata() |> set_column_type("age", "numerical")
#' validate_data(data.frame(age = 1:5), meta)
validate_data <- function(data, meta) {
  expected     <- names(meta$columns)
  missing_cols <- setdiff(expected, names(data))
  if (length(missing_cols) > 0L) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  invisible(TRUE)
}
