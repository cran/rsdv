#' Create a metadata object describing a dataset's column types
#'
#' @param data Optional data frame. If supplied, column types are
#'   auto-detected. You can override them with [set_column_type()].
#' @return An `rsdv_metadata` object.
#' @export
#' @examples
#' meta <- metadata(adult_income) |>
#'   set_column_type("age", "numerical") |>
#'   set_column_type("occupation", "categorical")
metadata <- function(data = NULL) {
  meta <- structure(
    list(columns = list(), primary_key = NULL, constraints = list()),
    class = "rsdv_metadata"
  )
  if (!is.null(data)) {
    meta <- auto_detect_columns(meta, data)
  }
  meta
}

#' @noRd
auto_detect_columns <- function(meta, data) {
  for (col in names(data)) {
    type <- detect_column_type(data[[col]])
    meta <- set_column_type(meta, col, type)
  }
  meta
}

#' @noRd
detect_column_type <- function(x) {
  if (is.logical(x)) return("boolean")
  if (inherits(x, "Date") || inherits(x, "POSIXct")) return("datetime")
  if (is.numeric(x)) return("numerical")
  if (is.factor(x) || is.character(x)) return("categorical")
  "categorical"
}

#' Set the type of a column in metadata
#'
#' @param meta An `rsdv_metadata` object.
#' @param column Column name (character).
#' @param type One of `"numerical"`, `"categorical"`, `"boolean"`,
#'   `"datetime"`, `"id"`.
#'
#' For categorical columns the level *order* used by the synthesizer follows
#' the input: a `factor` keeps its `levels()` order (including ordered
#' factors), while a plain character column gets a sorted unique-value order
#' for determinism. The sort is **lexicographic**, so numeric-like character
#' columns (`c("2", "10")`) come back ordered `"10", "2"`. Coerce these to
#' `factor` with the desired level order before fitting if order matters.
#'
#' @return The updated `rsdv_metadata` object (for piping).
#' @examples
#' metadata() |> set_column_type("age", "numerical")
#' @export
set_column_type <- function(meta, column, type) {
  if (!type %in% VALID_COLUMN_TYPES) {
    stop(sprintf(
      "Invalid column type '%s'. Must be one of: %s",
      type, paste(VALID_COLUMN_TYPES, collapse = ", ")
    ))
  }
  meta$columns[[column]] <- list(type = type)
  meta
}

#' Set the primary key column of the metadata
#'
#' @param meta An `rsdv_metadata` object.
#' @param column Name of the primary key column. Must already be registered
#'   via [set_column_type()].
#' @return The updated `rsdv_metadata` object (for piping).
#' @export
#' @examples
#' meta <- metadata() |>
#'   set_column_type("id", "id") |>
#'   set_primary_key("id")
set_primary_key <- function(meta, column) {
  if (!column %in% names(meta$columns)) {
    stop(sprintf(
      "Column '%s' not found in metadata. Register it with set_column_type() first.",
      column
    ))
  }
  col_type <- meta$columns[[column]]$type
  if (!identical(col_type, "id"))
    warning(sprintf(
      "Primary key column '%s' has type '%s', not 'id'. Synthesis will model it as %s data, which usually produces non-unique values -- set its type to 'id' if you want it excluded from synthesis.",
      column, col_type, col_type
    ), call. = FALSE)
  meta$primary_key <- column
  meta
}

#' Print method for rsdv_metadata
#'
#' @param x An `rsdv_metadata` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @examples
#' print(metadata())
#' @export
print.rsdv_metadata <- function(x, ...) {
  cat("rsdv Metadata\n")
  cat("  Columns:", length(x$columns), "\n")
  for (col in names(x$columns)) {
    cat(sprintf("    %s [%s]\n", col, x$columns[[col]]$type))
  }
  if (!is.null(x$primary_key)) {
    cat("  Primary key:", x$primary_key, "\n")
  }
  invisible(x)
}
