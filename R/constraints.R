#' Add a constraint to metadata
#'
#' @param meta An `rsdv_metadata` object.
#' @param constraint A constraint object from [equality_constraint()],
#'   [inequality_constraint()], [fixed_combinations_constraint()], or
#'   [custom_constraint()].
#' @return Updated `rsdv_metadata` (for piping).
#' @export
#' @examples
#' meta <- metadata() |>
#'   set_column_type("low", "numerical") |>
#'   set_column_type("high", "numerical") |>
#'   add_constraint(inequality_constraint("low", "high", type = "lt"))
add_constraint <- function(meta, constraint) {
  meta$constraints <- c(meta$constraints, list(constraint))
  meta
}

#' Constraint: two columns must be equal row-wise
#' @param col_a,col_b Column names (character).
#' @return An `rsdv_constraint` object.
#' @export
#' @examples
#' equality_constraint("city", "city_copy")
equality_constraint <- function(col_a, col_b) {
  structure(
    list(type = "equality", col_a = col_a, col_b = col_b),
    class = c("equality_constraint", "rsdv_constraint")
  )
}

#' Constraint: col_a must be less than / greater than col_b
#' @param col_a,col_b Column names (character).
#' @param type One of `"lt"`, `"lte"`, `"gt"`, `"gte"`.
#' @return An `rsdv_constraint` object.
#' @export
#' @examples
#' inequality_constraint("low", "high", type = "lt")
inequality_constraint <- function(col_a, col_b, type = c("lt", "lte", "gt", "gte")) {
  type <- match.arg(type)
  structure(
    list(type = "inequality", col_a = col_a, col_b = col_b, direction = type),
    class = c("inequality_constraint", "rsdv_constraint")
  )
}

#' Constraint: only observed column combinations are valid
#' @param columns Character vector of column names.
#' @param reference_data Data frame containing the allowed combinations.
#' @return An `rsdv_constraint` object.
#' @export
#' @examples
#' ref <- data.frame(city = c("NY", "LA"), state = c("NY", "CA"),
#'                   stringsAsFactors = FALSE)
#' fixed_combinations_constraint(c("city", "state"), ref)
fixed_combinations_constraint <- function(columns, reference_data) {
  allowed <- unique(reference_data[, columns, drop = FALSE])
  structure(
    list(type = "fixed_combinations", columns = columns, allowed = allowed),
    class = c("fixed_combinations_constraint", "rsdv_constraint")
  )
}

#' Constraint: arbitrary row-wise predicate
#' @param fn A function `f(row)` accepting a one-row data frame, returning
#'   a single logical.
#' @return An `rsdv_constraint` object.
#' @export
#' @examples
#' custom_constraint(function(row) row$x > 0)
custom_constraint <- function(fn) {
  structure(
    list(type = "custom", fn = fn),
    class = c("custom_constraint", "rsdv_constraint")
  )
}

#' Check a single constraint against each row of a data frame
#'
#' @param data A data frame.
#' @param constraint An `rsdv_constraint` object.
#' @return Logical vector of length `nrow(data)`.
#' @export
#' @examples
#' df <- data.frame(a = c(1, 2, 3), b = c(1, 2, 9))
#' check_constraint(df, equality_constraint("a", "b"))
check_constraint <- function(data, constraint) {
  UseMethod("check_constraint", constraint)
}

#' @export
check_constraint.equality_constraint <- function(data, constraint) {
  data[[constraint$col_a]] == data[[constraint$col_b]]
}

#' @export
check_constraint.inequality_constraint <- function(data, constraint) {
  a <- data[[constraint$col_a]]
  b <- data[[constraint$col_b]]
  switch(constraint$direction,
    lt  = a < b,
    lte = a <= b,
    gt  = a > b,
    gte = a >= b
  )
}

#' @export
check_constraint.fixed_combinations_constraint <- function(data, constraint) {
  # Use paste-based key comparison to preserve column types (avoids apply()
  # coercing mixed-type data frames to character matrix).
  sep <- "\031"  # ASCII unit-separator, unlikely in real data
  test_keys    <- do.call(paste, c(data[, constraint$columns, drop = FALSE],
                                   list(sep = sep)))
  allowed_keys <- do.call(paste, c(constraint$allowed, list(sep = sep)))
  test_keys %in% allowed_keys
}

#' @export
check_constraint.default <- function(data, constraint) {
  stop(sprintf(
    "No check_constraint method for class '%s'.",
    paste(class(constraint), collapse = "/")
  ))
}

#' @export
check_constraint.custom_constraint <- function(data, constraint) {
  vapply(seq_len(nrow(data)), function(i) {
    isTRUE(constraint$fn(data[i, , drop = FALSE]))
  }, logical(1))
}

#' Check all constraints in metadata against a data frame
#'
#' @param data A data frame.
#' @param meta An `rsdv_metadata` object.
#' @return Logical vector of length `nrow(data)`. `TRUE` = row passes all constraints.
#' @export
#' @examples
#' meta <- metadata() |>
#'   set_column_type("x", "numerical") |>
#'   add_constraint(custom_constraint(function(row) row$x > 0))
#' check_constraints(data.frame(x = c(1, -1, 2)), meta)
check_constraints <- function(data, meta) {
  if (length(meta$constraints) == 0L) return(rep(TRUE, nrow(data)))
  results <- lapply(meta$constraints, function(c_obj) check_constraint(data, c_obj))
  Reduce(`&`, results)
}
