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
#'
#' For continuous numerical columns, exact `==` is almost never satisfied by
#' the copula sampler; use the `tolerance` argument or
#' [inequality_constraint()] with a narrow band. With `tolerance > 0`, equality
#' is `abs(a - b) <= tolerance` for numeric columns and exact `==` otherwise.
#'
#' @param col_a,col_b Column names (character).
#' @param tolerance Numeric. When non-zero, numeric columns compare with
#'   `abs(a - b) <= tolerance` instead of exact `==`. Ignored for
#'   non-numeric columns. Default `0` (exact equality).
#' @return An `rsdv_constraint` object.
#' @export
#' @examples
#' equality_constraint("city", "city_copy")
#' equality_constraint("price_left", "price_right", tolerance = 1e-6)
equality_constraint <- function(col_a, col_b, tolerance = 0) {
  if (!is.numeric(tolerance) || length(tolerance) != 1L ||
      !is.finite(tolerance) || tolerance < 0)
    stop("`tolerance` must be a single non-negative finite number.")
  structure(
    list(type = "equality", col_a = col_a, col_b = col_b,
         tolerance = tolerance),
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
#'
#' With `vectorized = FALSE` (default) `fn` is invoked once per row with a
#' one-row data frame and must return a single logical — easy to write but
#' slow on large frames. With `vectorized = TRUE` `fn` is invoked **once**
#' with the full data frame and must return a logical vector of length
#' `nrow(data)`; use this when your predicate is vectorisable for substantial
#' speedups on large synthetic samples.
#'
#' @param fn A predicate function. If `vectorized = FALSE`, signature is
#'   `f(row)` returning a single logical. If `vectorized = TRUE`, signature
#'   is `f(data)` returning a logical vector of length `nrow(data)`.
#' @param vectorized Logical. See above. Default `FALSE`.
#' @return An `rsdv_constraint` object.
#' @export
#' @examples
#' custom_constraint(function(row) row$x > 0)
#' # Vectorised — usually much faster:
#' custom_constraint(function(data) data$x > 0, vectorized = TRUE)
custom_constraint <- function(fn, vectorized = FALSE) {
  if (!is.function(fn)) stop("`fn` must be a function.")
  structure(
    list(type = "custom", fn = fn, vectorized = isTRUE(vectorized)),
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
  a <- data[[constraint$col_a]]
  b <- data[[constraint$col_b]]
  tol <- constraint$tolerance %||% 0
  # NA == NA is NA in base R; for constraint checking we treat any NA-involving
  # row as failing the constraint (so it is rejected by the rejection sampler)
  # rather than letting NA propagate into the row selector.
  res <- if (tol > 0 && is.numeric(a) && is.numeric(b)) {
    abs(a - b) <= tol
  } else {
    a == b
  }
  res & !is.na(res)
}

#' @export
check_constraint.inequality_constraint <- function(data, constraint) {
  a <- data[[constraint$col_a]]
  b <- data[[constraint$col_b]]
  res <- switch(constraint$direction,
    lt  = a < b,
    lte = a <= b,
    gt  = a > b,
    gte = a >= b
  )
  res & !is.na(res)  # NA -> FALSE (row fails); see equality method.
}

#' @export
check_constraint.fixed_combinations_constraint <- function(data, constraint) {
  # Length-prefix encoding: each field is "<nchar>:<value>" joined by '|'.
  # A reader doesn't parse the inner characters — the length prefix says how
  # many follow — so the encoding is collision-free regardless of what
  # characters (':', '|', newlines, etc.) appear inside the field values.
  test_keys    <- .row_keys(data,              constraint$columns)
  allowed_keys <- .row_keys(constraint$allowed, constraint$columns)
  test_keys %in% allowed_keys
}

#' @noRd
.row_keys <- function(df, columns) {
  cols <- lapply(columns, function(col) {
    v <- as.character(df[[col]])
    # Length-prefix each non-NA value so any internal delimiter is harmless;
    # encode NA as a sentinel that no length-prefixed value can collide with.
    ifelse(is.na(v), "<NA>", paste0(nchar(v), ":", v))
  })
  do.call(paste, c(cols, list(sep = "|")))
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
  if (isTRUE(constraint$vectorized)) {
    res <- constraint$fn(data)
    if (!is.logical(res) || length(res) != nrow(data))
      stop("A vectorized custom_constraint must return a logical vector of length nrow(data).")
    res & !is.na(res)
  } else {
    vapply(seq_len(nrow(data)), function(i) {
      isTRUE(constraint$fn(data[i, , drop = FALSE]))
    }, logical(1))
  }
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

# Print methods --------------------------------------------------------------

#' Print method for an equality_constraint
#' @param x An `equality_constraint` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.equality_constraint <- function(x, ...) {
  tol <- x$tolerance %||% 0
  if (tol > 0)
    cat(sprintf("<equality_constraint>  abs(%s - %s) <= %g\n",
                x$col_a, x$col_b, tol))
  else
    cat(sprintf("<equality_constraint>  %s == %s\n", x$col_a, x$col_b))
  invisible(x)
}

#' Print method for an inequality_constraint
#' @param x An `inequality_constraint` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.inequality_constraint <- function(x, ...) {
  op <- switch(x$direction, lt = "<", lte = "<=", gt = ">", gte = ">=")
  cat(sprintf("<inequality_constraint>  %s %s %s\n", x$col_a, op, x$col_b))
  invisible(x)
}

#' Print method for a fixed_combinations_constraint
#' @param x A `fixed_combinations_constraint` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.fixed_combinations_constraint <- function(x, ...) {
  cat(sprintf(
    "<fixed_combinations_constraint>  %s  (%d allowed combination%s)\n",
    paste(x$columns, collapse = ", "),
    nrow(x$allowed), if (nrow(x$allowed) == 1L) "" else "s"
  ))
  invisible(x)
}

#' Print method for a custom_constraint
#' @param x A `custom_constraint` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.custom_constraint <- function(x, ...) {
  cat(sprintf("<custom_constraint>  %s\n",
              if (isTRUE(x$vectorized)) "vectorised predicate" else "row-wise predicate"))
  invisible(x)
}
