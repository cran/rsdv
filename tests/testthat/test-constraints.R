test_that("add_constraint() stores a constraint in metadata", {
  meta <- small_meta() |>
    add_constraint(equality_constraint("age", "age"))
  expect_length(meta$constraints, 1L)
})

test_that("equality_constraint() validates rows where col_a == col_b", {
  df <- data.frame(a = c(1, 2, 3), b = c(1, 2, 9))
  c_obj <- equality_constraint("a", "b")
  valid <- check_constraint(df, c_obj)
  expect_equal(valid, c(TRUE, TRUE, FALSE))
})

test_that("inequality_constraint() validates a < b", {
  df <- data.frame(low = c(1, 5, 10), high = c(5, 3, 20))
  c_obj <- inequality_constraint("low", "high", type = "lt")
  valid <- check_constraint(df, c_obj)
  expect_equal(valid, c(TRUE, FALSE, TRUE))
})

test_that("fixed_combinations_constraint() rejects unseen combinations", {
  real_df  <- data.frame(city = c("NY", "NY", "LA"), state = c("NY", "NY", "CA"),
                         stringsAsFactors = FALSE)
  c_obj    <- fixed_combinations_constraint(c("city", "state"), real_df)
  test_df  <- data.frame(city = c("NY", "LA", "NY"), state = c("NY", "CA", "TX"),
                         stringsAsFactors = FALSE)
  valid    <- check_constraint(test_df, c_obj)
  expect_equal(valid, c(TRUE, TRUE, FALSE))
})

test_that("custom_constraint() applies an arbitrary predicate", {
  df    <- data.frame(x = c(1, -1, 2))
  c_obj <- custom_constraint(function(row) row$x > 0)
  valid <- check_constraint(df, c_obj)
  expect_equal(valid, c(TRUE, FALSE, TRUE))
})

test_that("fixed_combinations_constraint() handles numeric columns without coercion", {
  real_df <- data.frame(x = c(1.0, 2.0), y = c(10L, 20L))
  c_obj   <- fixed_combinations_constraint(c("x", "y"), real_df)
  test_df <- data.frame(x = c(1.0, 3.0), y = c(10L, 30L))
  valid   <- check_constraint(test_df, c_obj)
  expect_equal(valid, c(TRUE, FALSE))
})

test_that("check_constraints() returns all-TRUE when no constraints", {
  df   <- small_data()
  meta <- small_meta()
  expect_true(all(check_constraints(df, meta)))
})

test_that("equality_constraint returns FALSE (not NA) for NA-containing rows", {
  df <- data.frame(a = c(1, NA, 3, 3), b = c(1, NA, 3, 9))
  res <- check_constraint(df, equality_constraint("a", "b"))
  expect_type(res, "logical")
  expect_false(any(is.na(res)))
  expect_identical(res, c(TRUE, FALSE, TRUE, FALSE))
})

test_that("inequality_constraint returns FALSE (not NA) for NA-containing rows", {
  df  <- data.frame(low = c(1, NA, 5, 7), high = c(2, 3, NA, 4))
  res <- check_constraint(df, inequality_constraint("low", "high", type = "lt"))
  expect_type(res, "logical")
  expect_false(any(is.na(res)))
  expect_identical(res, c(TRUE, FALSE, FALSE, FALSE))
})

test_that("check_constraints over a frame with NAs produces no NA in the selector", {
  meta <- metadata() |>
    set_column_type("a", "numerical") |>
    set_column_type("b", "numerical") |>
    add_constraint(equality_constraint("a", "b"))
  df <- data.frame(a = c(1, NA, 3), b = c(1, NA, 3))
  sel <- check_constraints(df, meta)
  expect_false(any(is.na(sel)))
  # Down-stream pattern: df[sel, ] must not produce phantom NA rows.
  expect_equal(nrow(df[sel, , drop = FALSE]), 2L)
})

# --- issue #12 follow-ups -------------------------------------------------

test_that("equality_constraint(tolerance > 0) does approximate equality on numerics", {
  c_tol <- equality_constraint("a", "b", tolerance = 0.01)
  df <- data.frame(a = c(1.000, 1.005, 1.020),
                   b = c(1.000, 1.000, 1.000))
  expect_identical(check_constraint(df, c_tol), c(TRUE, TRUE, FALSE))

  # Tolerance is ignored for non-numeric columns (falls back to exact ==).
  df_char <- data.frame(a = c("x", "y", "z"), b = c("x", "y", "Z"),
                        stringsAsFactors = FALSE)
  expect_identical(check_constraint(df_char, c_tol), c(TRUE, TRUE, FALSE))
})

test_that("equality_constraint validates tolerance argument", {
  expect_error(equality_constraint("a", "b", tolerance = -1), "non-negative")
  expect_error(equality_constraint("a", "b", tolerance = c(0, 1)), "single")
  expect_error(equality_constraint("a", "b", tolerance = "x"), "non-negative")
})

test_that("fixed_combinations_constraint key encoding is collision-free", {
  # Without length-prefixing, paste-separator collisions are possible when
  # adjacent fields concatenate to the same string under the separator. Use a
  # carefully chosen pair that would alias under any single-char separator.
  ref <- data.frame(a = c("foo", "foob"),
                    b = c("|bar", "ar"),  # contains the candidate separator
                    stringsAsFactors = FALSE)
  fc  <- fixed_combinations_constraint(c("a", "b"), ref)
  # The two reference rows differ; both should still be findable, and a row
  # that "spans the boundary" should NOT match.
  expect_identical(
    check_constraint(
      data.frame(a = c("foo", "foob", "foo|", "fooba", "foob"),
                 b = c("|bar", "ar", "bar", "r", "|bar"),
                 stringsAsFactors = FALSE),
      fc
    ),
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("custom_constraint(vectorized = TRUE) calls fn once with the whole frame", {
  calls <- 0L
  fn <- function(data) { calls <<- calls + 1L; data$x > 0 }
  cc  <- custom_constraint(fn, vectorized = TRUE)
  out <- check_constraint(data.frame(x = c(-1, 1, 2, -3)), cc)
  expect_identical(out, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(calls, 1L)
})

test_that("vectorized custom_constraint errors when fn returns a wrong-shape result", {
  bad_cc <- custom_constraint(function(data) "nope", vectorized = TRUE)
  expect_error(
    check_constraint(data.frame(x = 1:3), bad_cc),
    "must return a logical vector"
  )
})

test_that("print methods for constraint objects produce readable output", {
  expect_output(print(equality_constraint("a", "b")),
                "equality_constraint.*a == b")
  expect_output(print(equality_constraint("a", "b", tolerance = 1e-3)),
                "abs[(]a - b[)] <= 0[.]001")
  expect_output(print(inequality_constraint("low", "high", type = "lt")),
                "low < high")
  ref <- data.frame(city = c("NY", "LA"), state = c("NY", "CA"),
                    stringsAsFactors = FALSE)
  expect_output(print(fixed_combinations_constraint(c("city","state"), ref)),
                "city, state.*2 allowed combinations")
  expect_output(print(custom_constraint(function(r) r$x > 0)),
                "row-wise predicate")
  expect_output(print(custom_constraint(function(d) d$x > 0, vectorized = TRUE)),
                "vectorised predicate")
})
