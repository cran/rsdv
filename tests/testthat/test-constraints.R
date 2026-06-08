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
