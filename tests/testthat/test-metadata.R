test_that("metadata() creates empty metadata object", {
  meta <- metadata()
  expect_s3_class(meta, "rsdv_metadata")
  expect_equal(meta$columns, list())
  expect_null(meta$primary_key)
})

test_that("metadata() auto-detects column types from data", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  meta <- metadata(df)
  expect_equal(meta$columns[["x"]]$type, "numerical")
  expect_equal(meta$columns[["y"]]$type, "categorical")
})

test_that("set_column_type() sets a column type and is pipe-chainable", {
  meta <- metadata() |>
    set_column_type("age", "numerical") |>
    set_column_type("occupation", "categorical")
  expect_equal(meta$columns[["age"]]$type, "numerical")
  expect_equal(meta$columns[["occupation"]]$type, "categorical")
})

test_that("set_column_type() rejects invalid types", {
  meta <- metadata()
  expect_error(set_column_type(meta, "x", "foobar"), "Invalid column type")
})

test_that("get_columns_by_type() returns correct column names", {
  meta <- metadata() |>
    set_column_type("age", "numerical") |>
    set_column_type("name", "categorical") |>
    set_column_type("score", "numerical")
  expect_equal(sort(get_columns_by_type(meta, "numerical")), c("age", "score"))
  expect_equal(get_columns_by_type(meta, "categorical"), "name")
})

test_that("print.rsdv_metadata() produces output without error", {
  meta <- metadata() |> set_column_type("age", "numerical")
  expect_output(print(meta), "rsdv Metadata")
  expect_output(print(meta), "age")
})

test_that("set_primary_key() warns when the column type is not 'id'", {
  meta <- metadata() |> set_column_type("age", "numerical")
  expect_warning(set_primary_key(meta, "age"),
                 "type 'numerical', not 'id'")
})

test_that("set_primary_key() does not warn for an 'id'-typed column", {
  meta <- metadata() |> set_column_type("id", "id")
  expect_no_warning(set_primary_key(meta, "id"))
})
