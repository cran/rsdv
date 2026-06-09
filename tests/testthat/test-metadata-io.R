test_that("set_primary_key() stores the key and is pipe-chainable", {
  meta <- metadata() |>
    set_column_type("id", "id") |>
    set_primary_key("id")
  expect_equal(meta$primary_key, "id")
})

test_that("set_primary_key() errors if column not in metadata", {
  meta <- metadata()
  expect_error(set_primary_key(meta, "missing"), "not found")
})

test_that("metadata_to_json() produces valid JSON", {
  meta <- small_meta()
  json <- metadata_to_json(meta)
  expect_type(json, "character")
  parsed <- jsonlite::fromJSON(json)
  expect_true("columns" %in% names(parsed))
})

test_that("metadata round-trips through JSON", {
  meta <- small_meta()
  json <- metadata_to_json(meta)
  meta2 <- metadata_from_json(json)
  expect_equal(meta2$columns, meta$columns)
  expect_equal(meta2$primary_key, meta$primary_key)
})

test_that("save_metadata() and load_metadata() round-trip via file", {
  meta <- small_meta()
  path <- withr::local_tempfile(fileext = ".json")
  save_metadata(meta, path)
  meta2 <- load_metadata(path)
  expect_equal(meta2$columns, meta$columns)
})

# --- constraint round-trip (issue #12, S1-1 + S1-2) -------------------------

test_that("metadata_to_json/from_json round-trips equality and inequality constraints", {
  m <- metadata() |>
    set_column_type("a", "numerical") |>
    set_column_type("b", "numerical") |>
    add_constraint(equality_constraint("a", "b")) |>
    add_constraint(inequality_constraint("a", "b", type = "lt"))
  m2 <- metadata_from_json(metadata_to_json(m))

  # S3 classes survive — check_constraint dispatches correctly.
  expect_s3_class(m2$constraints[[1]], "equality_constraint")
  expect_s3_class(m2$constraints[[2]], "inequality_constraint")

  # Semantics survive too.
  df <- data.frame(a = c(1, 2, 3), b = c(2, 2, 9))
  expect_identical(check_constraint(df, m2$constraints[[1]]),
                   c(FALSE, TRUE, FALSE))
  expect_identical(check_constraint(df, m2$constraints[[2]]),
                   c(TRUE, FALSE, TRUE))
})

test_that("metadata_to_json/from_json round-trips fixed_combinations_constraint", {
  ref <- data.frame(city  = c("NY", "LA", "SF"),
                    state = c("NY", "CA", "CA"),
                    stringsAsFactors = FALSE)
  m <- metadata() |>
    set_column_type("city",  "categorical") |>
    set_column_type("state", "categorical") |>
    add_constraint(fixed_combinations_constraint(c("city", "state"), ref))
  m2 <- metadata_from_json(metadata_to_json(m))

  expect_s3_class(m2$constraints[[1]], "fixed_combinations_constraint")
  expect_s3_class(m2$constraints[[1]]$allowed, "data.frame")

  # Allowed table preserved up to row order; check membership semantics.
  test_df <- data.frame(city  = c("NY", "NY", "SF"),
                        state = c("NY", "CA", "CA"),
                        stringsAsFactors = FALSE)
  expect_identical(check_constraint(test_df, m2$constraints[[1]]),
                   c(TRUE, FALSE, TRUE))
})

test_that("metadata_to_json warns and drops custom_constraint (closures aren't serializable)", {
  m <- metadata() |>
    set_column_type("x", "numerical") |>
    add_constraint(custom_constraint(function(row) row$x > 0))
  expect_warning(j <- metadata_to_json(m), "custom_constraint cannot be serialized")
  m2 <- metadata_from_json(j)
  expect_equal(length(m2$constraints), 0L)
})

test_that("save_metadata / load_metadata round-trip survives constraints", {
  m <- metadata() |>
    set_column_type("a", "numerical") |>
    set_column_type("b", "numerical") |>
    add_constraint(inequality_constraint("a", "b", type = "lt"))
  path <- tempfile(fileext = ".json")
  save_metadata(m, path)
  m2 <- load_metadata(path)
  expect_s3_class(m2$constraints[[1]], "inequality_constraint")
})
