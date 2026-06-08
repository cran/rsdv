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
