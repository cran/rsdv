# Shared fixtures loaded automatically by testthat before every test file.

small_data <- function() {
  data.frame(
    age    = c(25L, 40L, 35L, 52L, 23L),
    income = c(30000, 80000, 55000, 120000, 28000),
    edu    = c("HS", "College", "College", "Grad", "HS"),
    stringsAsFactors = FALSE
  )
}

small_meta <- function() {
  # Intentionally set the primary key on a numerical column (`age`) so the
  # fixture exercises the non-id PK path; suppress the resulting advisory
  # warning here so the noise doesn't surface in every test that uses the
  # fixture.
  suppressWarnings(
    metadata() |>
      set_column_type("age", "numerical") |>
      set_column_type("income", "numerical") |>
      set_column_type("edu", "categorical") |>
      set_primary_key("age")
  )
}
