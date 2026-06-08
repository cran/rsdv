## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(rsdv)

set.seed(42)

meta  <- metadata(adult_income) |>
  set_column_type("age",        "numerical") |>
  set_column_type("occupation", "categorical") |>
  set_column_type("income",     "categorical")

syn   <- gaussian_copula_synthesizer(meta)
syn   <- fit(syn, adult_income)
synthetic_data <- sample(syn, n = nrow(adult_income))

