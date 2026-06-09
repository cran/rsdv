
# rsdv — The R Synthetic Data Vault

<!-- badges: start -->

[![R-CMD-check](https://github.com/kvenkita/rsdv/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/kvenkita/rsdv/actions)
<!-- badges: end -->

**Synthetic data generation in R (Gaussian Copula based, extensible to
deep generative models)**

`rsdv` is an R implementation of Python’s [Synthetic Data Vault
(SDV)](https://sdv.dev/) framework (Patki, Wedge, and Veeramachaneni
2016). It generates synthetic tabular data using Gaussian copula models,
with built-in quality and privacy evaluation.

## Installation

``` r
# Development version
remotes::install_github("kvenkita/rsdv")
```

## Quick start

``` r
library(rsdv)
#> 
#> Attaching package: 'rsdv'
#> The following object is masked from 'package:base':
#> 
#>     sample

set.seed(42)

# Describe column types
meta <- metadata(adult_income) |>
  set_column_type("id",         "id") |>
  set_column_type("age",        "numerical") |>
  set_column_type("occupation", "categorical") |>
  set_column_type("income",     "categorical") |>
  set_primary_key("id")

# Fit a GaussianCopula synthesizer
syn       <- gaussian_copula_synthesizer(meta)
syn       <- fit(syn, adult_income)

# Generate 500 synthetic rows
synth_data <- sample(syn, n = 500)

# Evaluate quality
qr <- quality_report(real = adult_income, synthetic = synth_data,
                     metadata = meta)
print(qr)
#> == rsdv Quality Report ==
#> 
#> Column Similarity (KS, numerical):
#>   age                  0.960
#>   fnlwgt               0.936
#>   education_num        0.776
#>   capital_gain         0.468
#>   capital_loss         0.484
#>   hours_per_week       0.724
#> 
#> Column Similarity (TVD, categorical):
#>   workclass            0.973
#>   education            0.942
#>   marital_status       0.988
#>   occupation           0.935
#>   relationship         0.970
#>   race                 0.988
#>   sex                  1.000
#>   native_country       0.956
#>   income               0.972
#> 
#> Property scores:
#>   Column Shapes        0.871
#>   Column Pair Trends   0.893
#>     (correlation 0.965, contingency 0.864)
#> 
#> Overall Score:               0.882
```

`quality_report()` aggregates metrics into the two-property hierarchy
used by SDMetrics — **Column Shapes** (per-column marginal fidelity) and
**Column Pair Trends** (correlation similarity for numerical pairs,
contingency similarity for categorical pairs) — with the overall score
the mean of the two.

`diagnostic_report()` complements it with structural-validity checks
(value ranges, category adherence, key uniqueness), and
`sample_conditions()` generates rows that hold given categorical values
fixed:

``` r
# Validity checks
diagnostic_report(adult_income, synth_data, meta)

# Conditional generation
sample_conditions(syn, data.frame(income = ">50K", .n = 20))
```

## Related work

- Python SDV: [sdv-dev/SDV](https://github.com/sdv-dev/SDV)
- Synthetic Data Vault paper: Patki et al., IEEE DSAA 2016
- CTGAN: Xu et al., NeurIPS 2019 (implemented in companion package
  `rsdv.torch`)
