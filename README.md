
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
#>   id                   0.958
#>   age                  0.948
#>   fnlwgt               0.950
#>   education_num        0.780
#>   capital_gain         0.468
#>   capital_loss         0.470
#>   hours_per_week       0.738
#> 
#> Column Similarity (TVD, categorical):
#>   workclass            0.961
#>   education            0.944
#>   marital_status       0.952
#>   occupation           0.951
#>   relationship         0.978
#>   race                 0.990
#>   sex                  0.992
#>   native_country       0.976
#>   income               0.980
#> 
#> Property scores:
#>   Column Shapes        0.877
#>   Column Pair Trends   0.903
#>     (correlation 0.967, contingency 0.865)
#> 
#> Overall Score:               0.890
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
