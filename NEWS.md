# rsdv 0.2.0

A correctness and robustness release driven by a code review of 0.1.0 (see
[issue #12](https://github.com/kvenkita/rsdv/issues/12) for the full
catalogue). Two changes alter previously-returned numeric output and are
called out separately below.

## ⚠ Default-output changes (potentially breaking)

* `nndr()` now standardises (z-scores) each numerical column by the
  **real-data** mean and standard deviation before the nearest-neighbour
  distance is computed. Without this, a single large-scale column (e.g.
  `income` in dollars) dominated the Euclidean distance and the score moved
  with measurement units rather than with row similarity. Pass
  `normalize = FALSE` to recover the previous behaviour exactly.
* `correlation_similarity()` and `contingency_similarity()` now return
  `score = NA_real_` (rather than `1`) when there are fewer than two columns
  of the relevant type, and `diagnostic_report()` returns `NA_real_` per
  column when the synthetic column is entirely `NA`. Aggregated property
  scores in `quality_report()` / `diagnostic_report()` skip these NAs
  (`na.rm = TRUE`) so they no longer overstate fidelity with a synthetic
  "1" where there is no signal to measure.

## New features

* `equality_constraint()` gains a `tolerance` argument: with `tolerance > 0`
  on numeric columns, the check is `abs(a - b) <= tolerance` instead of
  exact `==`. Default `0` preserves prior behaviour.
* `custom_constraint()` gains a `vectorized` argument: when `TRUE`, the
  predicate is called **once** with the whole data frame instead of once
  per row. Substantially faster on large synthetic samples for vectorisable
  predicates.
* `ml_efficacy()` gains a `seed` argument for reproducible
  train/test splits. The caller's global RNG state is restored on exit, so
  callers using `set.seed()` elsewhere are unaffected.
* `nndr()` gains a `normalize` argument (default `TRUE`) — see the
  default-output note above.
* `print()` methods for `equality_constraint`, `inequality_constraint`,
  `fixed_combinations_constraint`, and `custom_constraint`.

## Robustness and correctness

* `metadata_to_json()` / `metadata_from_json()` now round-trip the
  structural constraint types (`equality`, `inequality`, and
  `fixed_combinations`). `custom_constraint` cannot be serialised — it
  holds an R closure — and is dropped with a warning. Previously
  `metadata_to_json()` crashed on **any** constraint, so `save_metadata()`
  was effectively broken for non-trivial metadata.
* `check_constraint.equality_constraint` and
  `check_constraint.inequality_constraint` now return `FALSE` (not `NA`)
  for rows containing `NA`. This prevents `NA` from propagating into the
  row selector used by `sample()`'s rejection loop, which previously
  inserted phantom NA-only rows.
* `sample_conditions()` now honours metadata constraints alongside the
  user-supplied conditions (previously it filtered only on the conditions).
* `tvd_similarity()` now strips `NA`s from both sides and divides by the
  non-NA count on each side; previously NA-padding inflated TVD.
* `ks_similarity()` now suppresses the `ks.test()` *"p-value will be
  approximate in the presence of ties"* warning, which it leaked to users
  on any tied integer column (very common in tables with integer ages,
  capital gains, etc.).
* `fixed_combinations_constraint` now uses a collision-free length-prefix
  key encoding (`"<nchar>:<value>"`), removing a theoretical separator
  collision in the previous paste-based comparison.

## Clearer errors and validations

* `fit.gaussian_copula_synthesizer()` errors clearly when a modeled
  column is entirely `NA` or when no row is complete across all modeled
  columns. Previously the user saw a cryptic
  `'dim' must be an integer (>= 2)` from inside `copula::normalCopula`.
* `ml_efficacy()` validates `target_col` (must be a column of `real`) and
  `test_fraction` (must be strictly between 0 and 1) up front.
* `attribute_disclosure_risk()` validates that `known_cols` are present
  and numeric (one-hot encode categorical knowns first); previously
  triggered a cryptic `FNN::knnx.index` error.
* `gaussian_copula_synthesizer()` cross-checks
  `numerical_distributions` names against the metadata's numerical
  columns; silently-ignored typos like `list(capitl_gain = "gamma")` now
  raise a clear error.
* `sample_conditions()` validates that `.n` values are positive whole
  numbers (was silently truncating or accepting negatives).
* `privacy_report()` errors when only one of `sensitive_col` /
  `known_cols` is supplied (previously silently dropped disclosure-risk
  computation).
* `set_primary_key()` emits an advisory warning when the column's
  metadata type is not `"id"`, since the column would otherwise be
  modeled as ordinary data and the diagnostic key-uniqueness check would
  typically fail.

## Documentation

* `set_column_type()` docstring documents the level-ordering rule for
  categorical columns — `factor` keeps `levels()` order, character is
  sorted **lexicographically** (`c("2", "10")` becomes levels
  `c("10", "2")`).

# rsdv 0.1.0

Initial CRAN release.

## Synthesizer

* Gaussian copula synthesizer (`gaussian_copula_synthesizer()`) that fits a
  single joint copula over **all** modeled columns: numerical, categorical,
  and boolean.
* Parametric marginal fitting for numerical columns with best-fit selection
  among `norm`, `beta`, `gamma`, `truncnorm`, and `uniform` by
  Kolmogorov-Smirnov distance. Per-column overrides via
  `numerical_distributions`; global default via `default_distribution`.
* Categorical and boolean columns are embedded into the copula via their
  cumulative-frequency intervals, preserving cross-column dependence
  (numeric↔categorical and categorical↔categorical).
* `sample()` for unconditional generation and `sample_conditions()` for
  conditional generation on categorical or boolean values via rejection
  sampling.
* Per-column missingness rates from training data are reproduced in
  synthetic output.

## Metadata and constraints

* Column-type metadata system (`metadata()`, `set_column_type()`,
  `set_primary_key()`) with auto-detection and JSON serialization
  (`metadata_to_json()`, `save_metadata()`).
* Declarative constraint system: equality, inequality, fixed-combinations,
  and custom row-level predicates (`add_constraint()`,
  `check_constraints()`), enforced via rejection sampling.

## Evaluation

* `quality_report()` aggregates metrics into the two-property hierarchy used
  by the Python `SDMetrics` library:
    * **Column Shapes** — per-column marginal fidelity (KS similarity for
      numerical, TVD similarity for categorical).
    * **Column Pair Trends** — pairwise dependence
      (`correlation_similarity()` for numerical pairs,
      `contingency_similarity()` for categorical pairs).
  ML efficacy (train-on-synthetic / test-on-real, TSTR/TRTR) is reported
  separately, not folded into the overall score.
* `diagnostic_report()` checks structural validity: boundary adherence
  (numerical ranges), category adherence (categorical values), and key
  uniqueness for primary keys.
* `privacy_report()` reports the nearest-neighbour distance ratio (NNDR)
  and, optionally, attribute disclosure risk.
* `autoplot()` methods for quality, diagnostic, and privacy reports.

## Data

* Bundled dataset `adult_income` — a 500-row sample of the UCI Adult
  Income dataset used in examples and vignettes.

## Vignettes

* "Getting Started with rsdv" — practitioner-oriented guide covering
  metadata, fitting, conditional sampling, quality and diagnostic reports,
  privacy evaluation, constraints, and missing-data handling.
* "Migrating from synthpop" — side-by-side comparison and feature table.
