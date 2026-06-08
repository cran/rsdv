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
