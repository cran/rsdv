#' rsdv: Synthetic Tabular Data Generation with Gaussian Copulas
#'
#' rsdv generates synthetic tabular data from real datasets using Gaussian
#' copula models, with parametric marginal selection for numerical columns and
#' a cumulative-frequency embedding that brings categorical and boolean columns
#' into the same joint copula. It includes a metadata system, declarative
#' constraints, conditional sampling, and quality, validity, and privacy
#' reports modeled on those of the Python `SDMetrics` library.
#'
#' The main entry points are:
#' * [metadata()] — describe column types and primary keys.
#' * [gaussian_copula_synthesizer()] + [fit()] + [sample()] — fit a synthesizer
#'   and generate rows.
#' * [sample_conditions()] — generate rows that hold given categorical or
#'   boolean values fixed.
#' * [quality_report()], [diagnostic_report()], [privacy_report()] — evaluate
#'   the synthetic data.
#'
#' @keywords internal
"_PACKAGE"
