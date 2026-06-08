#' Adult Income dataset (500-row sample)
#'
#' A 500-row random sample of the UCI Adult Income dataset, used in
#' package examples and vignettes.
#'
#' @format A tibble with 500 rows and 16 variables:
#' \describe{
#'   \item{id}{Row identifier (integer)}
#'   \item{age}{Age in years (integer)}
#'   \item{workclass}{Employment type (character)}
#'   \item{fnlwgt}{Final weight, a census sampling weight (integer)}
#'   \item{education}{Highest level of education (character)}
#'   \item{education_num}{Education encoded as an integer (integer)}
#'   \item{marital_status}{Marital status (character)}
#'   \item{occupation}{Occupation category (character)}
#'   \item{relationship}{Relationship to householder (character)}
#'   \item{race}{Race (character)}
#'   \item{sex}{Sex (character)}
#'   \item{capital_gain}{Capital gains (integer)}
#'   \item{capital_loss}{Capital losses (integer)}
#'   \item{hours_per_week}{Hours worked per week (integer)}
#'   \item{native_country}{Country of origin (character)}
#'   \item{income}{Income bracket: `<=50K` or `>50K` (character)}
#' }
#' @source <https://archive.ics.uci.edu/dataset/2/adult>
"adult_income"
