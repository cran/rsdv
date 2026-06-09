# Internal package constant: the closed set of column-type strings accepted by
# set_column_type(). The synthesizer only models "numerical", "categorical",
# and "boolean"; "datetime" and "id" are recognised by the metadata system but
# excluded from synthesis (warned about in fit.gaussian_copula_synthesizer).
VALID_COLUMN_TYPES <- c("numerical", "categorical", "boolean", "datetime", "id")

#' Get column names matching a given type
#' @noRd
get_columns_by_type <- function(meta, type) {
  names(Filter(function(col) col$type == type, meta$columns))
}
