VALID_COLUMN_TYPES <- c("numerical", "categorical", "boolean", "datetime", "id")

#' Get column names matching a given type
#' @noRd
get_columns_by_type <- function(meta, type) {
  names(Filter(function(col) col$type == type, meta$columns))
}
