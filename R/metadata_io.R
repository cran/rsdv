#' Serialize metadata to a JSON string
#'
#' Round-trips column types, primary key, and the structural constraints
#' (`equality`, `inequality`, `fixed_combinations`). `custom_constraint` cannot
#' be serialized — it holds an R closure — and is dropped with a warning.
#'
#' @param meta An `rsdv_metadata` object.
#' @return A JSON character string. Inverse of [metadata_from_json()].
#' @export
#' @examples
#' meta <- metadata() |>
#'   set_column_type("a", "numerical") |>
#'   set_column_type("b", "numerical") |>
#'   add_constraint(inequality_constraint("a", "b", type = "lt"))
#' json <- metadata_to_json(meta)
#' meta2 <- metadata_from_json(json)
metadata_to_json <- function(meta) {
  cons <- Filter(Negate(is.null),
                 lapply(meta$constraints, .constraint_to_list))
  jsonlite::toJSON(
    list(
      columns     = meta$columns,
      primary_key = meta$primary_key,
      constraints = cons
    ),
    auto_unbox = TRUE,
    pretty     = TRUE,
    null       = "null"
  )
}

#' Deserialize metadata from a JSON string
#'
#' @param json A JSON character string produced by [metadata_to_json()].
#' @return An `rsdv_metadata` object. Constraints are reconstructed with their
#'   original S3 classes so [check_constraint()] dispatches correctly.
#' @examples
#' meta <- metadata() |>
#'   set_column_type("a", "numerical") |>
#'   set_column_type("b", "numerical") |>
#'   add_constraint(inequality_constraint("a", "b", type = "lt"))
#' metadata_from_json(metadata_to_json(meta))
#' @export
metadata_from_json <- function(json) {
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  cons   <- lapply(parsed$constraints %||% list(), .constraint_from_list)
  structure(
    list(
      columns     = parsed$columns %||% list(),
      primary_key = parsed$primary_key,
      constraints = cons
    ),
    class = "rsdv_metadata"
  )
}

# Convert a constraint object to a plain (JSON-friendly) list.
# Returns NULL for constraint types that cannot be serialized; the caller
# strips NULLs and metadata_to_json emits a warning for those.
#' @noRd
.constraint_to_list <- function(c) {
  switch(c$type,
    equality = list(type = "equality", col_a = c$col_a, col_b = c$col_b),
    inequality = list(type = "inequality", col_a = c$col_a, col_b = c$col_b,
                      direction = c$direction),
    fixed_combinations = list(
      type    = "fixed_combinations",
      columns = as.list(c$columns),
      # Column-major layout survives jsonlite's array-of-records ambiguity and
      # makes reconstruction by as.data.frame() unambiguous.
      allowed = lapply(as.list(c$allowed), as.list)
    ),
    custom = {
      warning("custom_constraint cannot be serialized to JSON (it holds an R function); dropping this constraint from the output.",
              call. = FALSE)
      NULL
    },
    stop(sprintf("Cannot serialize constraint of type '%s'.", c$type))
  )
}

# Reconstruct a constraint object from its JSON-parsed list form.
#' @noRd
.constraint_from_list <- function(L) {
  switch(L$type,
    equality   = equality_constraint(L$col_a, L$col_b),
    inequality = inequality_constraint(L$col_a, L$col_b, type = L$direction),
    fixed_combinations = {
      cols <- unlist(L$columns, use.names = FALSE)
      # `allowed` was emitted column-major as list(col1 = list(v1, v2, ...), ...).
      # Unlist each column to a vector, then bind into a data frame.
      allowed_cols <- lapply(L$allowed, function(col) unlist(col, use.names = FALSE))
      allowed_df   <- as.data.frame(allowed_cols, stringsAsFactors = FALSE)
      fixed_combinations_constraint(cols, allowed_df)
    },
    stop(sprintf("Unknown constraint type '%s' in metadata JSON.", L$type))
  )
}

#' Save metadata to a JSON file
#'
#' @param meta An `rsdv_metadata` object.
#' @param path File path to write to.
#' @return Invisibly returns `meta`.
#' @export
#' @examples
#' meta <- metadata() |> set_column_type("age", "numerical")
#' tmp <- tempfile(fileext = ".json")
#' save_metadata(meta, tmp)
#' meta2 <- load_metadata(tmp)
save_metadata <- function(meta, path) {
  writeLines(metadata_to_json(meta), con = path)
  invisible(meta)
}

#' Load metadata from a JSON file
#'
#' @param path Path to a JSON file produced by [save_metadata()].
#' @return An `rsdv_metadata` object.
#' @examples
#' meta <- metadata() |> set_column_type("age", "numerical")
#' tmp  <- tempfile(fileext = ".json")
#' save_metadata(meta, tmp)
#' load_metadata(tmp)
#' @export
load_metadata <- function(path) {
  metadata_from_json(readLines(path, warn = FALSE))
}

# NULL-coalescing operator (internal, not exported)
`%||%` <- function(x, y) if (is.null(x)) y else x
