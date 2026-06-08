#' Serialize metadata to a JSON string
#'
#' @param meta An `rsdv_metadata` object.
#' @return A JSON character string.
#' @export
#' @examples
#' meta <- metadata() |> set_column_type("age", "numerical")
#' json <- metadata_to_json(meta)
#' meta2 <- metadata_from_json(json)
metadata_to_json <- function(meta) {
  jsonlite::toJSON(
    list(
      columns     = meta$columns,
      primary_key = meta$primary_key,
      constraints = meta$constraints
    ),
    auto_unbox = TRUE,
    pretty     = TRUE,
    null       = "null"
  )
}

#' Deserialize metadata from a JSON string
#'
#' @param json A JSON character string produced by [metadata_to_json()].
#' @return An `rsdv_metadata` object.
#' @examples
#' json <- metadata_to_json(metadata() |> set_column_type("age", "numerical"))
#' metadata_from_json(json)
#' @export
metadata_from_json <- function(json) {
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  structure(
    list(
      columns     = parsed$columns %||% list(),
      primary_key = parsed$primary_key,
      constraints = parsed$constraints %||% list()
    ),
    class = "rsdv_metadata"
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
