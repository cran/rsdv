#' Create a Gaussian Copula synthesizer
#'
#' Fits a single Gaussian copula over **all** modeled columns. Numerical
#' columns use a fitted parametric marginal (see `default_distribution`);
#' categorical and boolean columns are embedded into the copula via their
#' cumulative-frequency intervals, so cross-column dependence (numeric vs.
#' categorical, categorical vs. categorical) is preserved.
#'
#' @param metadata An `rsdv_metadata` object.
#' @param enforce_min_max Logical. Clamp sampled numerical values to the
#'   observed range. Default `TRUE`.
#' @param numerical_distributions Optional named character vector/list mapping
#'   numerical column names to a distribution in `"norm"`, `"beta"`, `"gamma"`,
#'   `"truncnorm"`, `"uniform"`, or `"auto"`.
#' @param default_distribution Distribution used for numerical columns not named
#'   in `numerical_distributions`. `"auto"` (default) selects the best-fitting
#'   family per column by Kolmogorov-Smirnov distance.
#' @return An unfitted `gaussian_copula_synthesizer` object.
#' @export
#' @examples
#' \donttest{
#' meta <- metadata(adult_income) |>
#'   set_column_type("age", "numerical") |>
#'   set_column_type("occupation", "categorical")
#' syn <- gaussian_copula_synthesizer(meta, default_distribution = "auto")
#' syn <- fit(syn, adult_income)
#' }
gaussian_copula_synthesizer <- function(metadata, enforce_min_max = TRUE,
                                        numerical_distributions = list(),
                                        default_distribution = "auto") {
  valid <- c(NUMERIC_DISTRIBUTIONS, "auto")
  bad   <- setdiff(c(unlist(numerical_distributions), default_distribution), valid)
  if (length(bad) > 0L)
    stop(sprintf("Invalid distribution(s): %s. Must be one of: %s",
                 paste(sprintf("'%s'", bad), collapse = ", "),
                 paste(valid, collapse = ", ")))

  structure(
    list(
      metadata                = metadata,
      enforce_min_max         = enforce_min_max,
      numerical_distributions = numerical_distributions,
      default_distribution    = default_distribution,
      fitted                  = FALSE,
      copula                  = NULL,
      cor_matrix              = NULL,
      transformers            = NULL,
      num_cols                = NULL,
      cat_cols                = NULL,
      bool_cols               = NULL,
      modeled_cols            = NULL
    ),
    class = c("gaussian_copula_synthesizer", "rsdv_synthesizer")
  )
}

#' @importFrom generics fit
#' @export
fit.gaussian_copula_synthesizer <- function(object, data, ...) {
  validate_data(data, object$metadata)

  meta      <- object$metadata
  num_cols  <- get_columns_by_type(meta, "numerical")
  cat_cols  <- get_columns_by_type(meta, "categorical")
  bool_cols <- get_columns_by_type(meta, "boolean")

  object$num_cols  <- num_cols
  object$cat_cols  <- cat_cols
  object$bool_cols <- bool_cols

  object$transformers <- fit_transformers(
    data, meta,
    numerical_distributions = object$numerical_distributions,
    default_distribution    = object$default_distribution
  )

  unsupported <- names(Filter(is.null, object$transformers))
  if (length(unsupported) > 0L)
    warning(sprintf(
      "Column(s) %s have unsupported type(s) (e.g. 'datetime', 'id') and will be excluded from synthetic output.",
      paste(sprintf("'%s'", unsupported), collapse = ", ")
    ))

  # Every column with a transformer is modeled jointly by the copula.
  modeled_cols <- c(num_cols, cat_cols, bool_cols)
  object$modeled_cols <- modeled_cols

  if (length(modeled_cols) >= 2L) {
    # Fit on rows that are complete across all modeled columns.
    complete <- stats::complete.cases(data[, modeled_cols, drop = FALSE])
    u_mat <- do.call(cbind, lapply(modeled_cols, function(col) {
      tr <- object$transformers[[col]]
      switch(tr$type,
        numerical   = apply_numerical_transformer(data[[col]][complete], tr),
        categorical = encode_categorical_u(data[[col]][complete], tr),
        boolean     = encode_boolean_u(data[[col]][complete], tr)
      )
    }))
    colnames(u_mat) <- modeled_cols

    # Rank-based pseudo-observations for robust, marginal-free copula estimation.
    u_pobs <- copula::pobs(u_mat)
    nc     <- copula::normalCopula(dim = ncol(u_pobs), dispstr = "un")
    # "itau" (inversion of Kendall's tau) is robust for small samples and
    # avoids non-finite gradients that plague ML with few rows or tied values.
    fit_result <- copula::fitCopula(nc, data = u_pobs, method = "itau")

    object$copula     <- fit_result@copula
    object$cor_matrix <- copula::getSigma(fit_result@copula)
    colnames(object$cor_matrix) <- modeled_cols
    rownames(object$cor_matrix) <- modeled_cols

  } else if (length(modeled_cols) == 1L) {
    object$copula     <- NULL
    object$cor_matrix <- matrix(1, 1, 1,
                                dimnames = list(modeled_cols, modeled_cols))
  }

  object$fitted <- TRUE
  object
}

#' @export
sample.gaussian_copula_synthesizer <- function(x, n = 100, max_tries = 100L, ...) {
  if (!is_fitted(x)) stop("Synthesizer must be fitted before calling sample().")

  meta      <- x$metadata
  collected <- vector("list", max_tries)
  remaining <- n
  tries     <- 0L

  while (remaining > 0L && tries < max_tries) {
    tries  <- tries + 1L
    batch  <- .sample_raw(x, remaining)
    valid  <- check_constraints(batch, meta)
    good   <- batch[valid, , drop = FALSE]
    if (nrow(good) > 0L) {
      collected[[tries]] <- good
      remaining <- remaining - nrow(good)
    }
  }

  if (remaining > 0L) {
    warning(sprintf(
      "Could not satisfy all constraints after %d tries. Returning %d/%d rows.",
      max_tries, n - remaining, n
    ))
  }

  valid_batches <- Filter(Negate(is.null), collected)
  if (length(valid_batches) == 0L) {
    # All tries exhausted with zero valid rows — return a 0-row data frame
    col_names <- names(x$metadata$columns)
    empty <- vector("list", length(col_names))
    names(empty) <- col_names
    return(as.data.frame(empty, stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, valid_batches)
  out[seq_len(min(n, nrow(out))), , drop = FALSE]
}

#' Sample synthetic rows that match fixed column values (conditional sampling)
#'
#' Generates rows in which one or more **categorical or boolean** columns are
#' held to specified values, via rejection sampling against the fitted copula.
#' This preserves the modeled dependence between the conditioned columns and the
#' rest of the table (unlike overwriting values after the fact).
#'
#' @param x A fitted `gaussian_copula_synthesizer`.
#' @param conditions A data frame whose columns are the variables to fix. Each
#'   row is one condition; an optional integer column `.n` gives how many rows
#'   to generate for that condition (default 1 per row).
#' @param max_tries Maximum rejection-sampling rounds per condition.
#' @return A data frame of synthetic rows satisfying the conditions.
#' @export
#' @examples
#' \donttest{
#' meta <- metadata(adult_income)
#' syn  <- gaussian_copula_synthesizer(meta) |> fit(adult_income)
#' sample_conditions(syn, data.frame(income = ">50K", .n = 20))
#' }
sample_conditions <- function(x, conditions, max_tries = 100L) {
  if (!is_fitted(x)) stop("Synthesizer must be fitted before calling sample_conditions().")
  if (!is.data.frame(conditions) || nrow(conditions) == 0L)
    stop("`conditions` must be a data frame with at least one row.")

  counts    <- if (".n" %in% names(conditions)) conditions[[".n"]] else rep(1L, nrow(conditions))
  cond_cols <- setdiff(names(conditions), ".n")

  # Only categorical/boolean equality conditions can be matched exactly.
  num_cond <- intersect(cond_cols, x$num_cols)
  if (length(num_cond) > 0L)
    stop(sprintf(
      "Conditioning on numerical column(s) %s is not supported; condition on categorical/boolean columns only.",
      paste(sprintf("'%s'", num_cond), collapse = ", ")
    ))
  unknown <- setdiff(cond_cols, x$modeled_cols)
  if (length(unknown) > 0L)
    stop(sprintf("Unknown condition column(s): %s",
                 paste(sprintf("'%s'", unknown), collapse = ", ")))

  out_parts <- vector("list", nrow(conditions))
  for (i in seq_len(nrow(conditions))) {
    need      <- as.integer(counts[i])
    target    <- conditions[i, cond_cols, drop = FALSE]
    collected <- vector("list", max_tries)
    remaining <- need
    tries     <- 0L
    while (remaining > 0L && tries < max_tries) {
      tries <- tries + 1L
      # Oversample to offset the rejection rate for rare conditions.
      batch <- .sample_raw(x, max(remaining * 4L, 50L))
      match <- rep(TRUE, nrow(batch))
      for (col in cond_cols)
        match <- match & as.character(batch[[col]]) == as.character(target[[col]])
      good <- batch[match, , drop = FALSE]
      if (nrow(good) > 0L) {
        collected[[tries]] <- good
        remaining <- remaining - nrow(good)
      }
    }
    rows <- Filter(Negate(is.null), collected)
    if (remaining > 0L)
      warning(sprintf(
        "Condition %d: could only generate %d/%d rows after %d tries.",
        i, need - remaining, need, max_tries
      ))
    if (length(rows) > 0L) {
      part <- do.call(rbind, rows)
      out_parts[[i]] <- part[seq_len(min(need, nrow(part))), , drop = FALSE]
    }
  }

  out <- Filter(Negate(is.null), out_parts)
  if (length(out) == 0L) {
    col_names <- names(x$metadata$columns)
    empty <- vector("list", length(col_names)); names(empty) <- col_names
    return(as.data.frame(empty, stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}

# Internal: generate n rows without constraint checking
.sample_raw <- function(x, n) {
  modeled_cols <- x$modeled_cols
  all_cols     <- names(x$metadata$columns)

  result <- vector("list", length(all_cols))
  names(result) <- all_cols

  # Draw correlated uniforms from the copula (or independent uniforms when only
  # a single column is modeled and no copula was fit).
  if (length(modeled_cols) >= 2L) {
    u_samples <- copula::rCopula(n, x$copula)
    colnames(u_samples) <- modeled_cols
  } else if (length(modeled_cols) == 1L) {
    u_samples <- matrix(stats::runif(n), ncol = 1L,
                        dimnames = list(NULL, modeled_cols))
  } else {
    u_samples <- matrix(numeric(0), nrow = n, ncol = 0L)
  }

  for (col in modeled_cols) {
    tr <- x$transformers[[col]]
    u  <- u_samples[, col]
    vals <- switch(tr$type,
      numerical = {
        v <- invert_numerical_transformer(u, tr)
        if (x$enforce_min_max) v <- pmin(pmax(v, tr$min), tr$max)
        v
      },
      categorical = decode_categorical_u(u, tr),
      boolean     = decode_boolean_u(u, tr)
    )
    result[[col]] <- .apply_miss(vals, tr$miss_rate)
  }

  # Drop columns with no transformer (unsupported types: datetime, id)
  result <- Filter(Negate(is.null), result)

  as.data.frame(result, stringsAsFactors = FALSE)
}

# Randomly set approximately miss_rate fraction of vals to NA.
.apply_miss <- function(vals, miss_rate) {
  if (is.null(miss_rate) || miss_rate <= 0) return(vals)
  vals[stats::runif(length(vals)) < miss_rate] <- NA
  vals
}
