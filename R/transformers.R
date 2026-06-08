# All functions in this file are internal (@noRd). They are not exported.
# They are used by gaussian_copula.R for pre-processing and post-processing data.

# Supported parametric families for numerical marginals (mirrors SDV's
# GaussianCopulaSynthesizer). "auto" selects the best fit by KS distance.
NUMERIC_DISTRIBUTIONS <- c("norm", "beta", "gamma", "truncnorm", "uniform")

# Truncated-normal helpers (base R has no native truncnorm) -------------------

#' @noRd
.ptruncnorm <- function(x, mu, sigma, a, b) {
  za <- stats::pnorm((a - mu) / sigma)
  zb <- stats::pnorm((b - mu) / sigma)
  p  <- (stats::pnorm((x - mu) / sigma) - za) / (zb - za)
  pmin(pmax(p, 0), 1)
}

#' @noRd
.qtruncnorm <- function(p, mu, sigma, a, b) {
  za <- stats::pnorm((a - mu) / sigma)
  zb <- stats::pnorm((b - mu) / sigma)
  mu + sigma * stats::qnorm(za + p * (zb - za))
}

# Per-family fitters. Each returns list(dist, params) or NULL if infeasible. --

#' @noRd
.fit_norm <- function(x) {
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) return(NULL)
  list(dist = "norm", params = list(mean = mean(x), sd = s))
}

#' @noRd
.fit_uniform <- function(x) {
  list(dist = "uniform", params = list(min = min(x), max = max(x)))
}

#' @noRd
.fit_gamma <- function(x) {
  if (any(x <= 0)) return(NULL)            # gamma support is (0, Inf)
  m <- mean(x); v <- stats::var(x)
  if (!is.finite(v) || v <= 0) return(NULL)
  shape <- m^2 / v; rate <- m / v          # method of moments
  if (!is.finite(shape) || !is.finite(rate) || shape <= 0 || rate <= 0) return(NULL)
  list(dist = "gamma", params = list(shape = shape, rate = rate))
}

#' @noRd
.fit_beta <- function(x) {
  mn <- min(x); mx <- max(x); rng <- mx - mn
  if (rng <= 0) return(NULL)
  s <- pmin(pmax((x - mn) / rng, 1e-6), 1 - 1e-6)  # rescale to (0,1)
  m <- mean(s); v <- stats::var(s)
  if (!is.finite(v) || v <= 0) return(NULL)
  common <- m * (1 - m) / v - 1            # method of moments
  shape1 <- m * common; shape2 <- (1 - m) * common
  if (!is.finite(shape1) || !is.finite(shape2) || shape1 <= 0 || shape2 <= 0)
    return(NULL)
  list(dist = "beta", params = list(shape1 = shape1, shape2 = shape2,
                                    min = mn, max = mx))
}

#' @noRd
.fit_truncnorm <- function(x) {
  a <- min(x); b <- max(x)
  if (a == b) return(NULL)
  negll <- function(par) {
    mu <- par[1]; sigma <- exp(par[2])
    denom <- stats::pnorm((b - mu) / sigma) - stats::pnorm((a - mu) / sigma)
    if (!is.finite(denom) || denom <= 0) return(1e10)
    -sum(stats::dnorm((x - mu) / sigma, log = TRUE) - log(sigma) - log(denom))
  }
  start <- c(mean(x), log(max(stats::sd(x), 1e-3)))
  opt <- tryCatch(
    suppressWarnings(stats::optim(start, negll, method = "BFGS")),
    error = function(e) NULL
  )
  if (is.null(opt) || !is.finite(opt$value)) return(NULL)
  list(dist = "truncnorm",
       params = list(mu = opt$par[1], sigma = exp(opt$par[2]), a = a, b = b))
}

# CDF / quantile dispatch over a fitted numerical transformer -----------------

#' @noRd
.cdf_numeric <- function(x, tr) {
  p <- tr$params
  switch(tr$dist,
    norm      = stats::pnorm(x, p$mean, p$sd),
    uniform   = stats::punif(x, p$min, p$max),
    gamma     = stats::pgamma(x, shape = p$shape, rate = p$rate),
    truncnorm = .ptruncnorm(x, p$mu, p$sigma, p$a, p$b),
    beta      = stats::pbeta((x - p$min) / (p$max - p$min), p$shape1, p$shape2),
    constant  = rep(0.5, length(x))
  )
}

#' @noRd
.quantile_numeric <- function(u, tr) {
  p <- tr$params
  switch(tr$dist,
    norm      = stats::qnorm(u, p$mean, p$sd),
    uniform   = stats::qunif(u, p$min, p$max),
    gamma     = stats::qgamma(u, shape = p$shape, rate = p$rate),
    truncnorm = .qtruncnorm(u, p$mu, p$sigma, p$a, p$b),
    beta      = stats::qbeta(u, p$shape1, p$shape2) * (p$max - p$min) + p$min,
    constant  = rep(p$value, length(u))
  )
}

# One-sample KS distance between data and a fitted family (for selection).
#' @noRd
.ks_distance <- function(x, tr) {
  xs <- sort(x); n <- length(xs)
  fhat <- .cdf_numeric(xs, tr)
  max(pmax(abs(fhat - (1:n) / n), abs(fhat - (0:(n - 1)) / n)))
}

# NumericalTransformer --------------------------------------------------------
# Fits a univariate parametric distribution per column. apply() maps data to
# (0,1) via the fitted CDF (probability integral transform); invert() maps
# (0,1) back via the quantile function. Round-trips because quantile is the
# exact inverse of the CDF, regardless of how well the family fits.

#' @noRd
fit_numerical_transformer <- function(x, distribution = "auto") {
  x_clean   <- x[is.finite(x)]
  miss_rate <- mean(is.na(x))
  mn <- min(x_clean); mx <- max(x_clean)

  # Degenerate (constant) column: no distribution to fit.
  if (mn == mx) {
    return(list(type = "numerical", dist = "constant",
                params = list(value = mn), min = mn, max = mx,
                miss_rate = miss_rate))
  }

  families <- if (identical(distribution, "auto")) NUMERIC_DISTRIBUTIONS else distribution
  fitters  <- list(norm = .fit_norm, beta = .fit_beta, gamma = .fit_gamma,
                   truncnorm = .fit_truncnorm, uniform = .fit_uniform)
  fits <- Filter(Negate(is.null), lapply(families, function(fam) fitters[[fam]](x_clean)))
  names(fits) <- vapply(fits, `[[`, character(1L), "dist")

  if (length(fits) == 0L) {
    if (!identical(distribution, "auto"))
      warning(sprintf(
        "Distribution '%s' could not be fit to the data; falling back to 'uniform'.",
        distribution
      ))
    fits <- list(uniform = .fit_uniform(x_clean))
  }

  # Select the family with the smallest KS distance to the empirical CDF.
  ks     <- vapply(fits, function(f) .ks_distance(x_clean, f), numeric(1L))
  chosen <- fits[[which.min(ks)]]

  list(type = "numerical", dist = chosen$dist, params = chosen$params,
       min = mn, max = mx, miss_rate = miss_rate)
}

#' @noRd
apply_numerical_transformer <- function(x, tr) {
  eps <- 1e-6
  pmin(pmax(.cdf_numeric(x, tr), eps), 1 - eps)
}

#' @noRd
invert_numerical_transformer <- function(u, tr) {
  eps <- 1e-6
  out <- .quantile_numeric(u, tr)
  # Snap the clamped boundary probabilities back to the observed extremes,
  # keeping the apply()/invert() round-trip exact at the endpoints.
  out[u <= eps]     <- tr$min
  out[u >= 1 - eps] <- tr$max
  out
}

# CategoricalTransformer ------------------------------------------------------
# Stores observed levels, empirical probabilities, and the cumulative-frequency
# breakpoints used to embed categories into (0,1) for the copula.

#' @noRd
fit_categorical_transformer <- function(x) {
  is_factor  <- is.factor(x)
  is_ordered <- is.ordered(x)
  x_char     <- as.character(x)
  # Preserve factor level order; for plain characters, sort for determinism
  levels_vec <- if (is_factor) levels(x) else sort(unique(x_char[!is.na(x_char)]))
  freq       <- tabulate(match(x_char, levels_vec), nbins = length(levels_vec))
  prob       <- freq / sum(freq)
  breaks     <- c(0, cumsum(prob)); breaks[length(breaks)] <- 1  # guard rounding
  list(type = "categorical", levels = levels_vec, prob = prob, breaks = breaks,
       is_factor = is_factor, is_ordered = is_ordered,
       miss_rate = mean(is.na(x)))
}

#' @noRd
apply_categorical_transformer <- function(x, tr) {
  as.integer(match(as.character(x), tr$levels))
}

#' @noRd
invert_categorical_transformer <- function(codes, tr) {
  tr$levels[codes]
}

#' Sample n values from a categorical transformer's empirical distribution
#' @noRd
sample_categorical <- function(n, tr) {
  vals <- tr$levels[sample.int(length(tr$levels), n, replace = TRUE, prob = tr$prob)]
  if (isTRUE(tr$is_factor)) factor(vals, levels = tr$levels, ordered = isTRUE(tr$is_ordered))
  else vals
}

# Embed a categorical column into (0,1): each value is placed uniformly at
# random within its category's cumulative-frequency interval. This continuous
# representation lets the copula capture categorical<->other-column dependence.
#' @noRd
encode_categorical_u <- function(x, tr) {
  idx   <- match(as.character(x), tr$levels)
  lower <- tr$breaks[idx]
  width <- tr$prob[idx]
  lower + stats::runif(length(idx)) * width
}

# Map a (0,1) draw back to a category by locating its frequency interval.
#' @noRd
decode_categorical_u <- function(u, tr) {
  idx  <- findInterval(u, tr$breaks, rightmost.closed = TRUE, all.inside = TRUE)
  idx  <- pmin(pmax(idx, 1L), length(tr$levels))
  vals <- tr$levels[idx]
  if (isTRUE(tr$is_factor))
    factor(vals, levels = tr$levels, ordered = isTRUE(tr$is_ordered))
  else vals
}

# BooleanTransformer ----------------------------------------------------------
# Modeled as a 2-level categorical (FALSE, TRUE) so it, too, enters the copula.

#' @noRd
fit_boolean_transformer <- function(x) {
  p <- mean(as.logical(x), na.rm = TRUE)
  list(type = "boolean", prob_true = p, prob = c(1 - p, p),
       breaks = c(0, 1 - p, 1), miss_rate = mean(is.na(x)))
}

#' @noRd
apply_boolean_transformer <- function(x, tr) as.integer(as.logical(x))

#' @noRd
invert_boolean_transformer <- function(codes, tr) as.logical(codes)

#' @noRd
encode_boolean_u <- function(x, tr) {
  idx   <- ifelse(as.logical(x), 2L, 1L)
  lower <- tr$breaks[idx]
  width <- tr$prob[idx]
  lower + stats::runif(length(idx)) * width
}

#' @noRd
decode_boolean_u <- function(u, tr) {
  u >= tr$breaks[2]  # FALSE occupies [0, 1-p); TRUE occupies [1-p, 1]
}

# Dispatch helpers ------------------------------------------------------------

#' Fit one transformer per column according to metadata column types.
#'
#' `numerical_distributions` is a named character vector/list mapping column
#' names to a family in `NUMERIC_DISTRIBUTIONS`; `default_distribution` applies
#' to numerical columns not named there.
#' @noRd
fit_transformers <- function(data, meta, numerical_distributions = list(),
                             default_distribution = "auto") {
  cols <- names(meta$columns)
  trs  <- lapply(cols, function(col) {
    type <- meta$columns[[col]]$type
    x    <- data[[col]]
    switch(type,
      numerical   = {
        dist <- if (!is.null(numerical_distributions[[col]]))
          numerical_distributions[[col]] else default_distribution
        fit_numerical_transformer(x, dist)
      },
      categorical = fit_categorical_transformer(x),
      boolean     = fit_boolean_transformer(x),
      NULL
    )
  })
  stats::setNames(trs, cols)
}

#' Transform columns to intermediate representation
#' Numerical -> (0,1), categorical -> integer codes, boolean -> 0/1
#' @noRd
apply_transformers <- function(data, transformers, meta) {
  result <- data
  for (col in names(meta$columns)) {
    tr   <- transformers[[col]]
    if (is.null(tr)) next
    type <- meta$columns[[col]]$type
    result[[col]] <- switch(type,
      numerical   = apply_numerical_transformer(data[[col]], tr),
      categorical = apply_categorical_transformer(data[[col]], tr),
      boolean     = apply_boolean_transformer(data[[col]], tr),
      data[[col]]
    )
  }
  result
}

#' Inverse-transform columns back to original scale / levels
#' @noRd
invert_transformers <- function(data, transformers, meta) {
  result <- data
  for (col in names(meta$columns)) {
    tr   <- transformers[[col]]
    if (is.null(tr)) next
    type <- meta$columns[[col]]$type
    result[[col]] <- switch(type,
      numerical   = invert_numerical_transformer(data[[col]], tr),
      categorical = invert_categorical_transformer(data[[col]], tr),
      boolean     = invert_boolean_transformer(data[[col]], tr),
      data[[col]]
    )
  }
  result
}
