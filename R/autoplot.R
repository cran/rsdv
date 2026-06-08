utils::globalVariables(c("column", "score", "metric", "check"))

#' Plot a quality report
#'
#' Produces a bar chart of per-column similarity scores, with a horizontal
#' line at the overall score.
#'
#' @param object An `rsdv_quality_report` object.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @importFrom ggplot2 autoplot
#' @examples
#' \donttest{
#' syn <- gaussian_copula_synthesizer(metadata(adult_income)) |> fit(adult_income)
#' synth <- sample(syn, n = 500)
#' qr <- quality_report(adult_income, synth, metadata(adult_income))
#' ggplot2::autoplot(qr)
#' }
#' @export
autoplot.rsdv_quality_report <- function(object, ...) {
  scores <- rbind(
    data.frame(column = object$ks_scores$column,
               score  = object$ks_scores$score,
               metric = "KS (numerical)",
               stringsAsFactors = FALSE),
    data.frame(column = object$tvd_scores$column,
               score  = object$tvd_scores$score,
               metric = "TVD (categorical)",
               stringsAsFactors = FALSE)
  )

  ggplot2::ggplot(scores, ggplot2::aes(x = column, y = score, fill = metric)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_hline(yintercept = object$overall_score,
                        linetype = "dashed", colour = "grey40") +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::labs(
      title    = "rsdv Quality Report",
      subtitle = sprintf("Overall score: %.3f", object$overall_score),
      x        = "Column",
      y        = "Similarity score",
      fill     = "Metric"
    ) +
    ggplot2::theme_minimal()
}

#' Plot a privacy report
#'
#' Plots the NNDR score as a gauge-style bar.
#'
#' @param object An `rsdv_privacy_report` object.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @importFrom ggplot2 autoplot
#' @examples
#' \donttest{
#' syn <- gaussian_copula_synthesizer(metadata(adult_income)) |> fit(adult_income)
#' synth <- sample(syn, n = 500)
#' pr <- privacy_report(adult_income, synth)
#' ggplot2::autoplot(pr)
#' }
#' @export
autoplot.rsdv_privacy_report <- function(object, ...) {
  df <- data.frame(
    metric = "NNDR Privacy Score",
    score  = object$nndr_score
  )

  ggplot2::ggplot(df, ggplot2::aes(x = metric, y = score)) +
    ggplot2::geom_col(fill = "#3182bd", width = 0.4) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::labs(
      title    = "rsdv Privacy Report",
      subtitle = "NNDR score: higher = more private (lower re-identification risk)",
      x        = NULL,
      y        = "Score"
    ) +
    ggplot2::theme_minimal()
}
