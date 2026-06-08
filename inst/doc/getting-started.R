## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 7,
  fig.height = 4,
  fig.align = "center"
)

## ----quick-start, message = FALSE, warning = FALSE----------------------------
library(rsdv)

set.seed(42)

meta  <- metadata(adult_income) |>
  set_column_type("age",            "numerical")  |>
  set_column_type("education_num",  "numerical")  |>
  set_column_type("hours_per_week", "numerical")  |>
  set_column_type("occupation",     "categorical") |>
  set_column_type("income",         "categorical")

syn   <- gaussian_copula_synthesizer(meta) |> fit(adult_income)
synth <- sample(syn, n = 500)

head(synth[, c("age", "education_num", "occupation", "income")])

## ----metadata, warning = FALSE------------------------------------------------
meta <- metadata(adult_income) |>
  set_column_type("age",            "numerical")  |>
  set_column_type("education_num",  "numerical")  |>
  set_column_type("hours_per_week", "numerical")  |>
  set_column_type("occupation",     "categorical") |>
  set_column_type("marital_status", "categorical") |>
  set_column_type("income",         "categorical")

print(meta)

## ----fit-sample, message = FALSE, warning = FALSE-----------------------------
set.seed(42)

syn   <- gaussian_copula_synthesizer(meta)
syn   <- fit(syn, adult_income)
synth <- sample(syn, n = 500)

## ----distributions, message = FALSE, warning = FALSE--------------------------
syn_dist <- gaussian_copula_synthesizer(
  meta,
  numerical_distributions = list(capital_gain = "gamma"),
  default_distribution    = "norm"
) |>
  fit(adult_income)

## ----conditional, warning = FALSE---------------------------------------------
high_earners <- sample_conditions(
  syn,
  data.frame(income = ">50K", .n = 50, stringsAsFactors = FALSE)
)
table(high_earners$income)

## ----quality, warning = FALSE-------------------------------------------------
qr <- quality_report(adult_income, synth, meta)
print(qr)

## ----plot-column-similarity, fig.height = 5, warning = FALSE------------------
col_scores <- rbind(
  data.frame(
    column = qr$ks_scores$column,
    score  = qr$ks_scores$score,
    type   = "KS similarity\n(numerical)",
    stringsAsFactors = FALSE
  ),
  data.frame(
    column = qr$tvd_scores$column,
    score  = qr$tvd_scores$score,
    type   = "TVD similarity\n(categorical)",
    stringsAsFactors = FALSE
  )
)

ggplot2::ggplot(
  col_scores,
  ggplot2::aes(x = reorder(column, score), y = score, fill = type)
) +
  ggplot2::geom_col(width = 0.65, alpha = 0.9) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", score)),
    hjust = -0.15, size = 3.2, colour = "grey30"
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    limits = c(0, 1.15),
    labels = scales::percent_format(accuracy = 1)
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "KS similarity\n(numerical)"    = "#2171b5",
      "TVD similarity\n(categorical)" = "#238b45"
    )
  ) +
  ggplot2::labs(
    title    = "Column Similarity: Real vs. Synthetic",
    subtitle = sprintf("Overall quality score: %.3f", qr$overall_score),
    x        = NULL,
    y        = "Similarity score",
    fill     = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position    = "bottom",
    panel.grid.major.y = ggplot2::element_blank(),
    plot.title         = ggplot2::element_text(face = "bold"),
    plot.subtitle      = ggplot2::element_text(colour = "grey40")
  )

## ----plot-correlation, fig.height = 4, warning = FALSE------------------------
num_cols <- c("age", "education_num", "hours_per_week")

cor_real <- round(cor(adult_income[, num_cols], use = "complete.obs"), 3)
cor_syn  <- round(cor(synth[, num_cols],        use = "complete.obs"), 3)

mat_to_long <- function(mat, source) {
  nms <- colnames(mat)
  data.frame(
    var1   = rep(nms, each  = length(nms)),
    var2   = rep(nms, times = length(nms)),
    value  = as.vector(mat),
    source = source,
    stringsAsFactors = FALSE
  )
}

cor_long <- rbind(
  mat_to_long(cor_real, "Real data"),
  mat_to_long(cor_syn,  "Synthetic data")
)
cor_long$var1 <- factor(cor_long$var1, levels = rev(num_cols))
cor_long$var2 <- factor(cor_long$var2, levels = num_cols)

ggplot2::ggplot(cor_long, ggplot2::aes(var2, var1, fill = value)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", value)),
    size = 3.5, colour = "grey20"
  ) +
  ggplot2::facet_wrap(~source) +
  ggplot2::scale_fill_gradient2(
    low      = "#d73027",
    mid      = "white",
    high     = "#1a9850",
    midpoint = 0,
    limits   = c(-1, 1),
    name     = "Pearson r"
  ) +
  ggplot2::labs(
    title = "Correlation Matrix: Real vs. Synthetic",
    x = NULL, y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
    strip.text      = ggplot2::element_text(face = "bold"),
    legend.position = "right",
    plot.title      = ggplot2::element_text(face = "bold"),
    panel.grid      = ggplot2::element_blank()
  )

## ----plot-marginals-age, fig.height = 3.5, warning = FALSE--------------------
age_data <- rbind(
  data.frame(value = adult_income$age, source = "Real"),
  data.frame(value = synth$age,        source = "Synthetic")
)

ggplot2::ggplot(age_data, ggplot2::aes(x = value, fill = source, colour = source)) +
  ggplot2::geom_density(alpha = 0.35, linewidth = 0.7) +
  ggplot2::scale_fill_manual(values   = c("Real" = "#2171b5", "Synthetic" = "#ef6548")) +
  ggplot2::scale_colour_manual(values = c("Real" = "#2171b5", "Synthetic" = "#ef6548")) +
  ggplot2::labs(
    title  = "Age Distribution: Real vs. Synthetic",
    x      = "Age (years)", y = "Density",
    fill   = NULL, colour = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title      = ggplot2::element_text(face = "bold")
  )

## ----plot-marginals-income, fig.height = 3, warning = FALSE-------------------
income_real  <- as.data.frame(table(adult_income$income) / nrow(adult_income))
income_synth <- as.data.frame(table(synth$income)        / nrow(synth))
names(income_real)  <- c("category", "proportion")
names(income_synth) <- c("category", "proportion")
income_real$source  <- "Real"
income_synth$source <- "Synthetic"
income_data <- rbind(income_real, income_synth)

ggplot2::ggplot(
  income_data,
  ggplot2::aes(x = category, y = proportion, fill = source)
) +
  ggplot2::geom_col(position = "dodge", width = 0.55, alpha = 0.9) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_fill_manual(values = c("Real" = "#2171b5", "Synthetic" = "#ef6548")) +
  ggplot2::labs(
    title = "Income Category: Real vs. Synthetic",
    x = NULL, y = "Proportion", fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title      = ggplot2::element_text(face = "bold")
  )

## ----diagnostic, warning = FALSE----------------------------------------------
dr <- diagnostic_report(adult_income, synth, meta)
print(dr)

## ----privacy, warning = FALSE-------------------------------------------------
pr <- privacy_report(adult_income, synth)
print(pr)

## ----plot-privacy, fig.height = 3, warning = FALSE----------------------------
score_val <- pr$nndr_score

zones <- data.frame(
  xmin  = c(0,    0.25, 0.50, 0.75),
  xmax  = c(0.25, 0.50, 0.75, 1.00),
  ymid  = 0.5,
  label = c("High risk", "Moderate", "Good", "Excellent"),
  fill  = c("#d73027",   "#fee090",  "#a6d96a", "#1a9850"),
  stringsAsFactors = FALSE
)

ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = zones,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = fill),
    alpha = 0.35
  ) +
  ggplot2::geom_text(
    data = zones,
    ggplot2::aes(x = (xmin + xmax) / 2, y = 0.15, label = label),
    size = 3, colour = "grey30"
  ) +
  ggplot2::geom_segment(
    data = data.frame(x = score_val),
    ggplot2::aes(x = x, xend = x, y = 0, yend = 1),
    colour = "black", linewidth = 1.6
  ) +
  ggplot2::geom_label(
    data = data.frame(x = score_val),
    ggplot2::aes(x = x, y = 0.72,
                 label = sprintf("NNDR = %.3f", x)),
    hjust = -0.08, size = 3.8, fontface = "bold",
    fill = "white", label.size = 0
  ) +
  ggplot2::scale_fill_identity() +
  ggplot2::scale_x_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    title    = "Privacy Score: Nearest-Neighbour Distance Ratio (NNDR)",
    subtitle = "Higher score = lower re-identification risk",
    x = "NNDR score", y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.y      = ggplot2::element_blank(),
    axis.ticks.y     = ggplot2::element_blank(),
    panel.grid       = ggplot2::element_blank(),
    plot.title       = ggplot2::element_text(face = "bold"),
    plot.subtitle    = ggplot2::element_text(colour = "grey40")
  )

## ----attr-disclosure, warning = FALSE-----------------------------------------
adr <- attribute_disclosure_risk(
  real          = adult_income,
  synthetic     = synth,
  sensitive_col = "income",
  known_cols    = "age"
)
cat("Attribute disclosure risk (income given age):", round(adr, 3), "\n")

## ----constraints, warning = FALSE---------------------------------------------
meta_constrained <- meta |>
  add_constraint(
    inequality_constraint("education_num", "hours_per_week", type = "lt")
  )

syn_c   <- gaussian_copula_synthesizer(meta_constrained) |> fit(adult_income)
synth_c <- sample(syn_c, n = 500)

# Verify: education_num < hours_per_week in all rows — should return TRUE
all(synth_c$education_num < synth_c$hours_per_week)

