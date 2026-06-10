library(knitr)
library(tidyverse)
library(scales)

tmp <- tempfile(fileext = ".R")
purl("final-report.qmd", output = tmp, quiet = TRUE)
suppressMessages({
  sink(tempfile())
  source(tmp, local = FALSE)
  sink()
})

plot_data <- candidate_data |>
  filter(
    !is.na(team_fit_score),
    !is.na(tanking_index),
    !is.na(top4_avg_rating),
    !is.na(pre_win_pct)
  )

fit_mean <- mean(plot_data$team_fit_score, na.rm = TRUE)
fit_sd <- sd(plot_data$team_fit_score, na.rm = TRUE)
pre_mean <- mean(plot_data$pre_win_pct, na.rm = TRUE)

coef_fit <- coef(model_tanking_index_fit)

predict_tanking_fit <- function(team_fit_score, top4_z) {
  fit_z <- (team_fit_score - fit_mean) / fit_sd

  coef_fit[["(Intercept)"]] +
    coef_fit[["scale(top4_avg_rating)"]] * top4_z +
    coef_fit[["scale(team_fit_score)"]] * fit_z +
    coef_fit[["scale(top4_avg_rating):scale(team_fit_score)"]] * top4_z * fit_z +
    coef_fit[["pre_win_pct"]] * pre_mean +
    coef_fit[["season_2012TRUE"]] * 0 +
    coef_fit[["season_2020TRUE"]] * 0 +
    coef_fit[["season_2021TRUE"]] * 0
}

prediction_grid <- expand_grid(
  team_fit_score = seq(
    quantile(plot_data$team_fit_score, 0.03, na.rm = TRUE),
    quantile(plot_data$team_fit_score, 0.97, na.rm = TRUE),
    length.out = 100
  ),
  draft_strength_group = c("Stronger top-four draft", "Weaker top-four draft")
) |>
  mutate(
    top4_z = if_else(draft_strength_group == "Stronger top-four draft", 1, -1),
    predicted = predict_tanking_fit(team_fit_score, top4_z)
  )

prediction_points <- prediction_grid |>
  group_by(draft_strength_group) |>
  summarise(
    low_fit = first(predicted),
    high_fit = last(predicted),
    change = high_fit - low_fit,
    .groups = "drop"
  )

strong_change <- prediction_points |>
  filter(draft_strength_group == "Stronger top-four draft") |>
  pull(change)

weak_change <- prediction_points |>
  filter(draft_strength_group == "Weaker top-four draft") |>
  pull(change)

insight_text <- paste0(
  "Insight: the tanking-index model does not show a clear team-fit interaction.",
  "\nBoth lines are fairly similar: moving from low to high fit changes",
  "\nthe predicted tanking index by ", round(strong_change, 2),
  " in stronger drafts and ", round(weak_change, 2), " in weaker drafts."
)

p <- ggplot(plot_data, aes(x = team_fit_score, y = tanking_index)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8b8f94", linewidth = 0.6) +
  geom_point(color = "#95a0a8", alpha = 0.34, size = 2.4) +
  geom_line(
    data = prediction_grid,
    aes(x = team_fit_score, y = predicted, color = draft_strength_group),
    linewidth = 2
  ) +
  geom_point(
    data = prediction_grid |>
      group_by(draft_strength_group) |>
      slice(c(1, n())) |>
      ungroup(),
    aes(x = team_fit_score, y = predicted, color = draft_strength_group),
    size = 4
  ) +
  annotate(
    "label",
    x = quantile(plot_data$team_fit_score, 0.43, na.rm = TRUE),
    y = quantile(plot_data$tanking_index, 0.85, na.rm = TRUE),
    label = insight_text,
    hjust = 0,
    vjust = 0,
    label.r = unit(0.12, "lines"),
    label.padding = unit(0.45, "lines"),
    fill = "#fbfaf7",
    color = "#17212e",
    size = 4.1,
    lineheight = 0.95
  ) +
  scale_color_manual(
    values = c(
      "Stronger top-four draft" = "#b73b4b",
      "Weaker top-four draft" = "#2f6690"
    )
  ) +
  labs(
    title = "Team Fit Adds Little to the Tanking-Index Model",
    subtitle = "Predicted tanking index among likely lottery teams. Gray dots show observed team-seasons.",
    x = "Team fit score",
    y = "Predicted / observed tanking index",
    color = NULL,
    caption = "Predictions set top-four draft strength to one standard deviation above or below average, hold pre-March win pct at its sample mean, and set disrupted-season indicators to zero."
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = "#fbfaf7", color = NA),
    panel.background = element_rect(fill = "#fbfaf7", color = NA),
    plot.title = element_text(face = "bold", size = 27, color = "#17212e"),
    plot.subtitle = element_text(size = 14.5, color = "#606a78", margin = margin(b = 16)),
    plot.caption = element_text(size = 10.5, color = "#606a78", hjust = 0),
    axis.title = element_text(face = "bold", color = "#17212e"),
    axis.text = element_text(color = "#303842"),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 12.5, color = "#17212e"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e1ded8"),
    plot.margin = margin(22, 36, 20, 36)
  )

ggsave(
  "team-fit-tanking-index-graph.png",
  p,
  width = 13.333,
  height = 7.5,
  dpi = 180,
  bg = "#fbfaf7"
)
