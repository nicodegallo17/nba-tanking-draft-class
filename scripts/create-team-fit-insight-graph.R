library(knitr)
library(tidyverse)
library(ggrepel)
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
    !is.na(core_usage_drop_rate),
    !is.na(top4_avg_rating)
  ) |>
  mutate(
    draft_strength_group = if_else(
      top4_avg_rating >= median(top4_avg_rating, na.rm = TRUE),
      "Stronger top-four draft",
      "Weaker top-four draft"
    ),
    core_usage_drop_pct = 100 * core_usage_drop_rate,
    label = NA_character_
  )

top4_mean <- mean(candidate_data$top4_avg_rating, na.rm = TRUE)
top4_sd <- sd(candidate_data$top4_avg_rating, na.rm = TRUE)
fit_mean <- mean(candidate_data$team_fit_score, na.rm = TRUE)
fit_sd <- sd(candidate_data$team_fit_score, na.rm = TRUE)
core_mpg_mean <- mean(candidate_data$core_mpg_pre_march, na.rm = TRUE)

prediction_grid <- expand_grid(
  team_fit_score = seq(
    quantile(plot_data$team_fit_score, 0.03, na.rm = TRUE),
    quantile(plot_data$team_fit_score, 0.97, na.rm = TRUE),
    length.out = 100
  ),
  draft_strength_group = c("Stronger top-four draft", "Weaker top-four draft")
) |>
  mutate(
    top4_avg_rating = case_when(
      draft_strength_group == "Stronger top-four draft" ~ top4_mean + top4_sd,
      TRUE ~ top4_mean - top4_sd
    ),
    core_mpg_pre_march = core_mpg_mean,
    season_2012 = FALSE,
    season_2020 = FALSE,
    season_2021 = FALSE
  )

prediction_grid$predicted <- predict(
  model_core_usage_fit,
  newdata = prediction_grid
) * 100

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

insight_text <- paste0(
  "Insight: the model predicts a larger usage drop when team fit is high",
  "\nand the draft class is strong. In the stronger-draft line, moving",
  "\nfrom low to high fit adds about ", round(strong_change, 1), " percentage points."
)

p <- ggplot(
  plot_data,
  aes(x = team_fit_score, y = core_usage_drop_pct)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8b8f94", linewidth = 0.6) +
  geom_point(
    color = "#95a0a8",
    alpha = 0.32,
    size = 2.2
  ) +
  geom_line(
    data = prediction_grid,
    aes(
      x = team_fit_score,
      y = predicted,
      color = draft_strength_group
    ),
    linewidth = 2
  ) +
  geom_point(
    data = prediction_grid |>
      group_by(draft_strength_group) |>
      slice(c(1, n())) |>
      ungroup(),
    aes(
      x = team_fit_score,
      y = predicted,
      color = draft_strength_group
    ),
    size = 4
  ) +
  annotate(
    "label",
    x = quantile(plot_data$team_fit_score, 0.50, na.rm = TRUE),
    y = quantile(plot_data$core_usage_drop_pct, 0.82, na.rm = TRUE),
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
  scale_y_continuous(labels = label_number(suffix = " pp")) +
  labs(
    title = "Team Fit Matters More When the Draft Is Strong",
    subtitle = "Predicted core-player usage drop among likely lottery teams. Gray dots show observed team-seasons.",
    x = "Team fit score",
    y = "Predicted / observed core usage drop rate",
    color = NULL,
    caption = "Predictions use the core-usage team-fit model, holding pre-March core MPG at its sample mean and disrupted-season indicators at zero."
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
  "team-fit-insight-graph.png",
  p,
  width = 13.333,
  height = 7.5,
  dpi = 180,
  bg = "#fbfaf7"
)
