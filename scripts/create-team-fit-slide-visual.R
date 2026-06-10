library(knitr)
library(tidyverse)
library(grid)

tmp <- tempfile(fileext = ".R")
purl("final-report.qmd", output = tmp, quiet = TRUE)
suppressMessages({
  sink(tempfile())
  source(tmp, local = FALSE)
  sink()
})

example_team <- candidate_data |>
  arrange(desc(team_fit_score)) |>
  slice(1) |>
  select(season, team_abbreviation, team_fit_score, top4_avg_rating)

example_need <- team_position_need |>
  semi_join(example_team, by = c("season", "team_abbreviation")) |>
  select(position_group, position_need)

example_supply <- draft_position_supply |>
  filter(draft_year == example_team$season[1]) |>
  select(position_group, draft_value_share)

example_data <- full_join(example_need, example_supply, by = "position_group") |>
  mutate(
    position_need = replace_na(position_need, 0),
    draft_value_share = replace_na(draft_value_share, 0),
    contribution = position_need * draft_value_share,
    position_group = factor(position_group, levels = c("Guard", "Wing", "Big"))
  ) |>
  arrange(position_group)

position_colors <- c(
  Guard = "#2f6690",
  Wing = "#b73b4b",
  Big = "#2f807b"
)

colors <- list(
  bg = "#fbfaf7",
  ink = "#17212e",
  muted = "#606a78",
  line = "#d8d4cc",
  card = "white",
  red = "#b73b4b",
  equation = "#34446e"
)

fmt_num <- function(x) sprintf("%.2f", x)
fmt_pct <- function(x) sprintf("%.0f%%", 100 * x)

bar_card <- function(x, y, w, h, title, subtitle, values, labels, value_labels, max_value) {
  grid.roundrect(
    x = x, y = y, width = w, height = h,
    just = c("left", "bottom"),
    r = unit(0.012, "npc"),
    gp = gpar(fill = colors$card, col = colors$line, lwd = 1.2)
  )
  grid.rect(
    x = x, y = y + h - 0.018, width = w, height = 0.018,
    just = c("left", "bottom"),
    gp = gpar(fill = colors$red, col = NA)
  )
  grid.text(
    title,
    x = x + 0.018, y = y + h - 0.045,
    just = c("left", "top"),
    gp = gpar(col = colors$ink, fontsize = 18, fontface = "bold")
  )
  grid.text(
    subtitle,
    x = x + 0.018, y = y + h - 0.080,
    just = c("left", "top"),
    gp = gpar(col = colors$muted, fontsize = 10.5)
  )

  bar_left <- x + 0.09
  bar_right <- x + w - 0.06
  bar_w <- bar_right - bar_left
  ys <- y + h * c(0.52, 0.37, 0.22)

  for (i in seq_along(values)) {
    group <- as.character(labels[i])
    grid.text(
      group,
      x = x + 0.022, y = ys[i],
      just = c("left", "center"),
      gp = gpar(col = colors$ink, fontsize = 12.5, fontface = "bold")
    )
    grid.roundrect(
      x = bar_left, y = ys[i],
      width = bar_w, height = 0.025,
      just = c("left", "center"),
      r = unit(0.004, "npc"),
      gp = gpar(fill = "#edf1f2", col = NA)
    )
    grid.roundrect(
      x = bar_left, y = ys[i],
      width = bar_w * ifelse(max_value > 0, values[i] / max_value, 0),
      height = 0.025,
      just = c("left", "center"),
      r = unit(0.004, "npc"),
      gp = gpar(fill = position_colors[group], col = NA)
    )
    grid.text(
      value_labels[i],
      x = bar_right + 0.015, y = ys[i],
      just = c("left", "center"),
      gp = gpar(col = colors$ink, fontsize = 11.5)
    )
  }
}

draw_arrow <- function(x1, x2, y) {
  grid.lines(
    x = c(x1, x2), y = c(y, y),
    arrow = arrow(length = unit(0.018, "npc"), type = "closed"),
    gp = gpar(col = colors$muted, lwd = 2)
  )
}

png("team-fit-slide-visual.png", width = 2400, height = 1350, res = 160, bg = colors$bg)
grid.newpage()
grid.rect(gp = gpar(fill = colors$bg, col = NA))

grid.text(
  "Team Fit Score: Need x Draft Supply",
  x = 0.045, y = 0.925,
  just = c("left", "top"),
  gp = gpar(col = colors$ink, fontsize = 34, fontface = "bold")
)
grid.text(
  "Example from the data: Charlotte 2013 had weak frontcourt production, and the 2013 draft was strongest among bigs.",
  x = 0.045, y = 0.872,
  just = c("left", "top"),
  gp = gpar(col = colors$muted, fontsize = 15)
)

bar_card(
  x = 0.045, y = 0.37, w = 0.27, h = 0.40,
  title = "1. Team Need",
  subtitle = "Pre-March weakness by position",
  values = example_data$position_need,
  labels = example_data$position_group,
  value_labels = fmt_num(example_data$position_need),
  max_value = max(example_data$position_need)
)

bar_card(
  x = 0.365, y = 0.37, w = 0.27, h = 0.40,
  title = "2. Draft Supply",
  subtitle = "Share of top-35 draft value",
  values = example_data$draft_value_share,
  labels = example_data$position_group,
  value_labels = fmt_pct(example_data$draft_value_share),
  max_value = max(example_data$draft_value_share)
)

bar_card(
  x = 0.685, y = 0.37, w = 0.27, h = 0.40,
  title = "3. Fit Contribution",
  subtitle = "Need score x draft share",
  values = example_data$contribution,
  labels = example_data$position_group,
  value_labels = fmt_num(example_data$contribution),
  max_value = max(example_data$contribution)
)

draw_arrow(0.325, 0.355, 0.57)
draw_arrow(0.645, 0.675, 0.57)

grid.roundrect(
  x = 0.045, y = 0.16, width = 0.91, height = 0.13,
  just = c("left", "bottom"),
  r = unit(0.012, "npc"),
  gp = gpar(fill = "white", col = colors$line, lwd = 1.1)
)
grid.text(
  paste0(
    "Team fit score = sum(Position need_g x Draft value share_g) = ",
    fmt_num(example_team$team_fit_score[1])
  ),
  x = 0.5, y = 0.235,
  just = "center",
  gp = gpar(col = colors$equation, fontsize = 18, fontfamily = "serif")
)
grid.text(
  "Higher fit means the team's weaknesses line up with the draft class's strengths. This is a rough proxy, not a true team draft board.",
  x = 0.5, y = 0.085,
  just = "center",
  gp = gpar(col = colors$muted, fontsize = 12.5)
)

dev.off()
