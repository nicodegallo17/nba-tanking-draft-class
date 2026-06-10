library(grid)

out_dir <- getwd()

slide_w <- 2400
slide_h <- 1350

colors <- list(
  ink = "#17212e",
  muted = "#606a78",
  equation = "#34446e",
  border = "#d8d4cc",
  bg = "#fbfaf7",
  blue = "#2f6690",
  gold = "#9f6a18",
  red = "#b73b4b",
  teal = "#2f807b",
  green = "#3f7d55",
  purple = "#7359a8"
)

wrap <- function(text, width) {
  paste(strwrap(text, width = width), collapse = "\n")
}

new_slide <- function(filename, title, subtitle) {
  png(filename, width = slide_w, height = slide_h, res = 160, bg = colors$bg)
  grid.newpage()
  grid.rect(gp = gpar(fill = colors$bg, col = NA))
  grid.text(
    title,
    x = 0.045, y = 0.93, just = c("left", "top"),
    gp = gpar(col = colors$ink, fontsize = 34, fontface = "bold")
  )
  grid.text(
    subtitle,
    x = 0.045, y = 0.875, just = c("left", "top"),
    gp = gpar(col = colors$muted, fontsize = 17)
  )
}

finish_slide <- function() {
  dev.off()
}

draw_card <- function(x, y, w, h, accent, header, body = NULL,
                      formulas = NULL, formula_y = NULL,
                      note = NULL, body_width = 48,
                      header_size = 19, body_size = 12.2,
                      formula_size = 21, note_size = 11.5) {
  grid.roundrect(
    x = x, y = y, width = w, height = h,
    just = c("left", "bottom"),
    r = unit(0.012, "npc"),
    gp = gpar(fill = "white", col = colors$border, lwd = 1.1)
  )
  grid.rect(
    x = x, y = y + h - 0.018, width = w, height = 0.018,
    just = c("left", "bottom"),
    gp = gpar(fill = accent, col = NA)
  )
  pad_x <- 0.018
  grid.text(
    header,
    x = x + pad_x, y = y + h - 0.043,
    just = c("left", "top"),
    gp = gpar(col = colors$ink, fontsize = header_size, fontface = "bold")
  )
  next_y <- y + h - 0.09
  if (!is.null(body) && nzchar(body)) {
    grid.text(
      wrap(body, body_width),
      x = x + pad_x, y = next_y,
      just = c("left", "top"),
      gp = gpar(col = colors$muted, fontsize = body_size, lineheight = 0.92)
    )
  }
  if (!is.null(formulas)) {
    n <- length(formulas)
    formula_ys <- if (!is.null(formula_y)) {
      y + h * formula_y
    } else if (n == 1) {
      y + h * 0.32
    } else if (n == 2) {
      y + h * c(0.40, 0.22)
    } else {
      y + h * seq(0.50, 0.20, length.out = n)
    }
    for (i in seq_along(formulas)) {
      grid.text(
        formulas[[i]],
        x = x + w / 2, y = formula_ys[i],
        just = "center",
        gp = gpar(col = colors$equation, fontsize = formula_size, fontfamily = "serif")
      )
    }
  }
  if (!is.null(note) && nzchar(note)) {
    grid.text(
      wrap(note, body_width + 6),
      x = x + pad_x, y = y + 0.035,
      just = c("left", "bottom"),
      gp = gpar(col = colors$muted, fontsize = note_size, lineheight = 0.94)
    )
  }
}

# Slide 1: basic win-loss variables
new_slide(
  file.path(out_dir, "variable-definitions-slide-1.png"),
  "Variable Definitions: Win Percentage and Playoff Distance",
  "These variables describe team quality and playoff position before the late-season window."
)

draw_card(
  0.045, 0.23, 0.43, 0.53, colors$blue,
  "pre_win_pct / late_win_pct",
  "Win percentage before March 1 and on/after March 1. Pre-March win percentage is the baseline team-strength control.",
  formulas = list(
    expression("Pre-March win pct" == frac(W["pre"], G["pre"]) * "," ~~ "Late win pct" == frac(W["late"], G["late"]))
  ),
  body_width = 45,
  formula_size = 19
)

draw_card(
  0.525, 0.23, 0.43, 0.53, colors$gold,
  "games_back_cutline",
  "Distance from the relevant postseason cutoff entering March. The cutoff is the 8th seed before the play-in era and the 10th seed beginning in 2021.",
  formulas = list(
    expression("Games back" == max * bgroup("(", 0 * "," ~ frac((W["cutline"] - W["team"]) + (L["team"] - L["cutline"]), 2), ")"))
  ),
  body_width = 45,
  formula_size = 19
)

finish_slide()

# Slide 2: sample and outcome
new_slide(
  file.path(out_dir, "variable-definitions-slide-2.png"),
  "Variable Definitions: Sample and Main Outcome",
  "The sample isolates likely lottery teams, then measures whether they underperformed late in the year."
)

draw_card(
  0.045, 0.59, 0.91, 0.22, colors$red,
  "likely_lottery_candidate",
  "Model sample filter. A team is included if it is outside the postseason cutoff and has a weak playoff outlook entering March.",
  formulas = list(
    expression("Likely lottery candidate" == bgroup("{", atop(
      "1, outside cutoff and (games back \u2265 3 or pre-March win pct < 0.400)",
      "0, otherwise"
    ), ""))
  ),
  body_width = 90,
  formula_y = 0.25,
  formula_size = 13
)

draw_card(
  0.045, 0.14, 0.91, 0.36, colors$red,
  "tanking_index",
  "Main win-loss outcome. It compares actual March-onward wins to expected March-onward wins based on the team's pre-March record.",
  formulas = list(
    expression(hat(W)["late"] == G["late"] %.% p["pre"]),
    expression("Tanking index" == frac(hat(W)["late"] - W["late"], sqrt(G["late"] * p["pre"] * (1 - p["pre"]))))
  ),
  formula_y = c(0.56, 0.37),
  note = "Positive values mean the team won fewer late-season games than expected, scaled by expected random variation.",
  body_width = 90,
  formula_size = 18
)

finish_slide()

# Slide 3: draft incentives
new_slide(
  file.path(out_dir, "variable-definitions-slide-3.png"),
  "Variable Definitions: Draft Incentives",
  "These variables connect the quality of a draft class to the payoff from losing lottery position."
)

draw_card(
  0.045, 0.23, 0.43, 0.53, colors$teal,
  "top4_avg_rating",
  "Season-level draft strength. It averages the CraftedNBA ratings for the four highest-rated prospects in each draft class.",
  formulas = list(expression("Top-four average rating" == frac(r[1] + r[2] + r[3] + r[4], 4))),
  body_width = 45,
  formula_size = 20
)

draw_card(
  0.525, 0.23, 0.43, 0.53, colors$teal,
  "marginal_lottery_incentive",
  "Team-specific incentive. It weights top-four draft strength by the added probability of landing a top-four pick from moving one lottery slot worse.",
  formulas = list(
    expression("Marginal lottery incentive" == ""),
    expression("Top-four average rating" %*% "Marginal top-four-pick probability gain")
  ),
  formula_y = c(0.43, 0.31),
  note = "Higher values mean losing one extra slot has a larger expected draft payoff.",
  body_width = 44,
  formula_size = 14
)

finish_slide()

# Slide 4: roster fit
new_slide(
  file.path(out_dir, "variable-definitions-slide-4.png"),
  "Variable Definitions: Roster Fit",
  "This exploratory measure asks whether a team's weak positions match the strengths of that year's draft."
)

draw_card(
  0.045, 0.23, 0.43, 0.53, colors$green,
  "team_fit_score: team need",
  "Players are grouped into guards, wings, and bigs. Before March 1, each team's production in a position group is measured with simple box-score value per 36 minutes.",
  formulas = list(
    expression("Value per 36" == 36 %.% frac("points" + "rebounds" + "assists" + "steals" + "blocks" - "turnovers", "minutes")),
    expression("Position need"[g] == max(0, -z("Value per 36"[g])))
  ),
  body_width = 43,
  formula_size = 14
)

draw_card(
  0.525, 0.23, 0.43, 0.53, colors$green,
  "team_fit_score: draft supply",
  "Draft supply uses the top 35 CraftedNBA prospects. Each prospect's value is max(rating - 50, 0), then summed into guard, wing, and big shares.",
  formulas = list(
    expression("Draft value share"[g] == frac(sum("Draft value"[j], j %in% g), sum("Draft value"[j], j))),
    expression("Team fit score" == sum("Position need"[g] %.% "Draft value share"[g], g %in% "{Guard, Wing, Big}"))
  ),
  note = "Higher fit means a team's weak positions match the positions where that draft is strongest.",
  body_width = 43,
  formula_size = 13
)

finish_slide()

# Slide 5: usage outcome
new_slide(
  file.path(out_dir, "variable-definitions-slide-5.png"),
  "Variable Definitions: Core Usage",
  "A separate outcome tests whether stronger drafts predict larger late-season reductions in core-player minutes."
)

draw_card(
  0.045, 0.23, 0.43, 0.53, colors$blue,
  "core player definition",
  "For each team-season, core players are selected mechanically using only pre-March games: rank players by total starts, use total minutes as the tiebreaker, and keep the top three.",
  note = "The model keeps only those core players who appeared for the same team after March 1.",
  body_width = 45
)

draw_card(
  0.525, 0.23, 0.43, 0.53, colors$blue,
  "core_usage_drop_rate",
  "Roster-management outcome. It measures the percentage drop in combined minutes per team game for the continuing top-three pre-March core players.",
  formulas = list(expression("Core usage drop rate" == frac("Pre-March core MPG" - "Late core MPG", "Pre-March core MPG"))),
  body_width = 44,
  formula_size = 16
)

finish_slide()

# Slide 6: model notes
new_slide(
  file.path(out_dir, "variable-definitions-slide-6.png"),
  "Variable Definitions: Model Notes",
  "These notes clarify what the models can and cannot claim."
)

draw_card(
  0.045, 0.23, 0.43, 0.53, colors$purple,
  "observational unit",
  "The win-loss, position-adjusted, team-fit, and core-usage models are team-season models after filtering to likely lottery candidates.",
  note = "Season-level draft strength is still shared across teams in a year, so we report season-clustered inference when that matters.",
  body_width = 45
)

draw_card(
  0.525, 0.23, 0.43, 0.53, colors$purple,
  "important caveats",
  "The measures are proxies, not direct proof of intent. A high tanking index could reflect injuries, fatigue, schedule strength, or roster changes as well as strategic incentives.",
  note = "Controls include pre-March team strength or pre-March core MPG plus disrupted-season indicators for 2012, 2020, and 2021.",
  body_width = 44
)

finish_slide()
