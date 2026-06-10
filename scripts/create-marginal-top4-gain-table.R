library(grid)

slide_w <- 2400
slide_h <- 1350

colors <- list(
  bg = "#fbfaf7",
  ink = "#17212e",
  muted = "#606a78",
  equation = "#34446e",
  red = "#b73b4b",
  blue = "#2f6690",
  teal = "#2f807b",
  gold = "#9f6a18",
  line = "#d8d4cc",
  header = "#eef2f4",
  current = "#f5e8ea",
  old = "#e8eef4"
)

compute_lottery_odds <- function(weights, draws) {
  seeds <- seq_along(weights)
  top4_pick_odds <- numeric(length(weights))

  add_draw_path <- function(remaining, drawn, path_probability) {
    if (length(drawn) == draws) {
      final_order <- c(drawn, setdiff(seeds, drawn))
      top4_pick_odds[final_order[1:4]] <<-
        top4_pick_odds[final_order[1:4]] + path_probability
      return(invisible(NULL))
    }

    total_weight <- sum(weights[remaining])
    for (seed in remaining) {
      add_draw_path(
        remaining = setdiff(remaining, seed),
        drawn = c(drawn, seed),
        path_probability = path_probability * weights[seed] / total_weight
      )
    }
  }

  add_draw_path(seeds, integer(), 1)

  data.frame(
    seed = seeds,
    top4 = top4_pick_odds,
    stringsAsFactors = FALSE
  )
}

make_odds_table <- function(weights, draws) {
  odds <- compute_lottery_odds(weights, draws)
  odds$marginal_gain <- pmax(0, c(NA, head(odds$top4, -1)) - odds$top4)
  odds$marginal_gain[is.na(odds$marginal_gain)] <- 0
  odds
}

fmt_pct <- function(x) sprintf("%.1f%%", 100 * x)

post <- make_odds_table(
  weights = c(140, 140, 140, 125, 105, 90, 75, 60, 45, 30, 20, 15, 10, 5),
  draws = 4
)

pre <- make_odds_table(
  weights = c(250, 199, 156, 119, 88, 63, 43, 28, 17, 11, 8, 7, 6, 5),
  draws = 3
)

draw_table <- function(data, x, y, w, h, title, accent, note) {
  grid.roundrect(
    x = x, y = y, width = w, height = h,
    just = c("left", "bottom"),
    r = unit(0.012, "npc"),
    gp = gpar(fill = "white", col = colors$line, lwd = 1.2)
  )
  grid.rect(
    x = x, y = y + h - 0.021, width = w, height = 0.021,
    just = c("left", "bottom"),
    gp = gpar(fill = accent, col = NA)
  )
  grid.text(
    title,
    x = x + 0.02, y = y + h - 0.045,
    just = c("left", "top"),
    gp = gpar(col = colors$ink, fontsize = 19, fontface = "bold")
  )
  grid.text(
    note,
    x = x + 0.02, y = y + h - 0.083,
    just = c("left", "top"),
    gp = gpar(col = colors$muted, fontsize = 10.8)
  )

  table_top <- y + h - 0.13
  table_bottom <- y + 0.035
  table_h <- table_top - table_bottom
  n_rows <- nrow(data) + 1
  row_h <- table_h / n_rows

  col_rel <- c(0.18, 0.39, 0.43)
  col_x <- x + c(0, cumsum(col_rel[-length(col_rel)]) * w)
  col_w <- col_rel * w

  grid.rect(
    x = x + 0.012, y = table_top - row_h,
    width = w - 0.024, height = row_h,
    just = c("left", "bottom"),
    gp = gpar(fill = colors$header, col = NA)
  )

  headers <- c("Seed", "Top-four\nprobability", "Marginal gain\none slot worse")
  for (j in seq_along(headers)) {
    grid.text(
      headers[j],
      x = col_x[j] + col_w[j] / 2, y = table_top - row_h / 2,
      just = "center",
      gp = gpar(col = colors$ink, fontsize = 10.8, fontface = "bold", lineheight = 0.92)
    )
  }

  for (i in seq_len(nrow(data))) {
    row_y <- table_top - (i + 1) * row_h
    if (i %% 2 == 0) {
      grid.rect(
        x = x + 0.012, y = row_y,
        width = w - 0.024, height = row_h,
        just = c("left", "bottom"),
        gp = gpar(fill = "#fafafa", col = NA)
      )
    }
    values <- c(data$seed[i], fmt_pct(data$top4[i]), fmt_pct(data$marginal_gain[i]))
    for (j in seq_along(values)) {
      grid.text(
        values[j],
        x = col_x[j] + col_w[j] / 2, y = row_y + row_h / 2,
        just = "center",
        gp = gpar(col = colors$ink, fontsize = 11.5)
      )
    }
  }

  for (k in 0:n_rows) {
    yy <- table_top - k * row_h
    grid.lines(
      x = c(x + 0.012, x + w - 0.012),
      y = c(yy, yy),
      gp = gpar(col = colors$line, lwd = 0.7)
    )
  }
}

png("marginal-top4-pick-gain-table.png", width = slide_w, height = slide_h, res = 160, bg = colors$bg)
grid.newpage()
grid.rect(gp = gpar(fill = colors$bg, col = NA))

grid.text(
  "Marginal Top-Four Pick Gain by Lottery Position",
  x = 0.045, y = 0.925,
  just = c("left", "top"),
  gp = gpar(col = colors$ink, fontsize = 32, fontface = "bold")
)
grid.text(
  "Marginal gain is the added probability of a top-four pick from moving one lottery seed worse.",
  x = 0.045, y = 0.872,
  just = c("left", "top"),
  gp = gpar(col = colors$muted, fontsize = 15)
)

draw_table(
  post,
  x = 0.045, y = 0.125, w = 0.43, h = 0.69,
  title = "Post-2019 / Current Odds",
  accent = colors$red,
  note = "Four lottery picks drawn; seeds 1-3 have equal top-four odds."
)

draw_table(
  pre,
  x = 0.525, y = 0.125, w = 0.43, h = 0.69,
  title = "Pre-2019 Odds",
  accent = colors$blue,
  note = "Three lottery picks drawn; worst team could fall no lower than fourth."
)

grid.text(
  "Used in the model as: marginal_lottery_incentive = top4_avg_rating x marginal_top4_pick_gain",
  x = 0.5, y = 0.055,
  just = "center",
  gp = gpar(col = colors$equation, fontsize = 14, fontfamily = "serif")
)

dev.off()
