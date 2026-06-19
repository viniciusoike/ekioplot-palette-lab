# ---- Full-size plot functions ----
# Each takes data + colors (character vector) and returns a ggplot object.

plot_area_stacked <- function(df, colors) {
  n <- length(levels(df$fuel))
  ggplot(df, aes(year, consumption_gwh, fill = fuel)) +
    geom_area() +
    geom_hline(yintercept = 0) +
    scale_x_continuous(expand = expansion(mult = 0)) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = \(x) paste0(round(x / 1000), "k")
    ) +
    scale_fill_manual(values = colors) +
    labs(
      title = paste("Stacked Area \u2014", n, "groups"),
      x = NULL,
      y = "GWh",
      fill = NULL
    ) +
    theme_ekio() +
    theme(axis.ticks.x = element_line(linewidth = 0.25))
}

plot_area_share <- function(df, colors) {
  n <- length(levels(df$fuel))
  ggplot(df, aes(year, share, fill = fuel)) +
    geom_area() +
    geom_hline(yintercept = 0) +
    scale_x_continuous(expand = expansion(mult = 0)) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = \(x) paste0(round(x), "%")
    ) +
    scale_fill_manual(values = colors) +
    labs(
      title = paste("Share \u2014", n, "groups"),
      x = NULL,
      y = "Share",
      fill = NULL
    ) +
    theme_ekio() +
    theme(axis.ticks.x = element_line(linewidth = 0.25))
}

# ---- Line plots ----

plot_line_labeled <- function(df, colors) {
  n <- length(levels(df$fuel))
  last_yr <- max(df$year)

  end_pts <- df |>
    filter(year == last_yr) |>
    mutate(
      label = paste0(
        tools::toTitleCase(as.character(fuel)),
        "\n",
        round(consumption_gwh / 1000, 1),
        "k"
      )
    )

  ggplot(df, aes(year, consumption_gwh, color = fuel)) +
    geom_line(linewidth = 0.8) +
    geom_label(
      data = end_pts,
      aes(label = label),
      hjust = 0,
      size = 2.5,
      nudge_x = 2,
      linewidth = 0.2
    ) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.05)),
      limits = c(NA, last_yr + 15)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = \(x) paste0(round(x / 1000), "k")
    ) +
    scale_color_manual(values = colors) +
    guides(color = "none") +
    labs(
      title = paste("Line Chart \u2014", n, "series"),
      x = NULL,
      y = "GWh"
    ) +
    theme_ekio() +
    theme(axis.ticks.x = element_line(linewidth = 0.25))
}

plot_line_faceted <- function(df, colors) {
  ggplot(df, aes(year, consumption_gwh)) +
    geom_line(linewidth = 0.7, color = colors[1]) +
    facet_wrap(vars(fuel)) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = \(x) paste0(round(x / 1000), "k")
    ) +
    labs(title = "Small Multiples", x = NULL, y = "GWh") +
    theme_ekio() +
    theme(
      axis.ticks.x = element_line(linewidth = 0.25),
      axis.line.x = element_line(linewidth = 0.25)
    )
}

plot_line_single <- function(df, colors) {
  ggplot(df, aes(year, total)) +
    geom_line(color = colors[1], linewidth = 0.8) +
    geom_point(color = colors[1], size = 1.5) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = \(x) paste0(round(x / 1000), "k")
    ) +
    labs(title = "Single series \u2014 line + dots", x = NULL, y = "GWh") +
    theme_ekio() +
    theme(axis.ticks.x = element_line(linewidth = 0.25))
}

plot_line_single_trend <- function(df, colors) {
  ggplot(df, aes(date)) +
    geom_line(
      aes(y = co2),
      color = colors[1],
      alpha = 0.5,
      lwd = 0.5
    ) +
    geom_line(
      aes(y = trend_stl),
      color = colors[1],
      lwd = 0.8
    ) +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
    theme_ekio() +
    theme(
      axis.ticks.x = element_line(linewidth = 0.25),
      axis.line.x = element_line(linewidth = 0.25)
    )
}

plot_line_single_trend_dots <- function(df, colors) {
  ggplot(subset(df, date >= "1980-01-01"), aes(date)) +
    geom_point(
      aes(y = co2),
      color = colors[1],
      alpha = 0.3,
      size = 1
    ) +
    geom_line(
      aes(y = trend_stl),
      color = colors[1],
      lwd = 0.8
    ) +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
    theme_ekio() +
    theme(
      axis.ticks.x = element_line(linewidth = 0.25),
      axis.line.x = element_line(linewidth = 0.25)
    )
}


# ---- Bar plots ----

plot_bar_vertical <- function(df, colors) {
  ggplot(df, aes(x = cut, y = share)) +
    geom_col(fill = colors[1], width = 0.7) +
    geom_hline(yintercept = 0) +
    geom_text(aes(y = share + 1.5, label = label), size = 3.5) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.08)),
      labels = \(x) paste0(x, "%")
    ) +
    labs(title = "Diamond Cuts (vertical)", x = NULL, y = "Share") +
    theme_ekio()
}

plot_bar_horizontal <- function(df, colors) {
  ggplot(df, aes(x = cut, y = share)) +
    geom_col(fill = colors[1], width = 0.6) +
    geom_text(aes(x = cut, y = 0.5, label = cut), hjust = 0, size = 3) +
    geom_text(
      aes(
        y = ifelse(share > 5, share - 1.5, share + 1.5),
        label = label
      ),
      fontface = "bold",
      size = 3,
      color = ifelse(df$share > 5, "white", "#1A202C")
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = \(x) paste0(x, "%")
    ) +
    coord_flip() +
    labs(title = "Diamond Cuts (horizontal)", x = NULL, y = NULL) +
    theme_ekio() +
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

plot_bar_labels_inside <- function(df, colors) {
  ggplot(df, aes(x = cut, y = share)) +
    geom_col(fill = colors[1], width = 0.7) +
    geom_hline(yintercept = 0) +
    geom_text(
      aes(y = share - 1.5, label = label),
      color = "white",
      fontface = "bold",
      size = 3.5
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = \(x) paste0(x, "%")
    ) +
    labs(title = "Labels inside bars", x = NULL, y = "Share") +
    theme_ekio()
}

plot_bar_highlighted <- function(df, highlight_col, non_highlight_col) {
  df <- df |>
    mutate(
      highlight = if_else(
        cut == levels(cut)[length(levels(cut))],
        "top",
        "rest"
      )
    )

  fill_vals <- c(rest = non_highlight_col, top = highlight_col)

  ggplot(df, aes(x = cut, y = share, fill = highlight)) +
    geom_col(width = 0.6) +
    geom_text(aes(x = cut, y = 0.5, label = cut), hjust = 0, size = 3) +
    geom_text(
      aes(
        y = ifelse(share > 5, share - 1.5, share + 1.5),
        label = label
      ),
      fontface = "bold",
      size = 3,
      color = ifelse(df$share > 5, "white", non_highlight_col)
    ) +
    scale_fill_manual(values = fill_vals) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = \(x) paste0(x, "%")
    ) +
    coord_flip() +
    guides(fill = "none") +
    labs(title = "Highlighted category", x = NULL, y = NULL) +
    theme_ekio() +
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

plot_bar_highlighted_top <- function(df, highlight_col, non_highlight_col) {
  top_level <- levels(df$cut)[length(levels(df$cut))]
  df <- df |>
    mutate(
      highlight = factor(if_else(cut == top_level, 1L, 0L)),
      ytext = if_else(share < 5, share + 1.5, share - 1.5),
      coltext = factor(if_else(share < 5, 1L, 0L))
    )

  ggplot(df, aes(x = cut)) +
    geom_col(aes(y = share, fill = highlight), width = 0.5) +
    geom_text(
      aes(x = cut, y = 0, label = cut),
      hjust = 0, nudge_x = 0.4, size = 3
    ) +
    geom_text(
      aes(x = cut, y = ytext, label = label, color = coltext),
      fontface = "bold", size = 3
    ) +
    scale_y_continuous(
      breaks = seq(0, 40, 10),
      labels = \(x) paste0(x, "%"),
      limits = c(0, 45),
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = c(non_highlight_col, highlight_col)) +
    scale_color_manual(values = c("#ffffff", "#1A202C")) +
    guides(fill = "none", color = "none") +
    coord_flip() +
    labs(title = "Highlighted top category", x = NULL, y = NULL) +
    theme_ekio() +
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

# ---- Scatter plots ----

plot_scatter_single <- function(df, colors) {
  ggplot(df, aes(mpg, wt)) +
    geom_point(color = colors[1], size = 2.5) +
    labs(title = "Scatter (single color)", x = "MPG", y = "Weight") +
    theme_ekio(grid = "xy")
}

plot_scatter_grouped <- function(df, colors) {
  n_cyl <- length(unique(df$cyl))
  ggplot(df, aes(mpg, wt, fill = cyl)) +
    geom_point(shape = 21, color = "#ffffff", size = 3) +
    scale_fill_manual(values = colors[seq_len(min(n_cyl, length(colors)))]) +
    labs(
      title = "Scatter (grouped)",
      x = "MPG",
      y = "Weight",
      fill = "Cylinders"
    ) +
    theme_ekio(grid = "xy")
}

# ---- Histogram ----

plot_histogram <- function(colors) {
  ggplot(diamonds, aes(x = carat)) +
    geom_histogram(
      bins = 30,
      fill = colors[1],
      color = "#ffffff",
      linewidth = 0.25
    ) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = paste("Histogram \u2014 primary:", colors[1]),
      x = "Carat",
      y = "Count"
    ) +
    theme_ekio()
}

plot_histogram_fine <- function(colors) {
  ggplot(diamonds, aes(x = carat)) +
    geom_histogram(
      bins = 15,
      fill = colors[1],
      color = "#ffffff",
      linewidth = 0.25
    ) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Histogram \u2014 15 bins", x = "Carat", y = "Count") +
    theme_ekio()
}

plot_histogram_density <- function(colors) {
  ggplot(diamonds, aes(x = carat)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 30,
      fill = colors[1],
      color = "#ffffff",
      linewidth = 0.25,
      alpha = 0.7
    ) +
    geom_density(
      color = colors[min(2, length(colors))],
      linewidth = 0.8
    ) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Histogram + density", x = "Carat", y = "Density") +
    theme_ekio()
}

plot_histogram_by_cut <- function(colors) {
  ggplot(diamonds, aes(x = carat)) +
    geom_histogram(
      bins = 25,
      fill = colors[1],
      color = "#ffffff",
      linewidth = 0.15
    ) +
    geom_hline(yintercept = 0) +
    facet_wrap(vars(cut)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = "By cut (faceted)", x = "Carat", y = "Count") +
    theme_ekio() +
    theme(axis.line.x = element_line(linewidth = 0.25))
}

# ---- Mini plots for "All Charts" tab ----

plot_mini_area <- function(df, colors) {
  ggplot(df, aes(year, consumption_gwh, fill = fuel)) +
    geom_area() +
    geom_hline(yintercept = 0) +
    scale_x_continuous(expand = expansion(mult = 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = colors) +
    labs(title = "Area", x = NULL, y = NULL, fill = NULL) +
    theme_ekio() +
    theme(legend.position = "none")
}

plot_mini_line <- function(df, colors) {
  ggplot(df, aes(year, consumption_gwh, color = fuel)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(expand = expansion(mult = 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual(values = colors) +
    labs(title = "Lines", x = NULL, y = NULL, color = NULL) +
    theme_ekio() +
    theme(legend.position = "none")
}

plot_mini_bar <- function(df, colors) {
  ggplot(df, aes(x = cut, y = share)) +
    geom_col(fill = colors[1], width = 0.7) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Bar", x = NULL, y = NULL) +
    theme_ekio()
}

# ---- Complex plots ----

plot_bubble <- function(df, colors) {
  n <- min(length(colors), 5)

  # Dynamic binning based on number of colors. `n` is scalar, so branch with
  # plain control flow \u2014 a scalar-LHS case_when() over a vector RHS is
  # deprecated in dplyr.
  bins <- if (n == 2) {
    if_else(df$percentage < 25, "<25", "\u226525")
  } else if (n == 3) {
    case_when(
      df$percentage < 25 ~ "<25",
      df$percentage < 30 ~ "25-29",
      TRUE ~ "\u226530"
    )
  } else if (n == 4) {
    case_when(
      df$percentage < 20 ~ "<20",
      df$percentage < 25 ~ "20-24",
      df$percentage < 30 ~ "25-29",
      TRUE ~ "\u226530"
    )
  } else {
    case_when(
      df$percentage < 18 ~ "<18",
      df$percentage < 25 ~ "18-24",
      df$percentage < 30 ~ "25-29",
      df$percentage < 35 ~ "30-34",
      TRUE ~ "\u226535"
    )
  }
  df <- df |> mutate(age_group = bins)

  lvls <- sort(unique(df$age_group))
  df <- df |>
    mutate(
      age_group = factor(age_group, levels = lvls),
      text_color = if_else(percentage >= 25, "white", "black")
    )

  ggplot(df, aes(x = country, y = job_factor)) +
    geom_point(aes(size = percentage, color = age_group), stroke = 0.5) +
    geom_vline(xintercept = 6.465, color = "grey80", linewidth = 0.25) +
    geom_text(aes(label = percentage), color = df$text_color, size = 3) +
    scale_x_discrete(position = "top") +
    scale_size_continuous(
      range = c(9, 14),
      breaks = c(15, 25, 35, 45),
      labels = c("15%", "25%", "35%", "45%"),
      guide = "none"
    ) +
    scale_color_manual(values = colors[seq_len(n)]) +
    guides(color = guide_legend()) +
    labs(title = "Job Change Drivers", x = NULL, y = NULL, color = NULL) +
    theme_ekio(grid = "none") +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8, hjust = 1),
      legend.position = "bottom",
      legend.direction = "horizontal",
      aspect.ratio = 1
    )
}

plot_bump <- function(ranking, df_gdp, measures, countries_sel, colors) {
  n_hl <- min(length(colors) - 1, length(countries_sel))
  sel <- countries_sel[seq_len(n_hl)]
  col_vec <- c(colors[seq_len(n_hl)], colors[length(colors)])

  ranking <- ranking |>
    mutate(
      highlight = if_else(country %in% sel, country, ""),
      highlight = factor(highlight, levels = c(sel, "")),
      is_highlight = factor(if_else(country %in% sel, 1L, 0L))
    )

  ggplot(ranking, aes(measure, rank, group = country)) +
    geom_bump(aes(color = highlight, linewidth = is_highlight)) +
    geom_point(shape = 21, color = "white", aes(fill = highlight), size = 3) +
    geom_text(
      data = filter(ranking, measure == measures[[3]], is_highlight != 1L),
      aes(x = measure, y = rank, label = country),
      nudge_x = 0.05, hjust = 0, size = 3
    ) +
    geom_text(
      data = filter(ranking, measure == measures[[3]], is_highlight == 1L),
      aes(x = measure, y = rank, label = country),
      nudge_x = 0.05, hjust = 0, fontface = "bold", size = 3
    ) +
    geom_text(
      data = filter(ranking, measure == measures[[1]]),
      aes(x = measure, y = rank, label = rank_labels),
      nudge_x = -0.15, hjust = 0, size = 3
    ) +
    geom_text(
      data = df_gdp,
      aes(x = measure, y = position, label = measure_label),
      inherit.aes = FALSE, hjust = 0, fontface = "bold", size = 3
    ) +
    coord_cartesian(ylim = c(21, -2)) +
    scale_color_manual(values = col_vec) +
    scale_fill_manual(values = col_vec) +
    scale_linewidth_manual(values = c(0.5, 1.2)) +
    labs(title = "GDP Ranking", x = NULL, y = NULL) +
    theme_ekio(grid = "none") +
    theme(
      legend.position = "none",
      axis.text = element_blank()
    )
}

plot_pyramid <- function(df, colors) {
  dat <- filter(df, year %in% c(1970, 2000))
  breaks_share <- seq(-10, 10, 2)
  labels_share <- str_remove(as.character(breaks_share), "-")

  p1 <- ggplot(
    filter(dat, sex == "Male"),
    aes(share, age_trunc, alpha = as.factor(year))
  ) +
    geom_col(position = position_dodge(), fill = colors[1]) +
    scale_alpha_manual(
      name = "", values = c(0.5, 1),
      labels = c("Male (1970)", "Male (2000)")
    ) +
    scale_x_continuous(
      breaks = breaks_share, labels = labels_share,
      limits = c(-8, 0), expand = expansion(mult = c(0.05, 0))
    ) +
    labs(x = NULL, y = NULL) +
    theme_ekio(grid = "x") +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_line(linetype = 2),
      axis.text.y = element_blank()
    )

  p2 <- ggplot(
    filter(dat, sex == "Female"),
    aes(share, age_trunc, alpha = as.factor(year))
  ) +
    geom_col(position = position_dodge(), fill = colors[min(2, length(colors))]) +
    scale_alpha_manual(
      name = "", values = c(0.5, 1),
      labels = c("Female (1970)", "Female (2000)")
    ) +
    scale_x_continuous(
      breaks = breaks_share, labels = labels_share,
      limits = c(0, 8), expand = expansion(mult = c(0, 0.05))
    ) +
    labs(title = "Brazil \u2014 Population Pyramid", x = NULL, y = NULL) +
    theme_ekio(grid = "x") +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_line(linetype = 2),
      axis.text.y = element_text(hjust = 0.5)
    )

  (p1 | p2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

# ---- Mini plots for "All Charts" tab ----

plot_mini_scatter <- function(df, colors) {
  ggplot(df, aes(mpg, wt, fill = cyl)) +
    geom_point(shape = 21, color = "#ffffff", size = 2.5) +
    scale_fill_manual(values = colors[seq_len(min(3, length(colors)))]) +
    labs(title = "Scatter", x = NULL, y = NULL, fill = NULL) +
    theme_ekio(grid = "xy") +
    theme(legend.position = "none")
}

# ---- Continuous palette plots ----

plot_heatmap <- function(colors) {
  vol_df <- expand.grid(
    x = seq_len(nrow(volcano)),
    y = seq_len(ncol(volcano))
  )
  vol_df$z <- as.vector(volcano)

  ggplot(vol_df, aes(x, y, fill = z)) +
    geom_raster() +
    scale_fill_gradientn(colours = colors) +
    coord_fixed() +
    labs(title = "Continuous fill (volcano)", x = NULL, y = NULL, fill = "Height") +
    theme_ekio(grid = "none") +
    theme(axis.text = element_blank())
}

plot_gradient_scatter <- function(df, colors) {
  ggplot(df, aes(wt, mpg, color = hp)) +
    geom_point(size = 3) +
    scale_color_gradientn(colours = colors) +
    labs(
      title = "Gradient scatter",
      x = "Weight", y = "MPG", color = "Horsepower"
    ) +
    theme_ekio(grid = "xy")
}

plot_diverging_bar <- function(colors) {
  df <- mtcars |>
    tibble::rownames_to_column("car") |>
    dplyr::mutate(
      mpg_z = (mpg - mean(mpg)) / sd(mpg),
      car = forcats::fct_reorder(car, mpg_z)
    ) |>
    dplyr::slice_max(abs(mpg_z), n = 20)

  ggplot(df, aes(x = car, y = mpg_z, fill = mpg_z)) +
    geom_col(width = 0.7) +
    scale_fill_gradientn(colours = colors, limits = c(-2.5, 2.5)) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    coord_flip() +
    labs(
      title = "Diverging bar (MPG z-scores)",
      x = NULL, y = "Standard deviations from mean", fill = "z"
    ) +
    theme_ekio()
}

plot_correlation <- function(colors) {
  vars <- c("mpg", "cyl", "disp", "hp", "wt", "qsec")
  cor_mat <- cor(mtcars[, vars])

  df <- expand.grid(
    x = factor(vars, levels = vars),
    y = factor(vars, levels = rev(vars))
  )
  df$value <- as.vector(cor_mat[, rev(vars)])

  ggplot(df, aes(x, y, fill = value)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = round(value, 2)), size = 3) +
    scale_fill_gradientn(colours = colors, limits = c(-1, 1)) +
    coord_fixed() +
    labs(title = "Correlation matrix", x = NULL, y = NULL, fill = "r") +
    theme_ekio(grid = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
