library(ekioplot)
library(ggplot2)
library(dplyr)
library(trendseries)

# Area plot ---------------------------------------------------------------

subfuels <- fuels |>
  filter(year >= 1920) |>
  mutate(
    label_text = stringr::str_glue(
      "{fuel}\n{round(consumption_gwh/1000, 1)}GWh"
    ),
    share = consumption_gwh / sum(consumption_gwh) * 100,
    .by = "year"
  )

ggplot(subfuels, aes(year, consumption_gwh, fill = fuel)) +
  geom_area() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::label_number(scale = 1e-3)
  ) +
  scale_fill_ekio_d(palette = "cool") +
  theme_ekio() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.25)
  )

ggplot(subfuels, aes(year, consumption_gwh, fill = fuel)) +
  geom_area() +
  geom_text(
    data = subset(subfuels, year == max(subfuels$year)),
    aes(x = year - 10, y = consumption_gwh, label = label_text),
    hjust = 0,
    size = 3.5,
    family = "Lato",
    color = "#ffffff",
    position = position_stack(vjust = 0.5)
  ) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::label_number(scale = 1e-3)
  ) +
  scale_fill_ekio_d(palette = "cool") +
  theme_ekio() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.25)
  )


ggplot(subfuels, aes(year, share, fill = fuel)) +
  geom_area() +
  geom_text(
    data = subset(subfuels, year == max(subfuels$year)),
    aes(
      x = year - 15,
      y = share,
      label = scales::number(
        share,
        accuracy = 0.1,
        suffix = "%",
        decimal.mark = ","
      )
    ),
    hjust = 0,
    size = 3.5,
    family = "Lato",
    color = "#ffffff",
    position = position_stack(vjust = 0.5)
  ) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::label_percent(scale = 1),
    position = "right"
  ) +
  scale_fill_ekio_d(palette = "cool") +
  theme_ekio() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.25)
  )


# Line plot --------------------------------------------------------------

ggplot(subfuels, aes(year, consumption_gwh, color = fuel)) +
  geom_line(lwd = 0.7) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::label_number(scale = 1e-3)
  ) +
  scale_color_ekio_d(
    name = "Fuel",
    labels = \(x) stringr::str_to_title(x),
    palette = "cool"
  ) +
  labs(
    title = "Fossil fuel consumption over the years",
    subtitle = "Consumption (GWh) of main fossil fuels in the World (1920-2022)",
    x = NULL,
    y = "GWh (thous.)",
    caption = "Source: OWID"
  ) +
  theme_ekio() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.25)
  )

ggplot(subfuels, aes(year, consumption_gwh, color = fuel)) +
  geom_line(lwd = 0.7) +
  geom_label(
    data = subset(subfuels, year == max(subfuels$year)),
    aes(label = label_text),
    hjust = 0,
    size = 2.5,
    family = "Lato",
    nudge_x = 2.55
  ) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05)),
    limits = c(NA, 2040)
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, 10) * 1e3,
    labels = scales::label_number(scale = 1e-3),
    limits = c(NA, 59000),
    expand = expansion(mult = c(0, 0.05)),
  ) +
  guides(color = guide_legend()) +
  scale_color_ekio_d(palette = "cool") +
  theme_ekio() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.25),
    legend.position = "none"
  )


dat_co2 <- data.frame(
  date = zoo::as.Date.ts(time(co2)),
  co2 = zoo::coredata(co2)
)

dat_co2 <- as_tibble(dat_co2)

dat_co2 <- dat_co2 |>
  augment_trends(
    value_col = "co2",
    params = list(s.window = 51)
  )

ggplot(dat_co2, aes(date)) +
  geom_line(
    aes(y = co2),
    color = "#1E3A5F",
    alpha = 0.5,
    lwd = 0.5
  ) +
  geom_line(
    aes(y = trend_stl),
    color = "#1E3A5F",
    lwd = 0.8
  ) +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
  theme_ekio() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.25),
    axis.line.x = element_line(linewidth = 0.25)
  )


ggplot(subset(dat_co2, date >= "1980-01-01"), aes(date)) +
  geom_point(
    aes(y = co2),
    color = "#1E3A5F",
    alpha = 0.3,
    size = 1
  ) +
  geom_line(
    aes(y = trend_stl),
    color = "#1E3A5F",
    lwd = 0.8
  ) +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
  theme_ekio() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.25),
    axis.line.x = element_line(linewidth = 0.25)
  )

ggplot(subset(dat_co2, date >= "1980-01-01"), aes(date)) +
  geom_line(
    aes(y = trend_stl),
    color = "#1E3A5F",
    lwd = 0.8
  ) +
  theme_ekio() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.25),
    axis.line.x = element_line(linewidth = 0.25)
  )

ggplot(subfuels, aes(year, consumption_gwh)) +
  geom_line(lwd = 0.7, color = "#1E3A5F") +
  facet_wrap(vars(fuel), nrow = 3) +
  guides(color = "none") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::label_number(scale = 1e-3)
  ) +
  theme_ekio() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.25),
    axis.line.x = element_line(linewidth = 0.25),
  )

ggplot(subfuels, aes(year, consumption_gwh)) +
  geom_line(lwd = 0.7, color = "#1E3A5F") +
  facet_wrap(vars(fuel), nrow = 1) +
  guides(color = "none") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::label_number(scale = 1e-3)
  ) +
  theme_ekio() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.25),
    axis.line.x = element_line(linewidth = 0.25),
  )

# Bar plot ----------------------------------------------------------------
tbl <- diamonds |>
  count(cut) |>
  mutate(
    share = n / sum(n) * 100,
    label = format(round(share, 1)),
    label = if_else(nchar(label) == 1, paste0(label, ".0"), label),
    label = paste0(label, "%"),
    cut = forcats::fct_reorder(cut, share),
    highlight = factor(if_else(cut == "Ideal", 1L, 0L)),
    ytext = if_else(cut == "Fair", share + 1.5, share - 1.5),
    coltext = factor(if_else(cut == "Fair", 1L, 0L))
  )

sub <- filter(tbl, cut != "Fair")

ggplot(tbl, aes(x = cut, y = share)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0) +
  geom_text(aes(y = share + 2.5, label = label)) +
  theme_ekio()

ggplot(tbl, aes(share, cut)) +
  geom_col() +
  geom_text(
    aes(x = share, label = label),
    nudge_x = 5,
    hjust = 1
  ) +
  theme_ekio() +
  theme(
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_blank()
  )

ggplot(sub, aes(x = cut, y = share)) +
  geom_col() +
  geom_text(aes(y = 2, label = label), color = "white") +
  theme_ekio()

ggplot(sub, aes(x = cut, y = share)) +
  geom_col() +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white"
  ) +
  theme_ekio()

ggplot(sub, aes(x = cut, y = share)) +
  geom_col() +
  geom_text(
    aes(y = share, label = label),
    nudge_y = -2,
    color = "white"
  ) +
  theme_ekio()

ggplot(sub, aes(x = cut)) +
  geom_col(
    aes(y = share),
    fill = "#1E3A5F",
    width = 0.5
  ) +
  geom_text(
    aes(x = cut, y = 0, label = cut),
    hjust = 0,
    nudge_x = 0.4,
    size = 3,
    family = "Lato"
  ) +
  geom_text(
    aes(x = cut, y = ytext, label = label),
    fontface = "bold",
    size = 3,
    family = "Lato",
    color = "white"
  ) +
  scale_y_continuous(
    name = "Proportion",
    breaks = seq(0, 40, 10),
    labels = \(x) paste0(x, "%"),
    limits = c(0, 40),
    expand = c(0, 0)
  ) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_ekio() +
  theme(
    axis.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggplot(tbl, aes(x = cut)) +
  geom_col(aes(y = share, fill = highlight), width = 0.5) +
  geom_text(
    aes(x = cut, y = 0, label = cut),
    hjust = 0,
    nudge_x = 0.4,
    family = "Lato",
    size = 3
  ) +
  geom_text(
    aes(x = cut, y = ytext, label = label, color = coltext),
    fontface = "bold",
    size = 3,
    family = "Lato"
  ) +
  scale_y_continuous(
    name = "Proportion",
    breaks = seq(0, 40, 10),
    labels = \(x) paste0(x, "%"),
    limits = c(0, 40),
    expand = c(0, 0)
  ) +
  labs(x = "") +
  scale_fill_manual(values = c("gray45", "#2a9d8f")) +
  scale_color_manual(values = c("#ffffff", "#000000")) +
  guides(fill = "none", color = "none") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "gray80"),
    axis.ticks.x = element_line(color = "gray80"),
    plot.margin = margin(0.5, 0.75, 0.5, 0, unit = "cm")
  )

ggplot(subset(tbl, cut != "Fair"), aes(x = cut)) +
  geom_col(aes(y = share, fill = highlight), width = 0.8) +
  geom_text(
    aes(x = cut, y = 0.5, label = cut),
    hjust = 0,
    size = 3,
    family = "Lato"
  ) +
  geom_text(
    aes(x = cut, y = ytext, label = label, color = coltext),
    fontface = "bold",
    size = 3,
    family = "Lato"
  ) +
  scale_y_continuous(
    name = "Proportion",
    breaks = seq(0, 40, 10),
    labels = \(x) paste0(x, "%"),
    limits = c(0, 40),
    expand = c(0, 0)
  ) +
  labs(x = "") +
  scale_fill_manual(values = c("gray45", "#2a9d8f")) +
  scale_color_manual(values = c("#ffffff", "#000000")) +
  guides(fill = "none", color = "none") +
  coord_flip() +
  theme_ekio(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(color = "gray80"),
    axis.ticks.x = element_line(color = "gray80"),
    plot.margin = margin(0.5, 0.75, 0.5, 0, unit = "cm")
  )

# Histogram --------------------------------------------------------------

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(
    bins = 15,
    fill = "#2B4C7E",
    color = "#ffffff",
    lwd = 0.25
  ) +
  geom_hline(yintercept = 0) +
  # scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Carat", y = "Count") +
  theme_ekio()

# Scatter ----------------------------------------------------------------

ggplot(mtcars, aes(mpg, wt)) +
  geom_point(color = "#2B4C7E", size = 2) +
  theme_ekio() +
  theme(
    panel.grid.major.x = element_line()
  )

ggplot(mtcars, aes(mpg, wt, fill = factor(cyl))) +
  geom_point(shape = 21, color = "#ffffff", size = 3) +
  scale_fill_manual(values = c("#1E3A5F", "#DD6B20", "#2C7A7B")) +
  theme_ekio() +
  theme(
    panel.grid.major.x = element_line()
  )

# Heatmap ----------------------------------------------------------------

# Choropleth -------------------------------------------------------------

# Others -----------------------------------------------------------------

## Boxplot

## Ridgeplot

##
