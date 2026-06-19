# Load required libraries
library(ggplot2)
library(dplyr)
library(ragg)
library(patchwork)

# fmt: skip
job_drivers_data <- tibble(
  # Job factors
  job_factor = rep(
    c("Remuneration and additional benefits",
      "Further training and development opportunities",
      "Flexible working hours, remote or hybrid working",
      "Relationships with managers",
      "Work-life balance",
      "Meaningfulness of the work",
      "Reputation of the employer",
      "Relationships with colleagues (sense of belonging)",
      "Job security"),
    each = 7),
  # Country
  country = rep(
    c("Spain", "Germany", "Italy", "Poland", "France", "UK", "Average"), 9
  ),
  # Values inside bubbles
  percentage = c(
    # Remuneration and additional benefits
    41, 42, 37, 38, 38, 25, 38,
    # Further training and development opportunities
    33, 29, 30, 30, 24, 22, 28,
    # Flexible working hours, remote or hybrid working
    26, 26, 27, 31, 24, 26, 27,
    # Relationships with managers
    26, 30, 24, 24, 32, 22, 26,
    # Work-life balance
    23, 26, 26, 25, 23, 25, 25,
    # Meaningfulness of the work
    20, 26, 22, 25, 18, 21, 23,
    # Reputation of the employer
    22, 20, 18, 16, 24, 18, 19,
    # Relationships with colleagues (sense of belonging)
    23, 17, 20, 19, 21, 19, 19,
    # Job security
    25, 19, 18, 14, 13, 23, 18
  )
)

job_drivers_data <- job_drivers_data |>
  mutate(
    age_group = case_when(
      percentage < 25 ~ "<25",
      percentage < 30 ~ "25-29",
      TRUE ~ "≥30"
    ),
    age_group = factor(age_group, levels = c("<25", "25-29", "≥30")),
    job_factor = factor(
      job_factor,
      levels = rev(c(
        "Remuneration and additional benefits",
        "Further training and development opportunities",
        "Flexible working hours, remote or hybrid working",
        "Relationships with managers",
        "Work-life balance",
        "Meaningfulness of the work",
        "Reputation of the employer",
        "Relationships with colleagues (sense of belonging)",
        "Job security"
      ))
    ),
    # fmt: skip
    country = factor(
      country,
      levels = c(
        "Spain", "Germany", "Italy", "Poland", "France", "UK", "Average")
    ),
    text_color = if_else(percentage >= 25, "white", "black")
  )


# Define colors matching the original chart
bubble_colors <- c("#01a9f4", "#2151fe", "#071f79")

job_drivers_data <- job_drivers_data |>
  arrange(desc(job_factor), country)

# Create the bubble chart
base_plot <- ggplot(job_drivers_data, aes(x = country, y = job_factor)) +
  geom_point(
    aes(size = percentage, color = age_group),
    stroke = 0.5
  ) +
  # Add vertical line before "Average" column
  geom_vline(xintercept = 6.465, color = "grey80", linewidth = 0.25) +
  scale_x_discrete(position = "top") +

  # Scale for bubble sizes - adjusted to match the visual appearance
  scale_size_continuous(
    range = c(9, 14),
    breaks = c(15, 25, 35, 45),
    labels = c("15%", "25%", "35%", "45%"),
    name = "% of employees"
  ) +

  # Color scale
  scale_color_manual(
    values = bubble_colors,
    labels = c("<25", "25 to 29", "≥30")
  ) +

  # Add percentage labels inside bubbles
  geom_text(
    aes(label = percentage),
    color = job_drivers_data$text_color,
    size = 3,
    family = "Roboto Light"
  ) +
  guides(color = guide_legend(), size = "none") +
  # Theming to match McKinsey style
  theme_minimal(base_family = "Helvetica Neue Light") +
  theme(
    # Remove grid lines
    panel.grid = element_blank(),
    # Axis styling
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black", hjust = 1),

    # Legend styling
    legend.position = "inside",
    legend.position.inside = c(0.8, 1.1),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 9, color = "black"),
    legend.key.size = unit(0.25, "lines"),

    # Plot margins
    plot.margin = margin(10, 5, 10, 5),

    # Background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    aspect.ratio = 1
  )

theme_annotation <- theme(
  plot.title = element_text(
    family = "Arial",
    size = 14,
    color = "black",
    margin = margin(b = 5)
  ),
  plot.subtitle = element_text(
    size = 10,
    color = "grey30"
  ),
  plot.caption = element_text(
    size = 7.5,
    color = "grey50",
    hjust = 0,
    margin = margin(t = 20),
    lineheight = 0.9
  )
)

final_plot <- base_plot +
  plot_annotation(
    title = "According to employees, remuneration and additional benefits are the most\nimportant factors driving a job change.",
    subtitle = "Job change drivers,¹ % of employees naming factor as a top-three driver",
    caption = "¹Question was not included in US survey.\nSource: McKinsey HR Monitor Survey, Dec 2024, n = 3,000 employees and 1,500 HR professionals in France, Germany, Italy, Poland, Spain, and UK\n\nMcKinsey & Company",
    theme = theme_annotation
  )

final_plot

library(ggbump)

# fmt: skip
ranking <- structure(list(country = c("Australia", "Austria", "Belgium",
"Bulgaria", "Canada", "Switzerland", "Chile", "Colombia", "Costa Rica",
"Czechia", "Germany", "Denmark", "Spain", "Estonia", "Finland",
"France", "United Kingdom", "Greece", "Croatia", "Hungary", "Ireland",
"Iceland", "Israel", "Italy", "Japan", "Korea, Rep.", "Lithuania",
"Luxembourg", "Latvia", "Mexico", "Netherlands", "Norway", "New Zealand",
"Poland", "Portugal", "Romania", "Russian Federation", "Slovak Republic",
"Slovenia", "Sweden", "United States", "South Africa"), year = c(2022,
2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022,
2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022,
2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022,
2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022), gdp_over_pop = c(64491.4298860631,
52131.4466586387, 49582.8253649187, 13772.476794451, 54966.4888361088,
92101.4697414382, 15355.5064965406, 6630.28272608053, 13198.8217168097,
27638.3732756288, 48432.4558732596, 66983.1320017139, 29350.1685214481,
28332.6290917984, 50536.6244670542, 40963.837337041, 45850.426122263,
20732.0522190537, 18413.2346820557, 18463.2085249699, 104038.946076296,
72902.97995385, 54659.753964319, 34157.9901224641, 33815.3172733799,
32254.6247153457, 24826.7910371972, 126426.089934322, 21851.1052012696,
11091.3054302537, 55985.4034255707, 106148.778627718, 48249.2640150011,
18321.2808899532, 24274.5165823077, 15892.1185256988, 15606.638235363,
21258.1141354655, 29457.40284439, 55873.2208108977, 76398.5917422054,
6776.48007742932), gdp_ppp_over_pop = c(62625.3576423649, 67935.8479736374,
65027.2948954044, 33582.2826109195, 58399.5454813184, 83598.453356565,
30208.805531053, 20287.4002631683, 24922.6592740839, 49945.5001044375,
63149.5986898097, 74005.4785197575, 45825.1956330087, 46697.3597360426,
59026.7073352151, 55492.5655467572, 54602.544380158, 36834.8710885702,
40379.5724355215, 41906.6555658699, 126905.198534476, 69081.261667188,
49509.1289935125, 51864.9777354822, 45572.7238228213, 50069.8233877974,
48396.6935449884, 142213.851685246, 39956.1904770153, 21512.2695445099,
69577.4045796773, 114898.759885569, 51966.8625777625, 43268.5437126031,
41451.6148656947, 41887.9219020734, 37106.525814966, 37459.4738440687,
50031.6561840509, 64578.3963325427, 76398.5917422057, 15904.848362637
), gdp_ppp_over_k_hours_worked = c(68842.8578770362, 91129.2343497908,
97699.4361925451, 38298.6642006748, 67624.9962542074, 92828.5718370691,
34123.8828194073, 19857.9394155263, 28210.1909229472, 54991.5022712458,
86931.1611152649, 100679.655123606, 64710.3103325666, 53091.3785704289,
79292.8909173707, 83995.3349820628, 72912.5318963336, 41472.9601288999,
48823.7808806988, 49766.5917825825, 152929.632511973, 86646.2401154742,
55761.7169222038, 70532.9428951858, 51945.5193345165, 48543.7626350804,
58092.5477859096, 124998.415854506, 55729.1614883397, 21626.7272331265,
85073.3826252786, 149860.689464332, 54319.2468894888, 53044.9854754838,
51956.2719721076, 50849.7492667392, 37685.964413215, 51678.5045844961,
60417.3384981245, 90468.8643479866, 89915.5329130428, 29282.4156535399
)), row.names = c(NA, -42L), spec = structure(list(cols = list(
    country = structure(list(), class = c("collector_character",
    "collector")), year = structure(list(), class = c("collector_double",
    "collector")), gdp_over_pop = structure(list(), class = c("collector_double",
    "collector")), gdp_ppp_over_pop = structure(list(), class = c("collector_double",
    "collector")), gdp_ppp_over_k_hours_worked = structure(list(), class = c("collector_double",
    "collector"))), default = structure(list(), class = c("collector_guess",
"collector")), delim = ","), class = "col_spec"), class = c("spec_tbl_df",
"tbl_df", "tbl", "data.frame"))

ranking <- ranking |>
  filter(year == max(year)) |>
  pivot_longer(cols = -c(country, year), names_to = "measure") |>
  mutate(rank = rank(-value), .by = "measure")

ranking <- ranking |>
  mutate(
    highlight = if_else(country %in% countries_sel, country, ""),
    highlight = factor(highlight, levels = c(countries_sel, "")),
    is_highlight = factor(if_else(country %in% countries_sel, 1L, 0L)),
    rank_labels = if_else(rank %in% c(1, 5, 10, 15, 20), rank, NA),
    rank_labels = stringr::str_replace(rank_labels, "^1$", "1st"),
    measure = factor(measure, levels = measures)
  )

countries_sel <- c("Norway", "Belgium", "Austria", "United States", "Germany")
measures <- c("gdp_over_pop", "gdp_ppp_over_pop", "gdp_ppp_over_k_hours_worked")

cores <- c("#101010", "#f7443e", "#8db0cc", "#fa9494", "#225d9f", "#c7c7c7")

df_gdp <- tibble(
  measure = measures,
  measure_label = c(
    "GDP per person at market rates",
    "Adjusted for cost differences*",
    "Adjusted for costs and hours worked"
  ),
  position = -1.5
)

df_gdp <- df_gdp |>
  mutate(
    measure = factor(measure, levels = measures),
    measure_label = stringr::str_wrap(measure_label, width = 12),
    measure_label = paste0("  ", measure_label)
  )

plot_final <- ggplot(ranking, aes(measure, rank, group = country)) +
  geom_bump(aes(color = highlight, linewidth = is_highlight)) +
  geom_point(shape = 21, color = "white", aes(fill = highlight), size = 3) +
  geom_text(
    data = filter(ranking, measure == measures[[3]], is_highlight != 1L),
    aes(x = measure, y = rank, label = country),
    nudge_x = 0.05,
    hjust = 0,
    family = "Lato",
    size = 3
  ) +
  geom_text(
    data = filter(ranking, measure == measures[[3]], is_highlight == 1L),
    aes(x = measure, y = rank, label = country),
    nudge_x = 0.05,
    hjust = 0,
    family = "Lato",
    fontface = "bold",
    size = 3
  ) +
  geom_text(
    data = filter(ranking, measure == measures[[1]]),
    aes(x = measure, y = rank, label = rank_labels),
    nudge_x = -0.15,
    hjust = 0,
    family = "Lato",
    size = 3
  ) +
  geom_text(
    data = df_gdp,
    aes(x = measure, y = position, label = measure_label),
    inherit.aes = FALSE,
    hjust = 0,
    family = "Lato",
    fontface = "bold",
    size = 3
  ) +
  annotate("text", x = 1, y = -2.5, label = expression("\u2193")) +
  annotate("text", x = 2, y = -2.5, label = expression("\u2193")) +
  annotate("text", x = 3, y = -2.5, label = expression("\u2193")) +
  coord_cartesian(ylim = c(21, -2)) +
  scale_color_manual(values = cores) +
  scale_fill_manual(values = cores) +
  scale_linewidth_manual(values = c(0.5, 1.2)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_blank()
  )


library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(wpp2024)
library(showtext)
library(patchwork)
data("popAge5dt")

# Brazil, Ireland, Pakistan, Japan
countries <- tibble(
  country_code = c(392, 586, 372, 76),
  name = c("Japan", "Pakistan", "Ireland", "Brazil")
)

popage <- popAge5dt |>
  filter(country_code %in% countries$country_code) |>
  mutate(
    age_trunc = if_else(
      age %in% c("85-89", "90-94", "95-99", "100+"),
      "85-",
      age
    ),
    age_min = as.integer(str_extract(age_trunc, "[0-9]{1,2}(?=-)")),
    age_trunc = str_replace(age_trunc, "85-", "85+"),
    age_trunc = factor(age_trunc),
    age_trunc = forcats::fct_reorder(age_trunc, age_min)
  ) |>
  group_by(country_code, name, year, age_trunc) |>
  summarise(
    pop_male = sum(popM),
    pop_female = sum(popF)
  ) |>
  ungroup()

popage <- popage |>
  select(country_code, name, year, age_trunc, pop_male, pop_female) |>
  pivot_longer(
    cols = starts_with("pop"),
    names_to = "sex",
    values_to = "population"
  )

popage <- popage |>
  mutate(
    sex = if_else(sex == "pop_male", "Male", "Female"),
    sex = factor(sex, levels = c("Male", "Female")),
  ) |>
  group_by(country_code, name, year) |>
  mutate(share = population / sum(population, na.rm = TRUE) * 100) |>
  ungroup() |>
  mutate(share = if_else(sex == "Male", -share, share))

dat <- filter(popage, name == "Brazil")

breaks_share <- seq(-10, 10, 2)
labels_share <- str_remove(as.character(breaks_share), "-")

colors <- c("#1B9E77", "#7570B3")
font <- "Lato"


p1 <- ggplot(
  data = filter(dat, sex == "Male", year %in% c(1970, 2000)),
  aes(share, age_trunc, alpha = as.factor(year))
) +
  geom_col(position = position_dodge(), fill = colors[1]) +
  scale_alpha_manual(
    name = "",
    values = c(0.5, 1),
    labels = c("Male (1970)", "Male (2000)")
  ) +
  scale_x_continuous(
    breaks = breaks_share,
    labels = labels_share,
    limits = c(-8, 0),
    expand = expansion(mult = c(0.05, 0))
  ) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_minimal(base_size = 12, base_family = font) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = 2),
    axis.text.y = element_blank()
  )

p2 <- ggplot(
  data = filter(dat, sex == "Female", year %in% c(1970, 2000)),
  aes(share, age_trunc, alpha = as.factor(year))
) +
  geom_col(position = position_dodge(), fill = colors[2]) +
  scale_alpha_manual(
    name = "",
    values = c(0.5, 1),
    labels = c("Female (1970)", "Female (2000)")
  ) +
  scale_x_continuous(
    breaks = breaks_share,
    labels = labels_share,
    limits = c(0, 8),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_minimal(base_size = 12, base_family = font) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = 2),
    axis.text.y = element_text(hjust = 0.5)
  )

panel <- (p1 | p2) & theme(legend.position = "bottom")
panel <- panel + plot_layout(guides = "collect")

panel
