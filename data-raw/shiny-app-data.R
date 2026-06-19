# Rebuild the precomputed datasets bundled with the Palette Lab Shiny app.
#
# The app ships these as inst/shiny-app/app_data.rds so it can launch without
# the heavier build-time dependencies (zoo, trendseries, wpp2024, tidyr) and
# without re-running an STL decomposition / population aggregation each start.
#
# Run with: source("data-raw/shiny-app-data.R")
# Requires (build-time only): ekioplot, ggplot2, dplyr, tidyr, forcats,
#   stringr, tibble, zoo, trendseries, wpp2024.

library(ekioplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(tibble)

# ---- Data builders ----

build_fuel_data <- function() {
  base <- fuels |>
    filter(year >= 1920)

  wind <- tibble(
    entity = "World",
    fuel = "wind",
    year = 1985:2023,
    consumption_gwh = round(5 * exp(0.19 * (1985:2023 - 1985)))
  )

  solar <- tibble(
    entity = "World",
    fuel = "solar",
    year = 2000:2023,
    consumption_gwh = round(3 * exp(0.32 * (2000:2023 - 2000)))
  )

  bind_rows(base, wind, solar) |>
    mutate(
      fuel = factor(fuel, levels = c("coal", "oil", "gas", "wind", "solar"))
    ) |>
    mutate(share = consumption_gwh / sum(consumption_gwh) * 100, .by = year)
}

build_total_fuel <- function(df) {
  df |>
    summarise(total = sum(consumption_gwh), .by = year)
}

build_diamond_data <- function() {
  diamonds |>
    count(cut, name = "n") |>
    mutate(
      share = n / sum(n) * 100,
      label = paste0(formatC(round(share, 1), format = "f", digits = 1), "%"),
      cut = fct_reorder(cut, share)
    )
}

build_scatter_data <- function() {
  mtcars |>
    select(mpg, wt, cyl, hp) |>
    mutate(cyl = factor(cyl))
}

build_co2_data <- function() {
  data.frame(
    date = zoo::as.Date.ts(time(co2)),
    co2 = zoo::coredata(co2)
  ) |>
    as_tibble() |>
    trendseries::augment_trends(
      value_col = "co2",
      params = list(s.window = 51)
    )
}

# nolint start
build_bubble_data <- function() {
  tibble(
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
      each = 7
    ),
    country = rep(
      c("Spain", "Germany", "Italy", "Poland", "France", "UK", "Average"), 9
    ),
    percentage = c(
      41, 42, 37, 38, 38, 25, 38,
      33, 29, 30, 30, 24, 22, 28,
      26, 26, 27, 31, 24, 26, 27,
      26, 30, 24, 24, 32, 22, 26,
      23, 26, 26, 25, 23, 25, 25,
      20, 26, 22, 25, 18, 21, 23,
      22, 20, 18, 16, 24, 18, 19,
      23, 17, 20, 19, 21, 19, 19,
      25, 19, 18, 14, 13, 23, 18
    )
  ) |>
    mutate(
      job_factor = factor(job_factor, levels = rev(c(
        "Remuneration and additional benefits",
        "Further training and development opportunities",
        "Flexible working hours, remote or hybrid working",
        "Relationships with managers",
        "Work-life balance",
        "Meaningfulness of the work",
        "Reputation of the employer",
        "Relationships with colleagues (sense of belonging)",
        "Job security"
      ))),
      country = factor(
        country,
        levels = c("Spain", "Germany", "Italy", "Poland", "France", "UK", "Average")
      )
    ) |>
    arrange(desc(job_factor), country)
}
# nolint end

build_bump_data <- function() {
  measures <- c("gdp_over_pop", "gdp_ppp_over_pop", "gdp_ppp_over_k_hours_worked")
  countries_sel <- c("Norway", "Belgium", "Austria", "United States", "Germany")

  # nolint start
  raw <- structure(list(country = c("Australia", "Austria", "Belgium",
    "Bulgaria", "Canada", "Switzerland", "Chile", "Colombia", "Costa Rica",
    "Czechia", "Germany", "Denmark", "Spain", "Estonia", "Finland",
    "France", "United Kingdom", "Greece", "Croatia", "Hungary", "Ireland",
    "Iceland", "Israel", "Italy", "Japan", "Korea, Rep.", "Lithuania",
    "Luxembourg", "Latvia", "Mexico", "Netherlands", "Norway", "New Zealand",
    "Poland", "Portugal", "Romania", "Russian Federation", "Slovak Republic",
    "Slovenia", "Sweden", "United States", "South Africa"), year = rep(2022, 42),
    gdp_over_pop = c(64491.43, 52131.45, 49582.83, 13772.48, 54966.49,
      92101.47, 15355.51, 6630.28, 13198.82, 27638.37, 48432.46,
      66983.13, 29350.17, 28332.63, 50536.62, 40963.84, 45850.43,
      20732.05, 18413.23, 18463.21, 104038.95, 72902.98, 54659.75,
      34157.99, 33815.32, 32254.62, 24826.79, 126426.09, 21851.11,
      11091.31, 55985.40, 106148.78, 48249.26, 18321.28, 24274.52,
      15892.12, 15606.64, 21258.11, 29457.40, 55873.22, 76398.59, 6776.48),
    gdp_ppp_over_pop = c(62625.36, 67935.85, 65027.29, 33582.28, 58399.55,
      83598.45, 30208.81, 20287.40, 24922.66, 49945.50, 63149.60,
      74005.48, 45825.20, 46697.36, 59026.71, 55492.57, 54602.54,
      36834.87, 40379.57, 41906.66, 126905.20, 69081.26, 49509.13,
      51864.98, 45572.72, 50069.82, 48396.69, 142213.85, 39956.19,
      21512.27, 69577.40, 114898.76, 51966.86, 43268.54, 41451.61,
      41887.92, 37106.53, 37459.47, 50031.66, 64578.40, 76398.59, 15904.85),
    gdp_ppp_over_k_hours_worked = c(68842.86, 91129.23, 97699.44, 38298.66,
      67625.00, 92828.57, 34123.88, 19857.94, 28210.19, 54991.50,
      86931.16, 100679.66, 64710.31, 53091.38, 79292.89, 83995.33,
      72912.53, 41472.96, 48823.78, 49766.59, 152929.63, 86646.24,
      55761.72, 70532.94, 51945.52, 48543.76, 58092.55, 124998.42,
      55729.16, 21626.73, 85073.38, 149860.69, 54319.25, 53044.99,
      51956.27, 50849.75, 37685.96, 51678.50, 60417.34, 90468.86,
      89915.53, 29282.42)),
    class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -42L))
  # nolint end

  ranking <- raw |>
    pivot_longer(cols = -c(country, year), names_to = "measure") |>
    mutate(rank = rank(-value), .by = "measure") |>
    mutate(
      highlight = if_else(country %in% countries_sel, country, ""),
      highlight = factor(highlight, levels = c(countries_sel, "")),
      is_highlight = factor(if_else(country %in% countries_sel, 1L, 0L)),
      rank_labels = if_else(
        rank %in% c(1, 5, 10, 15, 20), as.character(rank), NA_character_
      ),
      rank_labels = str_replace(rank_labels, "^1$", "1st"),
      measure = factor(measure, levels = measures)
    )

  df_gdp <- tibble(
    measure = factor(measures, levels = measures),
    measure_label = paste0("  ", str_wrap(c(
      "GDP per person at market rates",
      "Adjusted for cost differences*",
      "Adjusted for costs and hours worked"
    ), width = 12)),
    position = -1.5
  )

  list(
    ranking = ranking,
    df_gdp = df_gdp,
    measures = measures,
    countries_sel = countries_sel
  )
}

build_pyramid_data <- function() {
  data("popAge5dt", package = "wpp2024", envir = environment())

  popAge5dt |>
    filter(country_code == 76) |>
    mutate(
      age_trunc = if_else(
        age %in% c("85-89", "90-94", "95-99", "100+"), "85+", age
      ),
      age_min = as.integer(str_extract(age_trunc, "[0-9]{1,2}(?=[-+])")),
      age_trunc = factor(age_trunc),
      age_trunc = fct_reorder(age_trunc, age_min)
    ) |>
    summarise(
      pop_male = sum(popM),
      pop_female = sum(popF),
      .by = c(country_code, name, year, age_trunc)
    ) |>
    pivot_longer(
      cols = c(pop_male, pop_female),
      names_to = "sex", values_to = "population"
    ) |>
    mutate(
      sex = if_else(sex == "pop_male", "Male", "Female"),
      sex = factor(sex, levels = c("Male", "Female")),
      share = population / sum(population, na.rm = TRUE) * 100,
      share = if_else(sex == "Male", -share, share),
      .by = c(country_code, name, year)
    )
}

# ---- Build and save ----

subfuels <- build_fuel_data()

app_data <- list(
  subfuels = subfuels,
  total_fuel = build_total_fuel(subfuels),
  co2_data = build_co2_data(),
  diamond_cuts = build_diamond_data(),
  scatter_df = build_scatter_data(),
  bubble_data = build_bubble_data(),
  bump_list = build_bump_data(),
  pyramid_data = build_pyramid_data()
)

saveRDS(
  app_data,
  file.path("inst", "shiny-app", "app_data.rds"),
  version = 2
)

cli::cli_alert_success("Wrote inst/shiny-app/app_data.rds")
