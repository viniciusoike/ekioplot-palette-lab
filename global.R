# Sourced automatically by Shiny at app startup, before ui.R and server.R.
# Launch from the repo root with shiny::runApp(".").

# ---- Dependency check ----
# Fail early with a clear message
# rather than an opaque error deep in a plot.
.app_pkgs <- c(
  "shiny", "bslib", "ggplot2", "dplyr", "forcats", "stringr", "tibble",
  "ggbump", "patchwork", "colorspace", "colourpicker"
)
.missing <- .app_pkgs[!vapply(
  .app_pkgs, requireNamespace, logical(1), quietly = TRUE
)]
if (length(.missing) > 0) {
  rlang::check_installed(.missing, "to run the EKIO Palette Lab Shiny app.")
}

library(ekioplot)
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(forcats)
library(stringr)
library(tibble)
library(ggbump)
library(patchwork)
library(colorspace)
library(colourpicker)

# Shareable palette URLs
enableBookmarking("url")

source("utils.R")
source("plot_funs.R")

# ---- Precomputed datasets ----
# Built by data-raw/shiny-app-data.R; bundled so the app launches without the
# heavier build-time deps (zoo, trendseries, wpp2024, tidyr).
.app_data <- readRDS("app_data.rds")
subfuels <- .app_data$subfuels
total_fuel <- .app_data$total_fuel
co2_data <- .app_data$co2_data
diamond_cuts <- .app_data$diamond_cuts
scatter_df <- .app_data$scatter_df
bubble_data <- .app_data$bubble_data
bump_list <- .app_data$bump_list
pyramid_data <- .app_data$pyramid_data
