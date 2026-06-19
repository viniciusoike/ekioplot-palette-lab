function(request) page_sidebar(
  theme = bs_theme(
    preset = "shiny",
    primary = "#1E3A5F",
    "font-family-base" = "-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif"
  ),

  title = tags$div(
    class = "d-flex align-items-center justify-content-between w-100",
    tags$div(
      tags$span("EKIO Palette Lab", style = "font-weight: 700; font-size: 20px;"),
      tags$span(
        " — compare palettes across chart types",
        class = "text-secondary", style = "font-size: 13px;"
      )
    )
  ),

  sidebar = sidebar(
    width = 320,
    tags$head(
      tags$style(HTML(app_css)),
      tags$script(src = "sortable.min.js")
    ),
    tags$script(HTML(app_js)),

    # ---- Palette editor ----
    selectInput(
      "preset", "Load palette",
      choices = c("(custom)" = "", palette_choices)
    ),
    numericInput("n_colors", "Number of colors", value = 6, min = 2, max = 12),
    textInput("palette_name", "Name", value = "contrast"),
    uiOutput("color_inputs"),

    hr(),

    # ---- Highlight ----
    tags$h6("Highlight colors", style = "font-weight: 600;"),
    colourInput("highlight_color", "Highlight", value = default_colors[1]),
    colourInput(
      "non_highlight_color", "Non-highlight",
      value = default_colors[length(default_colors)]
    ),

    hr(),

    # ---- Export ----
    tags$h6("Export", style = "font-weight: 600;"),
    verbatimTextOutput("export_code"),
    actionButton(
      "copy_btn", "Copy to clipboard",
      class = "btn-primary btn-sm", style = "width: 100%;"
    ),
    actionButton(
      "bookmark_btn", "Copy share link",
      icon = icon("link"),
      class = "btn-outline-secondary btn-sm mt-2", style = "width: 100%;"
    )
  ),

  # ---- Main content ----

  # Palette preview card
  card(
    card_header(
      class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
      "Palette preview",
      tags$div(
        class = "d-flex gap-2 align-items-center flex-wrap",
        uiOutput("ab_toggle"),
        selectInput(
          "cvd_view", NULL,
          choices = c(
            "Normal vision" = "none",
            "Deuteranopia" = "deutan",
            "Protanopia" = "protan",
            "Tritanopia" = "tritan"
          ),
          width = "160px", selectize = FALSE
        ),
        input_dark_mode(id = "dark_mode"),
        actionButton(
          "pin_btn", "Pin palette",
          class = "btn-sm btn-outline-primary"
        )
      )
    ),
    card_body(
      uiOutput("palette_strip"),
      tags$small(
        class = "text-secondary",
        "Drag swatches to reorder · click a swatch to set it as the highlight color"
      ),
      uiOutput("pinned_strip"),
      uiOutput("history_strip"),
      accordion(
        open = FALSE, class = "mt-2",
        accordion_panel(
          "Distance & contrast",
          uiOutput("distance_summary"),
          uiOutput("contrast_check")
        ),
        accordion_panel(
          "Color vision simulation",
          uiOutput("cvd_strips")
        )
      )
    )
  ),

  # Plot tabs
  navset_card_tab(
    id = "plot_tabs",

    nav_panel(
      "Overview",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("all_area", height = "300px"),
        plotOutput("all_line", height = "300px"),
        plotOutput("all_bar", height = "300px"),
        plotOutput("all_scatter", height = "300px")
      )
    ),

    nav_panel(
      "Area",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("area_stacked", height = "400px"),
        plotOutput("area_share", height = "400px")
      )
    ),

    nav_panel(
      "Line",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("line_labeled", height = "400px"),
        plotOutput("line_faceted", height = "400px")
      ),
      plotOutput("line_single", height = "400px"),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("line_trend", height = "400px"),
        plotOutput("line_trend_dots", height = "400px")
      )
    ),

    nav_panel(
      "Bar",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("bar_vertical", height = "400px"),
        plotOutput("bar_horizontal", height = "400px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("bar_labels_inside", height = "400px"),
        plotOutput("bar_highlighted", height = "400px")
      ),
      plotOutput("bar_highlighted_top", height = "400px")
    ),

    nav_panel(
      "Scatter",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("scatter_single", height = "400px"),
        plotOutput("scatter_grouped", height = "400px")
      )
    ),

    nav_panel(
      "Histogram",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("hist_plot", height = "400px"),
        plotOutput("hist_fine", height = "400px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("hist_density", height = "400px"),
        plotOutput("hist_by_cut", height = "400px")
      )
    ),

    nav_panel(
      "Continuous",
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("heatmap_plot", height = "400px"),
        plotOutput("gradient_scatter", height = "400px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("diverging_bar", height = "500px"),
        plotOutput("correlation_plot", height = "500px")
      )
    ),

    nav_panel(
      "Complex",
      plotOutput("bubble_plot", height = "600px"),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput("bump_plot", height = "600px"),
        plotOutput("pyramid_plot", height = "500px")
      )
    )
  )
)
