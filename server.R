function(input, output, session) {

  # ---- Reactive state ----

  current_colors <- reactiveVal(toupper(unname(default_colors)))
  pinned_colors <- reactiveVal(NULL)
  pinned_name <- reactiveVal(NULL)
  history <- reactiveVal(list())

  push_history <- function(name, colors) {
    h <- history()
    key <- paste(colors, collapse = ",")
    keys <- vapply(h, \(x) paste(x$colors, collapse = ","), character(1))
    if (key %in% keys) return(invisible(NULL))
    h <- c(list(list(name = name, colors = colors)), h)
    history(h[seq_len(min(length(h), 8))])
  }

  # ---- Bookmarking (shareable palette URLs) ----
  # current_colors / pinned state live in reactiveVals, not inputs, so store
  # them explicitly. The per-slot color inputs are excluded — they're rebuilt
  # from current_colors on restore.
  setBookmarkExclude(c(
    paste0("color_", seq_len(12)), "reordered_colors", "swatch_clicked",
    "pin_btn", "copy_btn", "bookmark_btn", "history_select", "ab_view",
    "highlight_color", "non_highlight_color"
  ))

  onBookmark(function(state) {
    state$values$colors <- current_colors()
    state$values$name <- input$palette_name
    state$values$pinned <- pinned_colors()
    state$values$pinned_name <- pinned_name()
  })

  onRestore(function(state) {
    cols <- state$values$colors
    if (is.null(cols)) return(invisible(NULL))
    current_colors(cols)
    updateNumericInput(session, "n_colors", value = length(cols))
    updateTextInput(session, "palette_name", value = state$values$name)
    if (!is.null(state$values$pinned)) {
      pinned_colors(state$values$pinned)
      pinned_name(state$values$pinned_name)
      updateActionButton(session, "pin_btn", label = "Unpin")
    }
  })

  onBookmarked(function(url) {
    updateQueryString(url)
    showNotification(
      "Shareable link is now in the address bar.",
      duration = 4, type = "message"
    )
  })

  observeEvent(input$bookmark_btn, session$doBookmark())

  # ---- Color input UI ----

  output$color_inputs <- renderUI({
    n <- input$n_colors
    req(is.numeric(n), n >= 2)
    cols <- isolate(current_colors())
    lapply(seq_len(n), function(i) {
      colourInput(
        paste0("color_", i), label = NULL,
        value = if (i <= length(cols)) cols[i] else "#718096"
      )
    })
  })

  observeEvent(input$n_colors, {
    n <- input$n_colors
    req(is.numeric(n), n >= 2)
    cols <- current_colors()
    if (length(cols) < n) cols <- c(cols, rep("#718096", n - length(cols)))
    if (length(cols) > n) cols <- cols[seq_len(n)]
    current_colors(cols)
  })

  # One observer per possible slot (n_colors max is 12)
  lapply(seq_len(12), function(i) {
    observeEvent(input[[paste0("color_", i)]], {
      val <- toupper(input[[paste0("color_", i)]])
      req(is_valid_hex(val))
      cols <- current_colors()
      if (i <= length(cols) && !identical(cols[i], val)) {
        cols[i] <- val
        current_colors(cols)
      }
    }, ignoreInit = TRUE)
  })

  # Sync pickers when the palette changes from outside the inputs
  # (preset load, drag reorder, history restore). reactiveVal skips
  # invalidation on identical values, so this cannot loop.
  observeEvent(current_colors(), {
    cols <- current_colors()
    for (i in seq_along(cols)) {
      updateColourInput(session, paste0("color_", i), value = cols[i])
    }
    updateColourInput(session, "highlight_color", value = cols[1])
    updateColourInput(
      session, "non_highlight_color", value = cols[length(cols)]
    )
  })

  # ---- Preset loading ----

  observeEvent(input$preset, {
    req(input$preset != "")
    push_history(input$palette_name, current_colors())
    pal <- toupper(unname(ekio_pal(input$preset)))
    current_colors(pal)
    updateNumericInput(session, "n_colors", value = length(pal))
    updateTextInput(session, "palette_name", value = input$preset)
  })

  # ---- Reorder from drag-and-drop ----

  observeEvent(input$reordered_colors, {
    current_colors(toupper(input$reordered_colors))
  })

  # ---- Swatch click sets highlight color ----

  observeEvent(input$swatch_clicked, {
    updateColourInput(session, "highlight_color", value = input$swatch_clicked)
  })

  # ---- Pin / unpin ----

  observeEvent(input$pin_btn, {
    if (is.null(pinned_colors())) {
      pinned_colors(current_colors())
      pinned_name(input$palette_name)
      push_history(input$palette_name, current_colors())
      updateActionButton(session, "pin_btn", label = "Unpin")
    } else {
      pinned_colors(NULL)
      pinned_name(NULL)
      updateActionButton(session, "pin_btn", label = "Pin palette")
    }
  })

  output$ab_toggle <- renderUI({
    if (is.null(pinned_colors())) return(NULL)
    radioButtons(
      "ab_view", NULL,
      choices = c(Current = "current", Pinned = "pinned"),
      selected = "current", inline = TRUE
    )
  })

  # ---- History restore ----

  observeEvent(input$history_select, {
    idx <- as.integer(input$history_select)
    h <- history()
    req(idx >= 1, idx <= length(h))
    item <- h[[idx]]
    current_colors(item$colors)
    updateNumericInput(session, "n_colors", value = length(item$colors))
    updateTextInput(session, "palette_name", value = item$name)
    updateSelectInput(session, "preset", selected = "")
  })

  # ---- Active palette pipeline ----

  # Debounce so picker drags / rapid edits don't render every intermediate
  palette_debounced <- debounce(reactive(current_colors()), 250)

  cvd_mode <- reactive({
    if (is.null(input$cvd_view)) "none" else input$cvd_view
  })

  # A/B swap in place: plots show pinned or current, same size and position
  active_colors <- reactive({
    if (!is.null(pinned_colors()) && identical(input$ab_view, "pinned")) {
      pinned_colors()
    } else {
      palette_debounced()
    }
  })

  plot_colors <- reactive(apply_cvd(active_colors(), cvd_mode()))

  highlight_color <- debounce(reactive({
    val <- input$highlight_color
    if (is.null(val)) val <- current_colors()[1]
    apply_cvd(toupper(val), cvd_mode())
  }), 250)

  non_highlight_color <- debounce(reactive({
    val <- input$non_highlight_color
    cols <- current_colors()
    if (is.null(val)) val <- cols[length(cols)]
    apply_cvd(toupper(val), cvd_mode())
  }), 250)

  # ---- Dark theme ----

  plot_theme_extra <- reactive({
    if (identical(input$dark_mode, "dark")) dark_plot_theme() else theme()
  })

  # ---- Palette strip ----

  output$palette_strip <- renderUI({
    cols <- current_colors()
    swatches <- lapply(cols, function(col) {
      tags$div(
        class = "color-swatch",
        `data-color` = col,
        style = paste0("background-color: ", col, ";"),
        title = col,
        tags$span(class = "hex-tip", col)
      )
    })
    tags$div(
      id = "palette_swatches",
      style = "padding-bottom: 16px; display: flex; flex-wrap: wrap; gap: 2px;",
      do.call(tagList, swatches)
    )
  })

  output$pinned_strip <- renderUI({
    pinned <- pinned_colors()
    if (is.null(pinned)) return(NULL)
    n_cur <- length(current_colors())
    note <- if (length(pinned) != n_cur) {
      sprintf(" (%d colors vs current %d)", length(pinned), n_cur)
    } else {
      ""
    }
    swatches <- lapply(pinned, function(col) {
      tags$div(
        class = "color-swatch",
        style = paste0("background-color: ", col, "; opacity: 0.7;"),
        title = col,
        tags$span(class = "hex-tip", col)
      )
    })
    tagList(
      tags$div(class = "pinned-label", paste0("Pinned: ", pinned_name(), note)),
      tags$div(style = "margin-bottom: 8px;", do.call(tagList, swatches))
    )
  })

  # ---- History strip ----

  output$history_strip <- renderUI({
    h <- history()
    if (length(h) == 0) return(NULL)
    items <- lapply(seq_along(h), function(i) {
      sw <- lapply(h[[i]]$colors, function(col) {
        tags$div(
          class = "history-swatch",
          style = paste0("background-color: ", col, ";")
        )
      })
      tags$div(
        class = "history-item",
        onclick = sprintf(
          "Shiny.setInputValue('history_select', %d, {priority: 'event'})", i
        ),
        title = "Click to restore",
        tags$div(
          class = "d-flex", style = "gap: 2px;", do.call(tagList, sw)
        ),
        tags$span(class = "history-name", h[[i]]$name)
      )
    })
    tagList(
      tags$div(class = "pinned-label", "Recent palettes"),
      tags$div(
        class = "d-flex flex-wrap", style = "gap: 4px 12px;",
        do.call(tagList, items)
      )
    )
  })

  # ---- CVD simulation strips ----

  output$cvd_strips <- renderUI({
    cols <- palette_debounced()
    sims <- simulate_cvd(cols)

    rows <- lapply(names(sims), function(nm) {
      swatches <- lapply(sims[[nm]], function(col) {
        tags$div(
          class = "cvd-swatch",
          style = paste0("background-color: ", col, ";")
        )
      })
      tags$div(
        class = "cvd-row",
        tags$span(class = "cvd-label", nm),
        do.call(tagList, swatches)
      )
    })

    do.call(tagList, rows)
  })

  # ---- Distance & contrast diagnostics ----

  output$distance_summary <- renderUI({
    cols <- palette_debounced()
    pairs <- closest_pairs(cols)
    if (is.null(pairs)) return(NULL)

    closest <- pairs[1, ]
    badge_class <- if (closest$dist < 10) {
      "delta-bad"
    } else if (closest$dist < 20) {
      "delta-warn"
    } else {
      "delta-ok"
    }

    flagged <- pairs[pairs$dist < 20, , drop = FALSE]
    flag_rows <- lapply(seq_len(nrow(flagged)), function(k) {
      p <- flagged[k, ]
      tags$div(
        class = "d-flex align-items-center", style = "gap: 6px; margin: 2px 0;",
        tags$div(
          class = "history-swatch",
          style = paste0("background-color: ", cols[p$i], ";")
        ),
        tags$div(
          class = "history-swatch",
          style = paste0("background-color: ", cols[p$j], ";")
        ),
        tags$span(
          class = "history-name",
          sprintf("colors %d & %d — ΔE %.0f", p$i, p$j, p$dist)
        )
      )
    })

    tagList(
      tags$div(
        tags$span(
          class = paste("delta-badge", badge_class),
          sprintf(
            "Closest pair: %d & %d · ΔE %.0f",
            closest$i, closest$j, closest$dist
          )
        ),
        tags$small(
          class = "text-secondary ms-2",
          "minimum pairwise CIELAB distance — below 20 is hard to tell apart"
        )
      ),
      if (nrow(flagged) > 0) {
        tags$div(style = "margin-top: 6px;", do.call(tagList, flag_rows))
      }
    )
  })

  output$contrast_check <- renderUI({
    cols <- palette_debounced()
    ratio_span <- function(r) {
      tags$span(
        style = if (r >= 3) "font-weight: 700;" else "opacity: 0.5;",
        sprintf("%.1f", r)
      )
    }
    cells <- lapply(cols, function(col) {
      cr_white <- contrast_ratio("#FFFFFF", col)
      cr_dark <- contrast_ratio("#1A202C", col)
      tags$div(
        class = "text-center", style = "margin: 2px 4px;",
        tags$div(
          class = "contrast-cell",
          style = paste0("background-color: ", col, ";"),
          tags$span(style = "color: #FFFFFF;", "Aa"),
          tags$span(style = "color: #1A202C;", "Aa")
        ),
        tags$small(
          class = "history-name",
          ratio_span(cr_white), " / ", ratio_span(cr_dark)
        )
      )
    })
    tagList(
      tags$div(
        class = "pinned-label",
        "Label contrast — white / dark text, WCAG ratio (≥ 3 works for large labels)"
      ),
      tags$div(class = "d-flex flex-wrap", do.call(tagList, cells))
    )
  })

  # ---- Export code ----

  output$export_code <- renderText({
    cols <- current_colors()
    hex_str <- paste0('"', cols, '"', collapse = ", ")
    paste0(input$palette_name, " = c(", hex_str, ")")
  })

  observeEvent(input$copy_btn, {
    cols <- current_colors()
    hex_str <- paste0('"', cols, '"', collapse = ", ")
    code <- paste0(input$palette_name, " = c(", hex_str, ")")
    session$sendCustomMessage("copy_to_clipboard", code)
  })

  # ---- Data reactives ----

  fuel_data <- reactive({
    n <- min(length(active_colors()), 5)
    fuel_levels <- levels(subfuels$fuel)
    keep <- fuel_levels[seq_len(n)]

    subfuels |>
      filter(fuel %in% keep) |>
      mutate(
        fuel = factor(fuel, levels = keep),
        share = consumption_gwh / sum(consumption_gwh) * 100,
        .by = year
      )
  })

  bar_data <- reactive({
    n <- min(length(active_colors()), nrow(diamond_cuts))
    diamond_cuts |> tail(n)
  })

  # ---- Area plots ----

  output$area_stacked <- render_plot_hd({
    fd <- fuel_data()
    with_coverage(
      plot_area_stacked(fd, plot_colors()) + plot_theme_extra(),
      length(levels(fd$fuel)), length(active_colors())
    )
  })

  output$area_share <- render_plot_hd({
    fd <- fuel_data()
    with_coverage(
      plot_area_share(fd, plot_colors()) + plot_theme_extra(),
      length(levels(fd$fuel)), length(active_colors())
    )
  })

  # ---- Line plots ----

  output$line_labeled <- render_plot_hd({
    fd <- fuel_data()
    with_coverage(
      plot_line_labeled(fd, plot_colors()) + plot_theme_extra(),
      length(levels(fd$fuel)), length(active_colors())
    )
  })

  output$line_faceted <- render_plot_hd({
    fd <- fuel_data()
    with_coverage(
      plot_line_faceted(fd, plot_colors()) + plot_theme_extra(),
      length(levels(fd$fuel)), length(active_colors())
    )
  })

  output$line_single <- render_plot_hd(
    plot_line_single(total_fuel, plot_colors()) + plot_theme_extra()
  )

  output$line_trend <- render_plot_hd(
    plot_line_single_trend(co2_data, plot_colors()) + plot_theme_extra()
  )

  output$line_trend_dots <- render_plot_hd(
    plot_line_single_trend_dots(co2_data, plot_colors()) + plot_theme_extra()
  )

  # ---- Bar plots ----

  output$bar_vertical <- render_plot_hd(
    plot_bar_vertical(bar_data(), plot_colors()) + plot_theme_extra()
  )

  output$bar_horizontal <- render_plot_hd(
    plot_bar_horizontal(bar_data(), plot_colors()) + plot_theme_extra()
  )

  output$bar_labels_inside <- render_plot_hd(
    plot_bar_labels_inside(bar_data(), plot_colors()) + plot_theme_extra()
  )

  output$bar_highlighted <- render_plot_hd(
    plot_bar_highlighted(
      bar_data(), highlight_color(), non_highlight_color()
    ) + plot_theme_extra()
  )

  output$bar_highlighted_top <- render_plot_hd(
    plot_bar_highlighted_top(
      bar_data(), highlight_color(), non_highlight_color()
    ) + plot_theme_extra()
  )

  # ---- Scatter plots ----

  output$scatter_single <- render_plot_hd(
    plot_scatter_single(scatter_df, plot_colors()) + plot_theme_extra()
  )

  output$scatter_grouped <- render_plot_hd(
    plot_scatter_grouped(scatter_df, plot_colors()) + plot_theme_extra()
  )

  # ---- Histograms ----

  output$hist_plot <- render_plot_hd(
    plot_histogram(plot_colors()) + plot_theme_extra()
  )

  output$hist_fine <- render_plot_hd(
    plot_histogram_fine(plot_colors()) + plot_theme_extra()
  )

  output$hist_density <- render_plot_hd(
    plot_histogram_density(plot_colors()) + plot_theme_extra()
  )

  output$hist_by_cut <- render_plot_hd(
    plot_histogram_by_cut(plot_colors()) + plot_theme_extra()
  )

  # ---- Continuous plots ----
  # These (and the Complex tab) are the heaviest to draw, so cache on the
  # full render key — palette + dark mode — making A/B and tab revisits instant.

  output$heatmap_plot <- renderPlot({
    plot_heatmap(plot_colors()) + plot_theme_extra()
  }, res = 96, bg = "transparent") |>
    bindCache(plot_colors(), input$dark_mode)

  output$gradient_scatter <- renderPlot({
    plot_gradient_scatter(scatter_df, plot_colors()) + plot_theme_extra()
  }, res = 96, bg = "transparent") |>
    bindCache(plot_colors(), input$dark_mode)

  output$diverging_bar <- renderPlot({
    plot_diverging_bar(plot_colors()) + plot_theme_extra()
  }, res = 96, bg = "transparent") |>
    bindCache(plot_colors(), input$dark_mode)

  output$correlation_plot <- renderPlot({
    plot_correlation(plot_colors()) + plot_theme_extra()
  }, res = 96, bg = "transparent") |>
    bindCache(plot_colors(), input$dark_mode)

  # ---- Complex plots ----

  output$bubble_plot <- renderPlot({
    with_coverage(
      plot_bubble(bubble_data, plot_colors()) + plot_theme_extra(),
      min(length(active_colors()), 5), length(active_colors())
    )
  }, res = 96, bg = "transparent") |>
    bindCache(plot_colors(), input$dark_mode)

  output$bump_plot <- renderPlot({
    plot_bump(
      bump_list$ranking, bump_list$df_gdp,
      bump_list$measures, bump_list$countries_sel,
      plot_colors()
    ) + plot_theme_extra()
  }, res = 96, bg = "transparent") |>
    bindCache(plot_colors(), input$dark_mode)

  output$pyramid_plot <- renderPlot({
    plot_pyramid(pyramid_data, plot_colors()) + plot_theme_extra()
  }, res = 96, bg = "transparent") |>
    bindCache(plot_colors(), input$dark_mode)

  # ---- Overview ----

  output$all_area <- render_plot_hd(
    plot_mini_area(fuel_data(), plot_colors()) + plot_theme_extra()
  )

  output$all_line <- render_plot_hd(
    plot_mini_line(fuel_data(), plot_colors()) + plot_theme_extra()
  )

  output$all_bar <- render_plot_hd(
    plot_mini_bar(bar_data(), plot_colors()) + plot_theme_extra()
  )

  output$all_scatter <- render_plot_hd(
    plot_mini_scatter(scatter_df, plot_colors()) + plot_theme_extra()
  )
}
