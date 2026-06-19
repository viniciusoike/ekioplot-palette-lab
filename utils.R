# Helpers, palette catalog, CSS/JS. Packages are loaded and datasets are
# read from app_data.rds in global.R.

# ---- Palette catalog ----

palette_choices <- {
  pals <- list_ekio_palettes("all")
  list(
    Categorical = pals$categorical,
    "Small Group" = pals$small_group,
    Scientific = pals$scientific,
    Sequential = pals$sequential,
    Diverging = pals$diverging
  )
}

default_colors <- ekio_pal("contrast")

# ---- Color helpers ----

is_valid_hex <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^#[0-9A-Fa-f]{6}$", x)
}

simulate_cvd <- function(colors) {
  list(
    Deuteranopia = colorspace::deutan(colors),
    Protanopia = colorspace::protan(colors),
    Tritanopia = colorspace::tritan(colors)
  )
}

apply_cvd <- function(colors, mode = "none") {
  switch(mode,
    deutan = colorspace::deutan(colors),
    protan = colorspace::protan(colors),
    tritan = colorspace::tritan(colors),
    colors
  )
}

# All pairwise CIELAB distances, sorted ascending
closest_pairs <- function(colors) {
  if (length(colors) < 2) return(NULL)
  lab <- as(colorspace::hex2RGB(colors), "LAB")@coords
  m <- as.matrix(dist(lab))
  idx <- which(upper.tri(m), arr.ind = TRUE)
  out <- data.frame(i = idx[, 1], j = idx[, 2], dist = m[idx])
  out[order(out$dist), ]
}

relative_luminance <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  chan <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  as.numeric(0.2126 * chan[1, ] + 0.7152 * chan[2, ] + 0.0722 * chan[3, ])
}

contrast_ratio <- function(fg, bg) {
  l1 <- relative_luminance(fg)
  l2 <- relative_luminance(bg)
  (pmax(l1, l2) + 0.05) / (pmin(l1, l2) + 0.05)
}

# Transparent plot background so plots inherit the bslib card color in
# both light and dark mode (theme_ekio paints its own bg in light mode)
dark_plot_theme <- function() {
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "#343A40", color = NA),
    text = element_text(color = "#DEE2E6"),
    plot.title = element_text(color = "#F8F9FA"),
    plot.subtitle = element_text(color = "#ADB5BD"),
    axis.text = element_text(color = "#ADB5BD"),
    axis.title = element_text(color = "#CED4DA"),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.text = element_text(color = "#ADB5BD"),
    legend.title = element_text(color = "#CED4DA"),
    strip.text = element_text(color = "#DEE2E6"),
    strip.background = element_rect(fill = "#343A40", color = NA),
    # theme_ekio sets the axis-specific grid elements, so override those
    panel.grid.major.x = element_line(color = "#495057", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "#495057", linewidth = 0.4)
  )
}

# renderPlot wrapper: higher dpi for crisp output, transparent device
render_plot_hd <- function(expr, env = parent.frame()) {
  renderPlot(substitute(expr), env = env, quoted = TRUE,
             res = 96, bg = "transparent")
}

# Add a caption when a categorical chart shows fewer groups than the palette
# has colors, so colors beyond the data's group count aren't silently dropped.
with_coverage <- function(p, used, total) {
  if (used < total) {
    p + labs(caption = sprintf(
      "Showing %d of %d colors — this chart type has %d groups",
      used, total, used
    ))
  } else {
    p
  }
}

# ---- Data builders (moved) ----
# Datasets are precomputed by data-raw/shiny-app-data.R and read from
# app_data.rds in global.R. See that script to regenerate them.

# ---- CSS ----

app_css <- "
.color-swatch {
  display: inline-block; width: 40px; height: 40px; border-radius: 6px;
  border: 2px solid var(--bs-border-color); margin: 2px; cursor: grab;
  transition: transform 0.15s ease, border-color 0.15s ease;
  position: relative;
}
.color-swatch:active { cursor: grabbing; }
.color-swatch:hover { transform: scale(1.15); z-index: 2; }
.color-swatch.active { border-color: var(--bs-primary) !important; border-width: 3px; }
.sortable-ghost { opacity: 0.4; }
.color-swatch .hex-tip {
  display: none; position: absolute; bottom: -22px; left: 50%;
  transform: translateX(-50%); font-size: 10px;
  color: var(--bs-secondary-color);
  white-space: nowrap; font-family: monospace;
}
.color-swatch:hover .hex-tip { display: block; }
.cvd-row { margin: 6px 0; display: flex; align-items: center; }
.cvd-label {
  width: 100px; font-size: 11px; color: var(--bs-secondary-color);
  font-weight: 500; flex-shrink: 0;
}
.cvd-swatch {
  display: inline-block; width: 28px; height: 28px; border-radius: 4px;
  border: 1px solid var(--bs-border-color); margin: 1px;
}
.pinned-label {
  font-size: 11px; color: var(--bs-secondary-color);
  font-weight: 500; margin-bottom: 4px; margin-top: 8px;
}
.history-item {
  display: flex; align-items: center; gap: 6px; cursor: pointer;
  padding: 3px 8px; border-radius: 6px;
}
.history-item:hover { background: var(--bs-tertiary-bg); }
.history-swatch {
  width: 16px; height: 16px; border-radius: 3px; display: inline-block;
  border: 1px solid var(--bs-border-color);
}
.history-name { font-size: 11px; color: var(--bs-secondary-color); }
.delta-badge {
  font-family: monospace; font-size: 12px; font-weight: 600;
  padding: 3px 10px; border-radius: 999px; display: inline-block;
}
.delta-ok { background: #C6F6D5; color: #22543D; }
.delta-warn { background: #FEEBC8; color: #7B341E; }
.delta-bad { background: #FED7D7; color: #822727; }
.contrast-cell {
  width: 48px; height: 36px; border-radius: 6px; display: flex;
  align-items: center; justify-content: center; gap: 5px;
  font-size: 12px; font-weight: 600;
}
[data-bs-theme='dark'] .form-select,
[data-bs-theme='dark'] .form-control {
  background-color: var(--bs-secondary-bg);
  color: var(--bs-body-color);
  border-color: var(--bs-border-color);
}
[data-bs-theme='dark'] .btn-outline-primary {
  --bs-btn-color: #8FB4DC;
  --bs-btn-border-color: #5A7CA3;
  --bs-btn-hover-bg: #2B4A6F;
  --bs-btn-hover-color: #DEE2E6;
  --bs-btn-hover-border-color: #5A7CA3;
}
.card-header .shiny-input-container { margin-bottom: 0; width: auto; }
.card-header .form-group { margin-bottom: 0; }
.card-header .radio-inline, .card-header .form-check { margin-bottom: 0; }
#color_inputs .shiny-input-container { margin-bottom: 6px; }
#export_code {
  font-family: 'Fira Code', 'Monaco', monospace; font-size: 12px;
  background: #1A202C; color: #A8D0E8; padding: 12px; border-radius: 8px;
}
"

app_js <- "
Shiny.addCustomMessageHandler('copy_to_clipboard', function(text) {
  navigator.clipboard.writeText(text).then(function() {
    var btn = document.getElementById('copy_btn');
    var orig = btn.innerHTML;
    btn.innerHTML = 'Copied!';
    setTimeout(function() { btn.innerHTML = orig; }, 1500);
  });
});

$(document).on('shiny:connected', function() {
  var isDragging = false;
  var activeColor = null;

  function initSortable() {
    var el = document.getElementById('palette_swatches');
    if (!el) return;
    if (el._sortable) el._sortable.destroy();
    el._sortable = new Sortable(el, {
      animation: 150,
      ghostClass: 'sortable-ghost',
      onStart: function() { isDragging = true; },
      onEnd: function() {
        var swatches = el.querySelectorAll('.color-swatch');
        var newColors = Array.from(swatches).map(function(s) {
          return s.getAttribute('data-color');
        });
        Shiny.setInputValue('reordered_colors', newColors, {priority: 'event'});
        setTimeout(function() { isDragging = false; }, 100);
      }
    });
    if (activeColor) {
      $(el).find(\"[data-color='\" + activeColor + \"']\").addClass('active');
    }
  }

  var waitForTarget = setInterval(function() {
    var target = document.getElementById('palette_strip');
    if (target) {
      clearInterval(waitForTarget);
      new MutationObserver(function() {
        setTimeout(initSortable, 50);
      }).observe(target, { childList: true, subtree: true });
      initSortable();
    }
  }, 100);

  $(document).on('click', '#palette_swatches .color-swatch', function() {
    if (isDragging) return;
    var color = $(this).attr('data-color');
    activeColor = color;
    Shiny.setInputValue('swatch_clicked', color, {priority: 'event'});
    $('#palette_swatches .color-swatch').removeClass('active');
    $(this).addClass('active');
  });
});
"
