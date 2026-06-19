# EKIO Palette Lab

An interactive Shiny app for building and comparing
[EKIO](https://viniciusoike.github.io/ekioplot/) colour palettes across a wide
range of chart types. Edit palettes live, simulate colour-vision deficiencies,
check contrast, and copy ready-to-paste scale code.

**Hosted (runs entirely in your browser, no install):**
<https://viniciusoike.github.io/ekioplot-palette-lab/>

The hosted app runs via WebAssembly (webR) — there is no Shiny server to host.

## Run locally

The app depends on [`ekioplot`](https://github.com/viniciusoike/ekioplot) plus
several visualisation/UI packages:

```r
install.packages(
  "ekioplot",
  repos = c("https://viniciusoike.r-universe.dev", "https://cloud.r-project.org")
)
install.packages(c(
  "shiny", "bslib", "ggplot2", "dplyr", "forcats", "stringr", "tibble",
  "ggbump", "patchwork", "colorspace", "colourpicker"
))
```

Then, from the repo root:

```r
shiny::runApp(".")
```

`global.R` checks for these packages on startup and prompts to install any that
are missing.

## Repository layout

- `global.R`, `ui.R`, `server.R` — the Shiny app
- `utils.R`, `plot_funs.R` — helpers and plot builders (sourced by `global.R`)
- `app_data.rds` — precomputed datasets so the app launches without the heavier
  build-time deps; rebuild with `data-raw/shiny-app-data.R`
- `www/` — static assets
- `data-raw/` — data build script, plot references, and deployment notes
- `.github/workflows/deploy.yaml` — builds the shinylive app and publishes it to
  GitHub Pages on every push to `main`/`master`

## Deployment

See [`data-raw/DEPLOY-NOTES.md`](data-raw/DEPLOY-NOTES.md) for how the in-browser
build resolves the `ekioplot` WebAssembly binary from r-universe.

To preview the static build locally (requires the Emscripten/webR toolchain):

```r
shinylive::export(".", "_site")
httpuv::runStaticServer("_site")
```
