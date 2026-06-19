# Deployment: shinylive Palette Lab

The **Palette Lab** is published to GitHub Pages by
`.github/workflows/deploy.yaml`:

- Palette Lab → `https://viniciusoike.github.io/ekioplot-palette-lab/`

The app runs entirely in the browser via WebAssembly (webR) — there is no Shiny
server to host or pay for.

## How `ekioplot` reaches the browser build

shinylive auto-fetches WebAssembly binaries for CRAN packages from
`https://repo.r-wasm.org`. `ekioplot` is not on CRAN, so it is served from
**r-universe**, which auto-builds a WASM binary for every package it tracks.
shinylive resolves it from the `Repository:` field that an r-universe install
writes into the package `DESCRIPTION` — which is why `deploy.yaml` installs
`ekioplot` from `https://viniciusoike.r-universe.dev` before exporting.

## One-time setup

1. The `viniciusoike` r-universe registry (`viniciusoike/universe` →
   `packages.json`) must list `ekioplot`. The expected contents are mirrored in
   [`r-universe-packages.json`](./r-universe-packages.json). Confirm the WASM
   build at `https://viniciusoike.r-universe.dev/ekioplot` (check the **wasm**
   badge).

2. Enable GitHub Pages for **this** repo from the **`gh-pages`** branch
   (Settings → Pages → Source: Deploy from a branch → `gh-pages` / root). The
   workflow creates and pushes that branch on the first run.

After that, every push to `main`/`master` rebuilds and redeploys the app.

## Notes

- The deploy uses the latest *published* r-universe build of `ekioplot`, which
  may lag the newest `ekioplot` commit by a few minutes. The app only calls
  stable exported functions, so this is harmless.
- To preview the build locally (requires the Emscripten/webR toolchain):
  `shinylive::export(".", "_site")` then `httpuv::runStaticServer("_site")`.
