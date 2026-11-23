# CLAUDE.md — PCAGO onboarding

Permanent onboarding document for Claude Code sessions. Change it only when project knowledge changes.
For the current working state see `NEXT.md`; for history see `docs/sessions/`.

## Things future Claude sessions should always know

- **Address the user as Martin.**
- **Session start:** read `CLAUDE.md`, `NEXT.md`, and the newest file in `docs/sessions/` before doing anything else.
- **Handoff:** when Martin says *"Update the project documentation"* or *"Prepare a handoff"*:
  1. Update `CLAUDE.md` if project knowledge changed.
  2. Update `NEXT.md`.
  3. Create or update `docs/sessions/YYYY-MM-DD.md` for today (never overwrite earlier days).
- Keep `NEXT.md` in sync with progress during the session, not only at handoff.
- If something is unclear, ask Martin before writing code.
- Work on feature branches, never directly on `master`. Commit/push only when Martin asks.
- **This repo (`rnajena/pcago-unified`) is the active one.** The old `rnajena/pcago` repo
  (cloned at `/Users/martin/git/pcago`) is superseded; don't make changes there.
- Martin's machine is macOS **arm64** without R. All R work and testing happens in Docker
  (`--platform linux/amd64`, emulated, slow). Martin may install R locally if Docker gets too slow.

## Project overview

PCAGO is an interactive **R/Shiny web application** for analysing RNA-Seq read count data with
**principal component analysis (PCA)** and **agglomerative clustering**. Features: read count import,
normalization (DESeq2 VST/rlog, TPM, …), filtering genes by annotation (GFF/GTF, AnnotationHub,
BioMart, GO terms incl. a GO browser), many plots (sample PCA 2D/3D incl. animated movies via ffmpeg,
loadings, variance, Venn diagrams, dendrograms) and downloadable tables/plots.

Developed at RNA Bioinformatics / University of Jena (rnajena). Public instance:
https://pcago.bioinf.uni-jena.de, operated with the **KSZ** (university computing centre), which
requires a secure, supported stack (Ubuntu ≥ 24.04 LTS). License: GPL-3.

## Goals

- Keep PCAGO publicly available and maintained.
- Run it on a **secure, supported, reproducible, stable** software stack (issue #10).
- Avoid functional regressions when upgrading R / Bioconductor / Shiny.

## Repository layout

| Path | Purpose |
|---|---|
| `src/` | **The Shiny app** (`ui.R`, `server.R`, all R code, help pages, sample data) |
| `src/renv.lock` | **Pinned R dependencies** (R 4.6.2, Bioconductor 3.23, CRAN snapshot from Posit Package Manager) |
| `Dockerfile_server` | **Production server image** (`trichterhub/pcago-server`), rocker/shiny + Shiny Server |
| `docker/shiny-server.conf` | Shiny Server config used by the image (port 8000, timeouts, proxy settings) |
| `.dockerignore` | Keeps `.git`, docs etc. out of the build context |
| `docker/smoketest.R` | Pipeline smoke test run against the image (sample data, 16 steps) |
| `CLAUDE.md`, `NEXT.md`, `docs/sessions/` | Claude Code onboarding / state / history |

Inside `src/`:

| Path | Purpose |
|---|---|
| `ui.R`, `server.R` | Shiny entry points |
| `global.R` | Loaded by Shiny first: wrappers that keep validation errors intact through S4 generics (see below) |
| `environment.R` | Config: ffmpeg/git paths, parallel importer, limits, version (`PCAGO_VERSION` env var) |
| `defaultParameters.R` | Default UI parameter values |
| `widget*.R` / `plot*.R` | Shiny modules (widgets / plots) |
| `class*.R` | Data classes |
| `readcounts.R`, `geneAnnotation*.R`, `sampleAnnotation*.R`, `goTerms.R`, `pca.R`, `relevantGenes.R` | Domain logic |
| `aboutpage/`, `helppages/`, `helptooltips/`, `legal/`, `www/`, `scripts/` | Static content, assets, JS |
| `sampledata/` | Example data for manual testing |

## High-level architecture

- Classic Shiny app: `src/ui.R` + `src/server.R`, started by Shiny Server (production) or `shiny::runApp()`.
- `ui.R` builds a `navbarPage` with tabs About / Analyze / Help / Impressum.
- `server.R` sources all logic files and wires the reactive pipeline; `serverFunctions.R` holds most server logic.
- **Data pipeline:** import read counts → preprocessing (transpose, remove zero genes) → normalization
  → postprocessing (remove constant genes) → gene filtering (annotation / keywords / GO) → gene subset by
  variance → PCA (`pca.R`) / clustering → plots, each step with a report (`processingReports.R`).
- **Shiny modules (old style):** `xxxUI(id)` with `NS(id)` + server function `xxx_(input, output, session, ...)`
  called via `callModule()` (not `moduleServer()`).
- Gene annotation import can run in a forked process (`parallel::mcparallel`, see `helpers.R`).

## Technology stack

- **Server image:** `rocker/shiny:4.6.2` (Ubuntu 26.04 LTS, R 4.6.2, Shiny Server 1.5.25 with bundled Node.js 24),
  pinned by digest. Node.js is only used inside Shiny Server; no separate Node install in the server image.
- **R deps via renv** (`src/renv.lock`, 163 packages): shiny, shinyBS, shinyjs, DT, colourpicker,
  ggplot2, scatterplot3d, plot3D, VennDiagram, dendextend, svglite, Cairo, gplots, openxlsx;
  Bioconductor: DESeq2, AnnotationHub, AnnotationDbi, biomaRt, GenomicRanges, GenomicFeatures,
  txdbmaker, rtracklayer, GO.db, SummarizedExperiment.
- System tools: ffmpeg (movies). Git is not in the image; version comes from `PCAGO_VERSION`.
- The Electron desktop app was removed (no longer supported); Docker is the way to run PCAGO locally.

## Coding conventions and style

- R code uses **dot.separated names** (`readcounts.raw`, `pca.center`), two-space indentation,
  `<-` assignment, `T`/`F` for booleans, roxygen-like `#'` header comments.
- Files `source()` their dependencies at the top; follow the existing module pattern.
- Prefer `pkg::fun()` for functions that moved between packages (e.g. `txdbmaker::makeTxDbFromGRanges`).
- Keep changes minimal; don't reformat unrelated code.
- Dockerfiles: pin base images by tag + digest, pin package snapshots, `--no-install-recommends`,
  clean apt lists, run as non-root `shiny` (uid 999).

## Common commands

```bash
# Build the server image (repo root, amd64 emulation on Apple Silicon)
docker build --platform linux/amd64 --build-arg PCAGO_VERSION=$(git describe --always --dirty) \
  -t trichterhub/pcago-server:2.0-dev -f Dockerfile_server .
# Run it
docker run --rm -p 8000:8000 -v "$(pwd)"/logs:/home/shiny/logs trichterhub/pcago-server:2.0-dev
# R console inside the image
docker run --rm -it --platform linux/amd64 --entrypoint R trichterhub/pcago-server:2.0-dev
# Pipeline smoke test (R functions) and headless-Chrome UI test (needs Node >= 22 + Chrome)
docker run --rm --platform linux/amd64 -v "$(pwd)"/docker/smoketest.R:/tmp/smoketest.R:ro \
  --entrypoint Rscript trichterhub/pcago-server:2.0-dev /tmp/smoketest.R
node docker/browsertest.mjs http://127.0.0.1:8000
# Fast iteration on app code: mount src/ instead of rebuilding
docker run -d --name pcago-test --platform linux/amd64 -p 8000:8000 -v "$(pwd)/src":/srv/pcago:ro trichterhub/pcago-server:2.0-dev
# Issues
gh issue view 9 --repo rnajena/pcago-unified   # Docker modernization (moved from rnajena/pcago#10)
```

### Updating R dependencies

`src/renv.lock` was generated in a throwaway `rocker/shiny:4.6.2` container: install the direct
dependencies (found by `renv::dependencies("src")`, plus `txdbmaker`, minus knitr/rmarkdown) with
`BiocManager::install()`, then `renv::snapshot(packages = deps, lockfile = ...)`. Afterwards the CRAN
repo URL in the lockfile is set to a **dated** P3M snapshot (`https://p3m.dev/cran/YYYY-MM-DD`;
today's date is usually not published yet). Keep the `renv` install URL in `Dockerfile_server` on the same date.

## Development workflow

1. Read `CLAUDE.md`, `NEXT.md`, latest session note.
2. Feature branch from `master` (current: `issue-10-modernize-docker`).
3. Make focused changes; build + test in Docker.
4. Update `NEXT.md` as work progresses; session note at the end.
5. Commit/push/PR only when Martin asks.

## Known constraints

- KSZ requires Ubuntu ≥ 24.04 LTS (26.04 preferred — satisfied by rocker/shiny 4.6.x).
- rocker/shiny 4.5.3+ images are **amd64 only**.
- Shiny Server does **not** forward container env vars to apps → values go into `$R_HOME/etc/Renviron.site`.
- The KSZ Apache reverse proxy needs websockets disabled (`disable_protocols` in `docker/shiny-server.conf`).
- Tests: `docker/smoketest.R` (pipeline functions) and `docker/browsertest.mjs` (headless Chrome: "Load example
  data" + visit all tabs, fails on disconnect or real output errors). Not covered: uploads, downloads, movie
  export, BioMart/AnnotationHub import, GO browser → test manually.
- **S4 generics swallow Shiny validation errors (R 4.x + Bioconductor):** a reactive that fails `validate()`
  inside an argument of an S4 generic (`colnames(conditions())`, app generics like `mergeSampleAnnotation`)
  is re-raised as a plain error → Shiny closes the session ("Disconnected from the server").
  `src/global.R` wraps those generics so arguments are forced first (base-name generics at load time,
  app generics via `pcago.wrap.app.generics()` at session start in `server.R`). New S4 generics that
  shadow base functions must be added to `pcago.generics.forced`.
- Observers must not fail on `NULL` inputs (an error in an observer closes the session) — use `isTRUE()` etc.
- BioMart / AnnotationHub need network access at runtime.

## Important architectural decisions

- 2026-11-23: Onboarding docs established (`CLAUDE.md` / `NEXT.md` / `docs/sessions/`).
- 2026-11-23: Work moved from `rnajena/pcago` to `rnajena/pcago-unified` (newer code, holds the Dockerfiles).
- 2026-11-23: Server image based on **rocker/shiny** (pinned R 4.6.2, Ubuntu 26.04); app copied from the
  build context instead of `git clone master`; dependencies via **renv** instead of a downloaded packrat zip.
- 2026-11-23: No separate Node.js in the server image; image stays `trichterhub/pcago-server` on Docker Hub.
- 2026-11-23: Legacy Ubuntu 18.04 / R 3.4 tooling removed (`Dockerfile`, `install.sh`, `check_*.sh`, `src/packrat/`,
  `run_packrat.sh`, `extract_packrat_licenses.sh`). Electron desktop app (`src-electron/`, `src/pcago.R`) removed as no longer supported.
- 2026-11-23: Images are tagged `X.Y` **and** `latest` on Docker Hub.
- 2026-11-23: Validation-vs-S4 problem solved centrally in `src/global.R` (forcing wrappers) instead of
  editing ~50 call sites.
- 2026-11-23: `shiny` user kept at uid 999 (gid stays 996; gid 999 is systemd-journal) for compatibility with existing host log volumes.
