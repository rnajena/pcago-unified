# NEXT.md — current working state

_Last updated: 2026-11-23_

# Current Status

- Working in **`rnajena/pcago-unified`**, branch **`issue-10-modernize-docker`** (nothing committed yet).
- New `Dockerfile_server` builds successfully (`trichterhub/pcago-server:2.0-dev`, local only, not pushed):
  - `rocker/shiny:4.6.2` pinned by digest → **Ubuntu 26.04.1 LTS, R 4.6.2, Shiny Server 1.5.25 (Node.js 24)**
  - R deps from `src/renv.lock` (163 pkgs, Bioconductor 3.23, CRAN snapshot P3M 2026-11-22)
  - app copied from build context (no `git clone master`), no downloaded packrat zip, no 3.4.3→3.4.4 hack, no npm
  - non-root `shiny` user (uid 999 kept), HEALTHCHECK, server log → `docker logs`, app logs → `/home/shiny/logs`
  - config in `docker/shiny-server.conf` (port 8000, 1800 s timeouts, websockets disabled for Apache proxy)
- Code changes for R 4.x: `src/geneAnnotationGRanges.R` uses `txdbmaker::makeTxDbFromGRanges`;
  `src/environment.R` reads version from `PCAGO_VERSION`.
- **Verified:** container starts, page renders, health = healthy, version shown;
  `docker/smoketest.R` passes all 16 steps (import, GFF/txdbmaker, DESeq2 + rlog, TPM, variance, PCA,
  clustering, ggplot/Cairo PNG, scatterplot3d/SVG, VennDiagram, ffmpeg, GO.db).
- README "Server 2.0" section updated (tag 2.0, build + smoke test instructions).
- Tracked as **rnajena/pcago-unified#9** (old rnajena/pcago#10 closed with a link; transfer was not permitted).
- Legacy Ubuntu 18.04 / R 3.4 tooling removed (`Dockerfile`, `install.sh`, `check_*.sh`, `src/packrat/`, packrat scripts); READMEs updated.
- Committed and PR opened (see PR for #9).

# Current Focus

Issue pcago-unified#9 — PR open; Martin is testing the UI in the browser in parallel.

# Next Tasks

1. [ ] Martin: **manual browser test** of the full workflow at http://localhost:8000 (container `pcago-test`),
       especially interactive plots, downloads, PCA movie export, BioMart + AnnotationHub import, GO browser.
2. [ ] Fix any R 4.x / Bioconductor 3.23 issues found (watch `aes_string` deprecation warnings, biomaRt `useMart`).
3. [x] New issue pcago-unified#9 created, rnajena/pcago#10 closed.
4. [x] Commit + PR.
5. [ ] Regenerate `src/legal/licenses.md` for the new package versions (old list is from packrat).
6. [ ] Investigate recurring server crashes on the production host (memory limits? check `docker logs` + app logs after deployment).
7. [ ] Push `trichterhub/pcago-server:2.0` + `:latest` and coordinate deployment with KSZ.
8. [ ] Optional: GitHub Actions workflow to build the image (+ smoke test) on native amd64; Trivy/Grype scan for KSZ.
9. [ ] Optional: decide whether to revive or delete the Electron app (`src-electron/`, still references packrat).

# Open Questions

- Is the Electron desktop app (Node.js in `src-electron/`) still maintained? (Out of scope for #10.)
- Slim the image (multi-stage, drop `-dev` libs after build)? Current size ≈ 1.2 GB.

# Known Issues / Risks

- Build under amd64 emulation on Apple Silicon is slow (~12 min for the R packages) and needs the
  `TAR=internal` / `renv.tar.exe = ""` workaround (GNU tar ENOSYS under emulation).
- Only the pipeline functions are tested automatically; the Shiny reactive UI is not covered.
- Help pages log "File ... not found (hence cannot be embedded)" warnings with markdown ≥ 2 (harmless; images served from `www/`).
- External BioMart / AnnotationHub APIs may have changed since 2018 — untested.

# Recent Decisions

- 2026-11-23: Work moved to `rnajena/pcago-unified` (old `rnajena/pcago` is superseded).
- 2026-11-23: Base image `rocker/shiny:4.6.2` (Ubuntu 26.04 LTS) pinned by digest; renv replaces packrat for the server image.
- 2026-11-23: No standalone Node.js in the server image (Shiny Server bundles Node 24).
- 2026-11-23: Keep publishing as `trichterhub/pcago-server`.
- 2026-11-23: Keep `shiny` uid 999 (gid stays 996; gid 999 is systemd-journal on 26.04).
- 2026-11-23: Tags `2.0` and `latest`; issue moved by recreating it as pcago-unified#9.
- 2026-11-23: Legacy 18.04/packrat tooling removed in the same PR; Electron app kept but marked unsupported.
- 2026-11-23: Dropped unused GitHub-only packages `ggbiplot`, `gradientPickerD3`; added `txdbmaker`.
