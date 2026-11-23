# NEXT.md — current working state

_Last updated: 2026-11-23 (end of session)_

# Current Status

- Repo **`rnajena/pcago-unified`**, branch **`issue-10-modernize-docker`** (pushed),
  **PR #10** open against `master` (fixes issue **#9**, moved from rnajena/pcago#10 which is closed).
- **Release image built and tested locally:** `trichterhub/pcago-server:2.0` = `:latest`
  (built from commit `ac96d9f`, version string `2.0-ac96d9f`):
  - `rocker/shiny:4.6.2` pinned by digest → Ubuntu 26.04.1 LTS, R 4.6.2, Shiny Server 1.5.25 (bundled Node.js 24)
  - R packages from `src/renv.lock` (163 pkgs, Bioconductor 3.23, CRAN snapshot P3M 2026-11-22)
  - app copied from build context, non-root `shiny` (uid 999), HEALTHCHECK, server log → `docker logs`,
    app logs → `/home/shiny/logs`, config in `docker/shiny-server.conf` (port 8000, websockets disabled for Apache)
  - Tests on the release image: `docker/smoketest.R` 16/16 OK, `docker/browsertest.mjs` PASS
    (Load example data → PCA samples plot, all ~40 tabs, no disconnect), server log clean, health = healthy.
  - Martin's manual browser test: **works** (after the disconnect fix).
- **Not pushed to Docker Hub:** `docker push` → `insufficient_scope: authorization failed`
  (current Docker Desktop login has no push rights to `trichterhub`). Martin is clarifying access.
- Legacy Ubuntu 18.04 / R 3.4 tooling and the Electron desktop app are removed; READMEs + About page updated.
- Local container `pcago-test` (port 8000) runs the release image `2.0`.

# Current Focus

Get the image onto Docker Hub, then deploy with KSZ. **Martin asked to wait** with the KSZ message and the
Trivy scan until the Docker Hub push is solved.

# Next Tasks

1. [ ] **Docker Hub push** (blocked on Martin): after `docker login` with an account that can push to
       `trichterhub`: `docker push trichterhub/pcago-server:2.0` and `:latest`.
       Fallback if access is impossible: publish under another account (e.g. `mhoelzer`) → update image name
       in `README.md`, `Dockerfile_server` header comment, `CLAUDE.md`.
       If anything changed since `ac96d9f`, rebuild + retest first (see CLAUDE.md → Release process).
2. [ ] Merge PR #10 after the push (so `master` matches the published image).
3. [ ] Trivy (or Grype) vulnerability scan of `trichterhub/pcago-server:2.0` — report for KSZ. *(wait for Martin)*
4. [ ] Write the KSZ deployment note *(wait for Martin)*. Content: pin tag `2.0` (not `latest`); same port 8000,
       same log volume `/home/shiny/logs`, uid 999; Shiny Server log now in `docker logs`; websockets still
       disabled for the Apache proxy; optional Trivy report.
5. [ ] Manual test of paths not covered by automated tests (if Martin hasn't already): own file uploads,
       downloads, PCA movie export (.mp4), BioMart + AnnotationHub import (external APIs), GO browser.
6. [ ] After deployment: watch for the previously reported crashes for 1–2 weeks (`docker logs`, app logs),
       then close issue #9.
7. [ ] Regenerate `src/legal/licenses.md` for the new package versions (still lists 2018 packrat versions).
8. [ ] Check the sample annotation importer with source "Example data": fails with "No data to be imported
       selected!" (parameter checkboxes not offered for sample source). Probably pre-existing; the
       "Load example data" link works.
9. [ ] Optional: GitHub Actions to build the image + run smoke/browser tests on native amd64.
10. [ ] Optional: slim the image (multi-stage, drop `-dev` libs); currently ≈ 1.2 GB.

# Open Questions

- Who has push rights to `trichterhub` on Docker Hub (or should the image move to another account)?
- Does KSZ want a vulnerability scan report with the deployment request?

# Known Issues / Risks

- External BioMart / AnnotationHub APIs may have changed since 2018 — untested in the new stack.
- Further R 4.x behaviour changes may surface in UI paths not covered by the browser test
  (pattern to look for in logs: `Warning: Error in ...`; see CLAUDE.md → S4 generics).
- Builds on Apple Silicon run under emulation (slow, needs the `renv.tar.exe = ""` workaround in the Dockerfile).
- Help pages log "File ... not found (hence cannot be embedded)" warnings with markdown ≥ 2 (harmless).

# Recent Decisions

- 2026-11-23: Work in `rnajena/pcago-unified`; old repo superseded; issue recreated as pcago-unified#9.
- 2026-11-23: Base `rocker/shiny:4.6.2` (Ubuntu 26.04 LTS) pinned by digest; renv replaces packrat; no separate Node.js.
- 2026-11-23: Publish as `trichterhub/pcago-server`, tags `2.0` and `latest`.
- 2026-11-23: Legacy 18.04/packrat tooling and the Electron desktop app removed (Docker is the local option).
- 2026-11-23: S4/validation disconnect fixed centrally in `src/global.R`.
- 2026-11-23: KSZ message + Trivy scan postponed until the Docker Hub push works (Martin).
