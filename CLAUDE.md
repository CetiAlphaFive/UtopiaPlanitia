# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

UtopiaPlanitia is an R package providing variable importance, heterogeneity tests, nuisance-estimation backends, and visualization tools for causal forests (built on `grf`). Most functions relate to heterogeneous treatment effect (HTE) analysis.

- **Version:** 0.3.1 (see `DESCRIPTION` for the source of truth)
- **GitHub:** [CetiAlphaFive/UtopiaPlanitia](https://github.com/CetiAlphaFive/UtopiaPlanitia)
- **pkgdown site:** [cetialphafive.github.io/UtopiaPlanitia](https://cetialphafive.github.io/UtopiaPlanitia/)

## Common Commands

Run from the package root. Most use `devtools` / `testthat` edition 3.

```r
# Load package for interactive dev (no install)
devtools::load_all()

# Regenerate man/ + NAMESPACE after editing roxygen2 blocks
devtools::document()

# Run the full test suite
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-loco-grf.R")

# Run tests matching a description filter
devtools::test(filter = "loco")        # runs test-loco*.R files

# Full R CMD check (what CI runs via check-standard.yaml)
devtools::check()

# Rebuild the pkgdown site
pkgdown::build_site()

# Lint (CI runs lint.yaml); test coverage via test-coverage.yaml
lintr::lint_package()
covr::package_coverage()
```

CI workflows live in `.github/workflows/`: `check-standard.yaml` (R CMD check), `lint.yaml`, `test-coverage.yaml` (Codecov), `pkgdown.yaml`.

## Architecture

Three functional clusters, all centered on a fitted `grf::causal_forest` (or related grf/ranger model):

### 1. Nuisance-estimation backends (CATE estimation)
Wrappers that fit a causal forest with non-default nuisance estimators (Y-hat, W-hat), all returning a `causal_forest`-compatible object so downstream diagnostics work unchanged:
| File | Export | Purpose |
|------|--------|---------|
| `autocf.R` | `autocf()` | Auto-selected nuisance causal forest. Cross-fits a pool of candidates (`grf`, `glmnet`, `xgboost`, `tabpfn`, `bart`), scores each by K-fold weighted MSE, picks the best per nuisance. Each candidate has its own `.autocf_fp_*` fit-predict closure; the `xgboost` candidate (and only that one) needs the `mlr3` stack (`mlr3`/`mlr3learners`/`mlr3tuning`/`paradox`) for its AutoTuner. `tabpfn` candidate is dropped when weights are non-trivial or `TABPFN_TOKEN` is unset |
| `glmcf.R` | `glmcf()` | Nuisances via `cv.glmnet` |
| `tabcf.R` | `tabcf()` | Nuisances via TabPFN (requires `TABPFN_TOKEN`) |
| `setup_tabpfn_token.R` | `setup_tabpfn_token()` | Set up the `TABPFN_TOKEN` env var for `tabcf()` |

### 2. Diagnostics & tests (the analytical core)
| File | Export | Purpose |
|------|--------|---------|
| `cf_loco.R` + `compute_vimp.R` | `cf_loco()` | LOCO variable importance for causal forests |
| `cf_loco_methods.R` | `print.cf_loco`, `summary.cf_loco`, `plot.cf_loco` | S3 methods for LOCO objects |
| `cf_perm.R` | `cf_perm()` | PermuCATE conditional-permutation variable importance (Paillard et al., 2025) — permutes rather than refits, reports one-sided p-values + CIs. `loss = "R"` (default, Robinson residual, uses forest `Y.hat`/`W.hat`) or `loss = "AIPW"` (`grf::get_scores()`); light path (default) scores forest in place, `cross.fit = TRUE` opts into K-fold refit with Nadeau-Bengio inference (R-loss only). Complements `cf_loco()` |
| `cf_perm_methods.R` | `print.cf_perm`, `summary.cf_perm`, `plot.cf_perm` | S3 methods for PermuCATE objects |
| `loco.R` | `loco()` | LOCO variable importance for ranger AND grf outcome forests (regression/probability/boosted); auto-detects backend from model class |
| `omni_hetero.R` | `omni_hetero()` | Omnibus HTE test battery (calibration, high/low CATE, RATE) |

### 3. Visualization
| File | Export | Purpose |
|------|--------|---------|
| `plot.causal_forest.R` | `plot.causal_forest` | S3 `plot()` dispatcher; `type =` "diag" (default), "rank", "pdp", "inter" |
| `summary.causal_forest.R` | `summary.causal_forest`, `print.summary.causal_forest` | S3 `summary()` for causal forests |
| `plot_diag.R` | (internal panels) | Multi-panel diagnostic plot |
| `plot_pdp.R` | `plot_pdp()` | Real 1-way and 2-way partial dependence plots; `subgroup = TRUE` switches to doubly-robust (AIPW) subgroup-ATE points (1-way) / heatmap (2-way) for binary or low-cardinality covariates |
| `plot_rank.R` | `plot_rank()`, `rank_plot()` | Ranked CATEs with confidence intervals |
| `plot_scatter.R` | `plot_scatter()` | Individual OOB CATEs vs. a covariate |
| `plot_rate.R` | (internal) `plot_rate` | TOC/RATE curve panel |
| `plot_blp.R` | (internal) `plot_blp` | BLP calibration panel |
| `utopia_plot.R` | `print.utopia_plot` | Shared S3 print method wrapping plot objects |
| `omni_hetero` plotting | `plot.omni_hetero`, `print.omni_hetero` | S3 methods (in `plot.omni_hetero.R`) |

`zzz.R` holds package `.onLoad`/`.onAttach` hooks. `R/old/` is archived/pruned code — not built; `dev/` (plans, specs) and `runs/` are agent/scratch working dirs, also not part of the package. `tools/blp_smoke.R` is a standalone `Rscript` smoke test that runs hetero + null toy DGPs through `omni_hetero() |> plot()` to eyeball the BLP calibration panel — handy after touching `plot_blp.R`/`omni_hetero.R`.

### Cross-cutting conventions
- **S3 dispatch is the public interface.** A fitted `causal_forest` flows into `summary()` / `plot()`; LOCO objects (`cf_loco`) and test objects (`omni_hetero`) carry their own print/summary/plot methods. New functionality should return a classed object with matching S3 methods rather than printing directly.
- **NAMESPACE is generated** by roxygen2 — never hand-edit it; edit roxygen blocks and run `devtools::document()`.
- `grf` and `rlang` are the only hard `Imports`. Everything else (ggplot2, ranger, glmnet, xgboost, TabPFN, mlr3 stack, etc.) is `Suggests` — guard usage with `requireNamespace()` and keep examples/tests skippable when a suggested package is absent. `future` provides optional parallelism for `tabcf()`/`autocf()`/`setup_tabpfn_token()`; `MLbalance` backs a `plot_diag.R` calibration panel; `survival` is used by `loco.R`; `dbarts` is the `bart` candidate in `autocf()`. `loco.R` does **not** depend on `conformalInference` (removed 2026-07; its one code path that used it now uses the in-package `loco_custom_split()`).
- Markdown roxygen is enabled (`Roxygen: list(markdown = TRUE)`).
- **R 4.1+ is the floor** (`Depends: R (>= 4.1.0)`), so the native pipe `|>` is always available — use it (as in `tools/blp_smoke.R`) rather than adding a `magrittr` dependency for `%>%`.

## Dev Conventions

- **Documentation:** roxygen2 (markdown enabled)
- **Feature workflow.** Non-trivial features are built plan-first: a dated `dev/plans/YYYY-MM-DD-<feature>.md` (what/why) paired with a `dev/specs/YYYY-MM-DD-<feature>-design.md` (design), then a dedicated `tests/testthat/test-<feature>.R` and a `NEWS.md` entry under the development-version heading. `dev/` and `runs/` are tracked scratch dirs, build-ignored via `.Rbuildignore` (alongside `CLAUDE.md`, `tools/`, `docs/`, `pkgdown/`).
- **Git workflow.** Work lands on `main` from short-lived `feat/<feature>` branches via merge commits (e.g. `feat/cf_perm`, `feat/pdp-subgroup-ate`); the merge message names the feature. Don't commit feature work straight to `main`.
- **Tests:** testthat edition 3; snapshots in `tests/testthat/_snaps/`. Slow Monte Carlo correctness tests (e.g. `test-h2-crossfit-typeI.R`, which checks the cross-fit High/Low test holds its nominal Type I rate) are gated behind `UTOPIA_RUN_SLOW_TESTS=1` and `skip_on_cran()` — set the env var to run them locally
- **Site:** pkgdown (`_pkgdown.yml`)
- **CRAN prep:** `cran-comments.md` tracks submission notes
- **Backwards compatibility is mandatory.** Code from this package ends up in published replication materials, so a breaking change can silently invalidate someone's already-run results. Treat the public API — exported function names, argument names, argument order, defaults, and return-object structure / S3 classes — as a contract:
    - **Add, don't change.** New arguments go at the *end* of the signature with a default that reproduces the old behavior exactly (e.g. `omni_hetero(..., num.folds = 5)`). Never reorder or rename existing arguments, change a default, or alter a return structure without a deprecation path.
    - **Deprecate, don't delete.** Warn for at least one release (the `lifecycle` pattern) before removing or renaming anything user-facing; see the `tabcf()` `eps` → `clip` deprecation for the template.
    - **Flag breaking changes loudly.** If a change is unavoidably breaking, raise it with Jack *before* shipping, document it under an explicit "Breaking changes" heading in `NEWS.md`, and bump the version accordingly.
    - The API-stability guards in `tests/testthat/test-fixes-20260511.R` (`T-API: … formals unchanged`) pin exported signatures on purpose. If one fails, do **not** reflexively update it — first confirm the API change is intended, backwards-compatible, and flagged.

---

## Chief of Staff Integration

The canonical project tracker lives at `~/Dropbox/chief_of_staff/CHIEF_OF_STAFF.md`.

### "update COS" command

When Jack says **"update COS"** at the end of a session:

1. Write a brief status update (2-4 lines max) summarizing:
   - What was accomplished this session
   - What changed (status shifts, blockers added/removed)
   - What the next action is
2. Append it to the `## Raw Updates` section at the bottom of `~/Dropbox/chief_of_staff/CHIEF_OF_STAFF.md` using this format:

```
[YYYY-MM-DD] UtopiaPlanitia — Accomplished: [what]. Changed: [what]. Next: [what].
```

3. Confirm to Jack that the update was appended.

**Do not** read, parse, or reorganize the rest of the state file. Just append. The CoS agent handles integration.
