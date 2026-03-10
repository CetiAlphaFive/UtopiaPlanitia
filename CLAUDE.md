# CLAUDE.md — UtopiaPlanitia

## What This Is

UtopiaPlanitia is an R package providing variable importance, heterogeneity tests, and visualization tools for causal forests (built on `grf`). v0.2.0. Think of it as a shipyard for experimental causal forest tooling.

- **GitHub:** [CetiAlphaFive/UtopiaPlanitia](https://github.com/CetiAlphaFive/UtopiaPlanitia)
- **pkgdown site:** [cetialphafive.github.io/UtopiaPlanitia](https://cetialphafive.github.io/UtopiaPlanitia/)

## Key Source Files (`R/`)

| File | Purpose |
|------|---------|
| `summary.causal_forest.R` | S3 `summary()` + `print()` for causal forests |
| `plot.causal_forest.R` | S3 `plot()` dispatcher for causal forests |
| `cf_loco.R` | LOCO variable importance for causal forests |
| `compute_vimp.R` | Internal helper for `cf_loco()` |
| `loco.R` | LOCO variable importance for ranger models |
| `omni_hetero.R` | Omnibus heterogeneity test battery |
| `plot_diag.R` | Multi-panel diagnostic plots |
| `plot_pdp.R` | Real partial dependence plots (1-way and 2-way) |
| `plot_scatter.R` | CATE scatter plots (individual OOB CATEs vs. covariate) |
| `plot_inter.R` | Hex interaction plots (deprecated) |
| `plot_rank.R` | Ranked CATEs with confidence intervals |

## Dev Conventions

- **Documentation:** roxygen2 (with markdown enabled)
- **Tests:** testthat (edition 3) — minimal coverage currently
- **Site:** pkgdown
- **Style:** Standard R package layout (`R/`, `man/`, `tests/`)

## Dependencies

- **Imports:** `grf` (≥ 2.3.2), `rlang`
- **Suggests:** testthat, roxygen2, ggplot2, ggdist, ggExtra, gridExtra, hexbin, MLbalance, ranger

## Current State

- v0.3.0
- S3 `summary()` and `plot()` methods for causal forests
- Core LOCO importance and omnibus heterogeneity test are functional
- Pruned from 13 to 8 exported functions; archived extras in `R/old/`

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
