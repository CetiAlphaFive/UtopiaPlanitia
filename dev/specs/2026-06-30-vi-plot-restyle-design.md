# Design: VI plot restyle (plot.cf_loco, plot.cf_perm)

**Date:** 2026-06-30
**Feature slug:** `vi-plot-restyle`
**Paired plan:** `dev/plans/2026-06-30-vi-plot-restyle.md`
**Reference:** `~/Dropbox/Neil_Dem_Sat_Project/replication_materials/scripts/9.vi_appendix.R`
(`generate_vi_plot`), rendered `paper/figures/appendix/vi.ordered_loco.pdf`.

## 1. Shared helper: a house VI theme + bar geometry

Add ONE internal builder (e.g. in a new `R/vi_plot_style.R` or appended to
`utopia_plot.R`) so both methods share identical styling and only differ in data /
inference overlays. No export.

### 1.1 theme_few without the dependency (Decision 1: hand-roll)

`theme_few()` (ggthemes) is `theme_bw()` with the panel grid removed and a thin
gray border. Replicate from base ggplot2 to avoid adding `ggthemes` to `Suggests`:

```r
.utopia_theme_vi <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
  ggplot2::theme(
    panel.grid.minor   = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color = "gray90", linetype = "solid"),
    panel.border       = ggplot2::element_rect(color = "gray70", fill = NA, linewidth = 0.5),
    axis.text.y        = ggplot2::element_text(size = 9,  color = "black"),
    axis.text.x        = ggplot2::element_text(family = "serif", size = 9, color = "black"),
    axis.title         = ggplot2::element_blank(),
    plot.title         = ggplot2::element_text(hjust = 0.5, face = "bold",
                                               size = 13, color = "black"),
    legend.position    = "none",
    axis.ticks.length  = ggplot2::unit(-0.15, "cm"),   # inward ticks (Tufte)
    axis.ticks.x       = ggplot2::element_line(color = "black"),
    axis.ticks.y       = ggplot2::element_blank(),
    plot.margin        = ggplot2::margin(t = 4, r = 8, b = 2, l = 2, unit = "pt"))
}
```

Font sizes bumped from the grid's 6/8 to 9/13 for a standalone figure. If exact
`theme_few()` fidelity is later preferred, swap the `theme_bw()` base for
`ggthemes::theme_few()` behind `requireNamespace("ggthemes")` — note it in the
docstring, but the hand-rolled version is the default to keep dependencies lean.

### 1.2 x-break helper (port verbatim)

```r
.utopia_vi_xbreaks <- function(v) {
  xspan <- range(c(0, v), na.rm = TRUE)
  pad   <- diff(xspan) * 0.02
  if (xspan[1] < 0) xspan[1] <- xspan[1] - pad
  xspan[2] <- xspan[2] + pad
  brks <- pretty(xspan, n = 3)
  list(breaks = brks, limits = range(brks))
}
```

### 1.3 Orientation / geometry

Match the reference exactly: `aes(x = importance, y = variable)` (NO `coord_flip`;
the reference maps the bar horizontally directly), variable factor reordered
descending so the largest importance sits at the top:

```r
df$Variable <- factor(df$Variable,
                      levels = df$Variable[order(df$Importance)])  # asc -> top is max
geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4)
geom_col(alpha = 0.75, fill = <fill>)
geom_point(size = 2.5, shape = 16, color = "black")              # black tip point
scale_x_continuous(breaks = brks, limits = lims, expand = expansion(mult = c(0, 0.02)))
```

(Slightly larger point, 2.5, for a single panel vs the grid's 2.)

## 2. plot.cf_loco() (R/cf_loco_methods.R:78-109)

No inference -> plain bar + tip point in the house color.

- Signature (Decision 3, additive): `plot.cf_loco <- function(x, fill = "#1f78b4", ...)`.
  Default `#1f78b4` (the reference's 1996 blue). Backwards compatible (new arg
  after `x`, before `...`; old calls `plot(vi)` unchanged).
- Body: build `df` from `x$vimp`, reorder factor, compute breaks via
  `.utopia_vi_xbreaks(df$Importance)`, assemble:

```r
ggplot2::ggplot(df, ggplot2::aes(x = .data[["Importance"]], y = .data[["Variable"]])) +
  ggplot2::geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4) +
  ggplot2::geom_col(alpha = 0.75, fill = fill) +
  ggplot2::geom_point(size = 2.5, shape = 16, color = "black") +
  ggplot2::scale_x_continuous(breaks = brks, limits = lims,
                              expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::labs(title = "LOCO Variable Importance", x = NULL, y = NULL) +
  .utopia_theme_vi()
```

- Keep the existing `requireNamespace("ggplot2")` guard and the normalized-axis
  note (move "(normalized)" into the title or a subtitle since axis titles are now
  blank, e.g. `title = if (x$normalized) "LOCO Variable Importance (normalized)"
  else "LOCO Variable Importance"`).

## 3. plot.cf_perm() (R/cf_perm_methods.R:78-111)

Keep the inference. Two overlays on the same base:

- **Significance -> bar fill** (Decision 2): `Significant <- !is.na(CI.lower) & CI.lower > 0`.
  `geom_col(aes(fill = Significant), alpha = 0.75)` with
  `scale_fill_manual(values = c(`FALSE` = "gray70", `TRUE` = fill.sig))`,
  `legend.position = "none"` (style match; significance is legible from color +
  the whisker crossing zero). Default `fill.sig = "#1f78b4"`.
- **One-sided lower CI whisker** (horizontal, since x = importance): for rows with
  finite `CI.lower`,
  `geom_segment(aes(x = CI.lower, xend = Importance, y = Variable, yend = Variable),
  color = "grey40", linewidth = 0.5)`. (`CI.upper` is `+Inf` by construction, so no
  upper whisker — same as today.)
- **Black tip point** at `x = Importance` as in cf_loco.
- Normalized objects: `CI.lower` is `NA`, so no whisker and all bars take the
  non-significant fill; title gets "(normalized)". Same degradation as today.
- x-breaks computed over `c(Importance, CI.lower)` so the whisker is never clipped:
  `.utopia_vi_xbreaks(c(v$Importance, v$CI.lower[is.finite(v$CI.lower)]))`.
- Signature (additive): `plot.cf_perm <- function(x, fill.sig = "#1f78b4", ...)`.

Title stays "PermuCATE Variable Importance".

## 4. Dependencies

- No new hard or Suggests dependency under the recommended hand-rolled theme.
  `ggplot2` remains a guarded `Suggests` (unchanged). `serif` font family is base.
- If the team later wants exact `theme_few()`, add `ggthemes` to `Suggests` and
  guard; out of scope for the default implementation.

## 5. Tests

- Audit existing plot assertions: `tests/testthat/test-cf_loco*.R`,
  `tests/testthat/test-cf_perm.R`, `helper-cf_perm.R` for any `expect` on geom
  layers / panel fill that the restyle would break; update them.
- No vdiffr / snapshot images currently exist (`tests/testthat/_snaps/` absent) —
  do NOT introduce pixel snapshots (fragile across ggplot2 versions). Instead
  assert structure with `skip_if_not_installed("ggplot2")`:
  - returns a `ggplot` object;
  - has a `GeomCol` and a `GeomPoint` layer, and a `GeomVline` (zero line);
  - cf_perm: a `GeomSegment` layer present when CIs finite, absent when normalized;
  - cf_perm: `Significant` mapping splits fills (>=1 significant in the signal DGP).
- Keep tests fast (small forests via existing `make_test_cf()`).

## 6. Docs + NEWS

- Update roxygen `@details` on both `plot` methods to describe the new style
  (white background, colored bar + black tip point, zero reference line, one-sided
  CI whisker for cf_perm) and document the new `fill` / `fill.sig` argument(s).
  Regenerate `man/` via `devtools::document()`.
- `NEWS.md` (development-version heading): "`plot.cf_loco()` and `plot.cf_perm()`
  restyled to the house variable-importance look (theme_few-style white panel,
  colored bar with a black tip point, zero reference line, inward serif ticks);
  `cf_perm` retains its one-sided CI whisker and significance shading. New `fill`
  / `fill.sig` arguments. `summary()` output unchanged." Note the visual change is
  not an API change (return type and signatures backwards compatible).

## 7. Risk / compat

- **Not an API break**: methods still return a `ggplot`; new args are appended
  with behavior-preserving defaults. The API-stability guards pin compute-function
  formals, not plot aesthetics.
- **Visual reproducibility**: anyone re-rendering a figure gets the new look. This
  is intended and flagged in NEWS; no stored results are invalidated (plots are
  not data).
- **Font availability**: `family = "serif"` is a base R alias (Times-like) present
  on all platforms; safe.
