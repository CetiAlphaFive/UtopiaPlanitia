# Restyle plot.cf_loco() and plot.cf_perm() to the Dem-Sat appendix style

**Date:** 2026-06-30
**Feature slug:** `vi-plot-restyle`
**Status:** plan
**Paired design:** `dev/specs/2026-06-30-vi-plot-restyle-design.md`
**Scope:** the `plot()` S3 methods for `cf_loco` and `cf_perm` only. The
`summary()` / `print()` methods for both are fine and are NOT touched.

## What

Re-skin `plot.cf_loco()` and `plot.cf_perm()` to match Jack's variable-importance
appendix figures from the Dem-Sat paper. Replace the current gray-panel lollipop
look with the Tufte-style **colored horizontal bar + black tip point** on a white
`theme_few()` background, a gray zero reference line, inward serif axis ticks, and
faint x-only gridlines.

## Why

The current `|> plot()` output for both methods (gray `#e6e6e6` panel, teal/sig
lollipop, no zero line) does not match Jack's house style and reads as generic.
The Dem-Sat appendix VI figures are the desired look and are already battle-tested
in a paper. `summary()` output is fine for both and stays as-is.

## Formatting reference (source of truth)

`~/Dropbox/Neil_Dem_Sat_Project/replication_materials/scripts/9.vi_appendix.R`,
functions `generate_vi_plot()` / `generate_alphabetical_vi_plot()`. Rendered target:
`paper/figures/appendix/vi.ordered_loco.pdf`. The per-panel recipe to port:

```r
ggplot(sub, aes(x = vip, y = varname)) +
  geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4) +
  geom_col(alpha = 0.75, fill = color) +
  geom_point(size = 2, shape = 16, color = "black") +
  theme_few() +
  scale_x_continuous(breaks = brks, limits = lims,
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = title_lbl, x = NULL, y = NULL) +
  theme(
    axis.text.y       = element_text(size = 6, color = "black"),
    axis.text.x       = element_text(family = "serif", size = 8, color = "black"),
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 12, color = "black"),
    legend.position   = "none",
    axis.ticks.length = unit(-0.15, "cm"),
    axis.ticks.x      = element_line(color = "black"),
    axis.ticks.y      = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "solid"),
    plot.margin       = margin(t = 4, r = 8, b = 2, l = 2, unit = "pt"))
# x breaks: xspan <- range(c(0, vip)); brks <- pretty(xspan, n = 3); lims <- range(brks)
```

Signature elements to keep: horizontal orientation, descending sort (largest at
top), **bar with alpha 0.75 + black tip point**, **gray60 zero vline**,
**inward serif ticks**, white background with **x-only gray90 gridlines**, bold
centered title, no legend, `pretty(n=3)` x breaks that always include 0.

## Adaptations for the package (single object vs 9-panel grid)

The Dem-Sat figure is a 3x3 small-multiples grid (per-year panels, per-year
colors, font size 6/8 to fit). The package methods plot ONE object, so:

- **One panel, one color** (no year facets / palette). Pick a single default fill.
- **Bump font sizes** for a standalone figure (grid used 6/8 because of 9 panels):
  axis text ~9, title ~13.
- **`cf_perm` keeps its inference.** The Dem-Sat plot shows point importance with
  no CIs; `cf_perm` carries one-sided lower CIs + significance, which are its whole
  value-add. Retain the one-sided lower-CI whisker and significance, restyled into
  the new theme (see design). `cf_loco` (no inference) is just bar + tip point.

## Open decisions (in the design doc)

1. **`ggthemes` dependency for `theme_few()`** — add `ggthemes` to `Suggests` and
   guard, OR hand-roll a `theme_few`-equivalent from base ggplot2 to avoid a new
   dependency. Recommend hand-roll (keeps the "ggplot2-only in Suggests" spirit).
2. **`cf_perm` significance encoding** — map bar fill to significance
   (significant = color, non-significant = gray) with no legend (style match), the
   zero line + whisker making significance visually obvious; or keep a small
   legend. Recommend fill-by-significance, no legend.
3. **Optional `fill` / `color` argument** on the plot methods (additive, after the
   existing args) so users can recolor. Recommend adding, default to the chosen
   house color.

## Non-goals

- No change to `summary()` / `print()` for either class.
- No change to `cf_loco()` / `cf_perm()` compute paths or return structures.
- No small-multiples / faceting in the package methods (single-object plots).
- No new hard dependency (ggplot2 stays a guarded Suggests).

## Acceptance

- `plot(cf_loco(cf))` and `plot(cf_perm(cf))` render in the Dem-Sat style
  (white theme_few-equivalent, colored bar + black tip point, gray zero line,
  inward serif ticks, x-only gray90 gridlines, bold centered title).
- `cf_perm` still shows the one-sided lower CI + significance.
- Both still return a `ggplot` object; no API/return-structure change.
- No new hard dependency; degrade gracefully if ggplot2 (and ggthemes, if used)
  absent.
- `NEWS.md` entry; any plot tests/snapshots updated; `devtools::check()` clean.
