# tabcf(): Repeated K-fold Cross-Fitting + Propensity Clip Control

**Date:** 2026-06-16
**Status:** Approved design
**Target:** `R/tabcf.R`

## Motivation

`tabcf()` cross-fits TabPFN nuisances with a single random K-fold partition.
The held-out predictions inherit Monte-Carlo noise from that one partition.
Repeated K-fold CV (R repeats, average per-unit predictions) reduces this
variance. Separately, the current `eps` argument **always** clips binary
propensities to `[eps, 1-eps]` with no opt-out; we replace it with an explicit
clip control that defaults to no clipping + a loud overlap warning.

## Scope

Two coupled changes to `tabcf()`:
1. Add repeated cross-fitting via a new `R` argument.
2. Replace `eps` (always-clip) with a polymorphic `clip` argument; deprecate `eps`.

Out of scope: changes to the grf estimator, identification, cluster handling
logic (reused per repeat unchanged), or the TabPFN fit/predict path.

## 1. Repeated cross-fitting

### New argument
- `R` integer `>= 1`, default `1L`. Number of repeated K-fold partitions.
- Validation mirrors `K`: single non-NA integer, `R >= 1`, no upper bound.

### Semantics
- For each repeat `r in 1:R`: build a fresh K-fold partition, cross-fit TabPFN
  nuisances (existing per-fold logic), optionally clip (see section 2), yielding
  `Y.hat_r`, `W.hat_r` (length n).
- Final nuisances = elementwise mean over repeats:
  `Y.hat = mean_r Y.hat_r`, `W.hat = mean_r W.hat_r`.
  Mean of clipped values stays in range.
- grf refit consumes averaged nuisances. Estimator unchanged.

### Seeding (guarantees R=1 is byte-identical to current behavior)
- Fold partition for repeat `r`: `set.seed(seed + (r - 1))` before
  `.tabcf_make_folds()`. r=1 -> `set.seed(seed)` -> same partition as today.
- TabPFN `random_state` for fold k in repeat r: `seed + (r - 1) * K + k`.
  r=1 -> `seed + k` -> same as today.
- User-supplied `tabpfn_args$control` still passed verbatim (user owns seeding).
- Final grf `set.seed(seed)` unchanged.

### Failure handling
- Any non-finite `Y.hat_r`/`W.hat_r`, or binary classifier prob outside `[0,1]`,
  in any repeat -> abort with an error naming the offending repeat.
  (Same strictness as today, applied per repeat. No silent degradation.)

## 2. Propensity clip control

### New argument `clip`, default `FALSE`
- `FALSE` -> no clipping. Compute overlap check on final averaged binary `W.hat`:
  if any `< 0.01` or `> 0.99`, emit a loud `warning()` reporting count-outside
  and min/max `W.hat`. Fixed 0.01/0.99 threshold (not user-configurable).
- `TRUE` -> clip to default bound `[1e-3, 1 - 1e-3]`.
- `c(lo, hi)` numeric length-2 -> clip to `[lo, hi]`; validate `0 < lo < hi < 1`.

### `eps` deprecation
- `eps` kept in signature, default `NULL` (was `1e-3`).
- If caller passes non-NULL `eps`: emit base `warning()` (deprecation) and map to
  `clip = c(eps, 1 - eps)`. Validate `eps` in `(0, 0.5)`.
- Passing both non-NULL `eps` AND non-default `clip` -> error (ambiguous).
- No `lifecycle` dependency added for a single-arg deprecation.

### Interaction with repeats
- `clip` active (TRUE or range): clip each repeat's `W.hat` to `[lo,hi]`, then
  average. `clipped` = total clipped across all repeats.
- `clip = FALSE`: no per-repeat clipping; overlap warning fires once on averaged
  `W.hat`. `clipped = 0`.
- Continuous `W`: clip and overlap-warning both inactive (unchanged).

## Refactor

Extract the current single cross-fit loop (existing `R/tabcf.R` lines ~259-316)
into a helper so `tabcf()` can call it once per repeat:

- `.tabcf_crossfit_once(X, Y, W, w_type, K, clusters, fold_seed, tabpfn_random_base, clip_active, lo, hi, tabpfn_args, user_supplied_control, verbose)`
  -> `list(Y.hat, W.hat, clipped)`. Owns: fold build, per-fold fit/predict,
  postcondition checks, per-repeat clipping. Unit-testable without the repeat loop.
- `.tabcf_resolve_clip(clip, eps)` -> `list(active, lo, hi)`. Owns deprecation
  mapping, validation, and the eps/clip conflict error.
- Generalize `.tabcf_clip_propensity()` to take `lo`/`hi` instead of `eps`.
- `.tabcf_check_overlap(W.hat)` -> warn on `<0.01`/`>0.99`, report worst.

`tabcf()` body becomes: validate -> resolve clip -> loop `r in 1:R` accumulating
`Y.hat_r`/`W.hat_r`/clipped -> average -> overlap check (if clip=FALSE) -> refit
-> annotate.

## Metadata (`attr(out, "tabcf_meta")`)
- Add `R`.
- Replace `eps` with `clip` (resolved value: `FALSE` or `c(lo, hi)`).
- Keep `clipped` (total across repeats; 0 when clip=FALSE).
- Keep `seed`, `w_type`, `tuning`.

## Documentation
- `@param R`, `@param clip`; mark `@param eps` deprecated.
- Update cross-fitting `@details` paragraph to describe repeats + averaging.
- Note cost scales `R x K x 2` TabPFN calls.
- Update `@return` `tabcf_meta` description (`R`, `clip`).
- One example: `tabcf(cf, K = 5, R = 10)`.
- NEWS.md: loudly flag the default behavior change (binary-W no longer
  auto-clips; warns instead) and `eps` deprecation.

## Testing
- `R = 1` produces output identical to current implementation (regression guard).
- `R = 2` averages two partitions (mock `.tabcf_crossfit_once` -> assert mean).
- `R` validation errors (R=0, R=1.5, R=NA).
- `clip = FALSE`: no clipping; overlap warning fires + content correct.
- `clip = TRUE`: clips at `[1e-3, 1-1e-3]`.
- `clip = c(lo,hi)`: manual range; invalid ranges error.
- `eps` deprecation: warns and maps to `clip=c(eps,1-eps)`.
- `eps` + `clip` together: errors.
- `clipped` accumulates across repeats when clip active.
- Continuous W: clip + warning inactive.

## Backward compatibility
- `R = 1` + default `clip = FALSE`: only behavior change vs. today is clipping.
  Previously binary W always clipped at `1e-3`; now warn-only by default.
  Documented in NEWS.md. Numeric nuisance values otherwise identical for R=1
  when clipping is requested (`clip = TRUE` reproduces old `eps = 1e-3`).
