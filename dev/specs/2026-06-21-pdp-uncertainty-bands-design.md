# Design memo: principled uncertainty bands for `plot_pdp()`

**Status:** DESIGN — open decisions pending (see §8). No code written.
**Date:** 2026-06-21
**Owner:** Jack
**Files in scope:** `R/plot_pdp.R` (`.compute_pd` L140–152; 1-way grid `seq(min,max)` L183–184; existing ATE band L268–279)
**Provenance:** brainstorm + adversarial Jacoby audit (31 agents, grounded in real R against `grf 2.6.1`) + methods research (interflex, conformal, estimand reconciliation; HMX 2019, Lei–Candès 2021). This memo exists so we DO NOT re-run that work.

---

## 1. Goal

Add principled pointwise (and ideally simultaneous) uncertainty bands to `plot_pdp()` for a fitted `grf::causal_forest`. Current `plot_pdp()` draws only a point estimate of the partial-dependence curve
`PD(v) = (1/m) * sum_j tauhat(v, X_{-,j})`
(stack m copies of X.sub, overwrite focal col with grid value v, `predict()$predictions`, block-average). Only existing band is the ATE ± 1.96·SE *horizontal* region.

---

## 2. THE KEY INSIGHT — 4 distinct estimands, not interchangeable

A band on the PDP curve can cover one of four different things. This distinction drives every other decision.

| Target | Estimand | Covers (what user learns) | Bias-robust? | Method family |
|---|---|---|---|---|
| **A** | `E_X[tau(v,X_-)]` — TRUE mean PD | "is there real heterogeneity in v" (what users THINK they read) | **NO** — variance-only centers on the biased/shrunk forest; honest for truth only if sq-bias = o(var). The audit's 4% boundary coverage IS this failing. | bias-aware: undersmoothing / 2nd-derivative bound (rdhonest/plrd-style) |
| **B** | conformal PREDICTION band | where a NEW individual's effect lands at v | YES — but covers a *new draw*, not a mean; much wider; different object | conformal (split/CQR; Lei–Candès for ITE) |
| **C** | interflex marginal-effect curve | a *different estimator's* (kernel/binning) ME(v)=∂E[Y|D,v]/∂D | bias-robust only by ABANDONING the forest | interflex (separate estimator) |
| **D** | `E_X[tauhat(v,X_-)]` — FOREST's regularized plug-in PD | uncertainty in the *fitted* curve (honest, not the truth) | trivially — absorbs bias into the estimand | variance-only / bootstrap (single fit) |

**Bottom line:** the user's mental model is A. Variance/bootstrap methods can only honestly deliver **D**. Delivering **A** requires a bias-aware build (large). Conformal (**B**) and interflex (**C**) silently SWAP the estimand — opt-in + relabel only.

---

## 3. Adversarial audit findings (on the original IF design)

Original proposal: IF/forest-weight representation `PD(v)=sum_i wbar_i(v)*Gamma_i` with `Gamma_i=get_scores(cf)`, `wbar_i(v)=(1/m)sum_j alpha_i(v,X_-j)`, `Vhat=sum_i wbar_i^2 (Gamma_i-PD)^2`, plus a "conservative" Cauchy–Schwarz fallback. **Verdict: do not ship as specified.** 7 CRITICAL, 10 MAJOR confirmed, numbers reproduced in R.

### CRITICAL
1. **Estimand mismatch (F1).** `predict.causal_forest` solves a LOCAL weighted moment (x-dependent denominator): `tauhat(x)=[sum a_i(x)(Y-Yhat)(W-What)]/[sum a_i(x)(W-What)^2]`. `sum alpha_i(x)*Gamma_i` (ATE-anchored scores) equals it only under homogeneous local treatment variance. Reproduced: `e=plogis(2.5*X1)` → local denom 0.086–0.219 vs global 0.135; `predict()` tracked local ratio to 0.008 while `alpha%*%Gamma` diverged 25–30%, persisting at 4000 trees. **A 95% CI that need not cover its own plotted line.**
2. **grf API ERROR in spec.** `get_forest_weights(cf, newdata)` returns `[n_eval x n_train]`, **ROWS sum to 1** — NOT `[n_train x n_eval]` "sums to 1 over i" as the spec said. Spec'd reduction averages the wrong axis → R silently RECYCLES a length-m vector against length-n_train scores (−0.0086 vs correct 0.373), no error thrown → fallback never fires. **Remember this orientation.**
3. **No cluster handling.** `Vhat=sum wbar_i^2(Gamma_i-PD)^2` is the iid sandwich. `grf::average_treatment_effect` aggregates AIPW scores to CLUSTER level before squaring (`sparse.model.matrix(~factor(clusters))`, sum-then-square, `n.adj/(n.adj-1)`). `get_scores()` returns per-row scores even under clusters. Proposed Vhat is the one-obs-per-cluster special case → SE ~17% (mild) to 2.7× (cluster-CATE) too narrow. autocf/glmcf/tabcf permit clusters; Engine C never triggers on cluster presence. Detect via `length(cf$clusters)>0` (unclustered is `integer(0)`, NOT NULL).
4. **Validation riggable.** DGP `tau=x1` (true PD(v)=v): coverage 82% at v=0 but **4% at v=2.3** — forest boundary/shrinkage bias (edge bias −0.81 ≈ 5×SE), invisible to ANY variance engine; conservative also 7% at edge. 1-way PDP does NOT trim hull. Structural test "conservative width ≥ IF width" is near-tautological (passes buggy anti-conservative code).

### MAJOR (selected)
- Fixed-weights variance understates EVERYWHERE (SEhat/MCsd ≈ 0.57–0.76, worst at center, NOT just edges) — omits weight-estimation variance.
- Curvature undercoverage: `tau=3*exp(-2*X1^2)` → peak est 2.84 vs 3.00, bias/SE≈1.65, peak coverage ≈64% (smoothing bias AIPW debiasing does NOT fix).
- Ignores `observation_weights(cf)` (sample/inverse-cluster weights) → PD line desyncs from on-plot ATE band; reproduced 0.4436 vs 0.4781.
- Overlap: `debiasing.weights=(W-What)/(What(1-What))` explodes as What→0/1 (What=1.0 → NaN, kurtosis~73). grf warns at min≤0.05/max≥0.95; PDP path bypasses. NaN ≠ caught error → fallback doesn't fire.
- "Always valid" (Engine C) is FALSE — inherits grf finite-tree IJ downward bias; grf docs themselves recommend more trees for valid CIs.
- Center≠plotted-line incoherence (~0.015 gap, persistent, not MC noise).

### Refuted sub-claim
- "grf IJ predict variance ignores clustering" — REFUTED: `predict(estimate.variance=TRUE)` IS cluster-aware (~44% inflation under clusters).

---

## 4. Method research findings

### Conformal — does NOT solve the audit's bias problem (category error)
- The audit = undercoverage of the TRUE MEAN at edges/curvature (bias-of-the-mean). Conformal is bias-robust because it covers a NEW DRAW, not a mean — "fixes" coverage by changing what's covered.
- **No conformal procedure targets an averaged/marginal functional like a PDP.** A PDP integrates out X → expectation → CI problem (CLT/IF/delta), not a prediction-interval problem.
- CATE conformal hits the **fundamental problem of causal inference** (tau_i never observed). Plain split conformal cannot calibrate on CATE residuals. Only Lei–Candès (2021) weighted+nested conformal applies — doubly-robust (valid if EITHER propensity OR outcome quantiles good), needs overlap 0<e(x)<1, calibration holdout; yields a PER-UNIT band, not a v-curve. Heavy, not off-the-shelf.
- Genuinely useful, opt-in + relabeled only: cheap split/CQR PREDICTION band on the OUTCOME (Y-hat) forest via `grf::quantile_forest` + `conformalInference::conformal.pred.split` (already a Suggest, used in `loco.R`).

### interflex (HMX 2019) — copy the band machinery, NOT the estimator
- interflex = kernel local-linear + binning estimators of a marginal-effect curve, with bootstrap CIs. It is a SEPARATE estimator on (Y,D,X), not the forest. Do not overlay on the forest curve (silent estimand swap). Do NOT copy its "forest zero-slice" anti-pattern (fix covariates to 0, native ±1.96σ — inherits bias + drops marginalization).
- **COPY these:**
  1. **Equal-mass quantile grid + support trimming** — evaluate focal grid on within-support quantiles; trim/flag sparse tails instead of extrapolating. Directly mitigates the 4%-boundary regime.
  2. **sup-t uniform-band helper** (Montiel Olea & Plagborg-Møller 2019): (i) percentile search for symmetric tail prob s.t. whole bootstrap curves fall inside jointly, or (ii) MVN draw with max standardized deviation as critical value (needs cross-point covariance). Upgrades pointwise → simultaneous; drops onto bootstrap curves or an IJ covariance.
  3. **Binning + tercile-interaction Wald test** as a near-free linearity/shape diagnostic.

### Comparison table
| Method | Interval type | Bias-robust? | Fit pattern | grf feasibility | Pointwise/sim | Cost |
|---|---|---|---|---|---|---|
| IF/delta (orig) | CI for D, sold as A | No | single fit | direct; BUT mean of per-row var /m ignores cross-row covariance → understates SE; grf exposes no covariance for an average-of-predictions | pointwise; sup-t needs max-stat | low |
| Bootstrap-refit (cluster/half-sample) | CI for D, honest | absorbs bias into D (still not A) | B refits | cleanest drop-in; recompute whole PD curve over B resamples, percentile bands; `clusters=` | NATURAL sup-t | high, parallel |
| Conformal split/CQR | prediction interval, new Y | yes | 1 fit + holdout | OUTCOME forest only; NOT CATE (ITE unobserved) | marginal; Bonferroni over grid | low |
| Conformal ITE (Lei–Candès) | prediction interval, fresh tau_i | yes (DR) | several fits | from grf parts, not off-the-shelf; per-unit not v-curve | marginal | highest |
| interflex kernel+boot | CI for smoother's ME (C) | by redefining estimand | separate est + B boot | NOT from causal_forest; cross-check only | pointwise; sup-t helper | moderate |

---

## 5. RECOMMENDATION (research's, leader concurs)

"IF-first" does NOT survive the audit; conformal is NOT the rescue. Honest, decisive path:

1. **Default = cluster/half-sampling BOOTSTRAP band for the forest PD, labeled Target D** ("uncertainty in the forest's regularized curve, not proof of the true effect"). Only thing a feasible method covers honestly; natural sup-t; clean drop-in on fitted `causal_forest` with `clusters=`; FIXES the IF cross-row-covariance SE bug by construction.
2. **Keep IF/delta as fast opt-in** — but (a) fix the averaging SE to account for cross-row covariance, (b) relabel as D not truth.
3. **Conformal = explicitly-relabeled PREDICTION band, opt-in only.** Outcome-PDP via `quantile_forest`/`conformalInference` first (cheap, available); Lei–Candès ITE as documented heavier path. Never under a "CI" label.
4. **Bias-aware Target-A CI = documented FUTURE item** (undersmoothing / 2nd-deriv bound). The only honest path to covering the TRUTH and directly answering the audit; largest build. Do not block default on it.
5. **Copy interflex** equal-mass support trimming + sup-t helper regardless of engine.

### Proposed API surface
`plot_pdp(..., se.method = , interval.type = "pointwise", level = 0.95, trim = )`

| `se.method` | Delivers | Estimand label | Status |
|---|---|---|---|
| `"bootstrap"` **(default)** | cluster/half-sampling percentile band over refit PD curves | D | ship |
| `"if"` | IF/delta-method, covariance-correct, fast | D (relabeled) | ship, fix SE |
| `"conformal"` | split/CQR PREDICTION band on outcome PDP | prediction interval | opt-in |
| `"conformal-ite"` | Lei–Candès ITE prediction band (needs propensity+overlap) | prediction interval | opt-in, heavy |
| `"none"` | curve only | — | ship |

- `interval.type = c("pointwise","simultaneous")` — `"simultaneous"` uses sup-t helper on bootstrap curves (or max-t on IF covariance). Applies to `"bootstrap"`/`"if"`; conformal is marginal-only (error or Bonferroni+warning).
- `trim` / equal-mass focal grid + sparse-support WARNING regardless of `se.method`.
- Auto-route to a safe path (with one-line `cli` message) when an engine's preconditions fail: clusters present (use cluster-aware variance/bootstrap), non-finite scores, `range(cf$W.hat)` outside [0.05,0.95], non-trivial `observation_weights`, or `get_scores`/`get_forest_weights` error — NOT only on function error.

---

## 6. Validation requirements (acceptance gate before merge)

Gated `UTOPIA_RUN_SLOW_TESTS=1` + `skip_on_cran()` (mirror `test-h2-crossfit-typeI.R`).
- Coverage reported **edge quantiles (≤2%, ≥98%) vs interior SEPARATELY** — never pooled.
- DGPs: (a) clustered + cluster-level CATE heterogeneity; (b) real curvature in `x_var`; (c) sample-weighted; (d) `equalize.cluster.weights`; (e) propensity heterogeneous in focal covariate (the F1 trigger); (f) poor overlap.
- Validate the SAME estimand the line displays (D, unless A is built).
- Cluster-aware variance must reproduce `average_treatment_effect()` SE to ~3 digits in the uniform-weight limit.
- Fast structural tests: `line == CI_center` within tol; identity `sum wbar*Gamma ≈ block-mean predict()` in the clean unclustered well-overlap case (~1.5e-3 at 2000 trees); SE shrinks ~1/sqrt(n); ESS map `1/sum wbar^2` flags low-support v; overlap warning fires + safe path engages. **DROP** "conservative ≥ IF width" as a primary signal.

---

## 7. grf API facts to remember (verified vs grf 2.6.1)
- `get_forest_weights(cf, newdata)` → `[n_eval x n_train]`, **rows sum to 1**. `wbar(v_g)=colMeans(alpha[block,])`. Assert `ncol(alpha)==length(Gamma)==nrow(cf$X.orig)`, `rowSums≈1`, finiteness; else fall back.
- `get_scores(cf)` → per-row DR-AIPW scores (same as ATE), per-row even under clusters.
- `predict.causal_forest(estimate.variance=TRUE)$variance.estimates` → pointwise IJ variance; IS cluster-aware; needs many trees; can be unstable/biased low at small num.trees.
- `average_treatment_effect` variance: cluster-level aggregation, `n.adj/(n.adj-1)`, honors `equalize.cluster.weights` + `observation_weights`.
- Cluster detection: `length(cf$clusters)>0` (unclustered = `integer(0)`, not NULL).
- `tauhat(x)` is a LOCAL RATIO (x-dependent denominator) — `sum alpha*Gamma` is only an approximation.
- `observation_weights(cf)` exists; weights both estimate and variance.

---

## 8. OPEN DECISIONS — pending user (resume here)

1. **Estimand framing (the crux).** Options: (a) **D now, A later** [leaning] — ship D-labeled default, scope bias-aware A as follow-up; (b) accept D, no A planned; (c) demand true-A now (blocks shipping, big build).
2. **Default engine.** Options: (a) **bootstrap default, IF opt-in** [leaning]; (b) bootstrap only; (c) covariance-correct IF default.
3. **Conformal in v1.** Options: (a) **defer entirely** [leaning]; (b) outcome-PDP only (cheap, relabeled); (c) full (outcome + Lei–Candès ITE).
4. Bands on by default (`interval` default TRUE/FALSE) + whether the plotted LINE switches to PD_IF when an IF/D band is on (coherence: line must == CI center).

Leader recommendation if forced: 1(a) D-now-A-later, 2(a) bootstrap-default+IF-opt-in, 3(a) defer conformal, 4 interval opt-in (default off) with line==center guaranteed when on. Next step after decisions: write spec → `writing-plans`.

---

## 9. Scope (agreed)
- 1-way PDP only for v1; 2-way deferred (hard to display uncertainty on a tile).
- Grouped (`color.var`) PDP: ribbon per group.
- Keep machinery that survives: forest-weight×AIPW representation is real; Cauchy–Schwarz bound correct AS A BOUND; gated MC sim concept; per-group ribbons; reuse ATE-band drawing pattern (L268–279).

---

## 10. References
- Friedman (2001) PDP, Ann. Statist. 29(5).
- Wager & Athey (2018) JASA — asymptotic normality + IJ variance.
- Athey, Tibshirani & Wager (2019) Ann. Statist. — GRF, honesty, subsample-rate bias condition.
- Hainmueller, Mummolo & Xu (2019) Political Analysis 27(2):163–192 — interflex.
- Montiel Olea & Plagborg-Møller (2019) — sup-t simultaneous bands.
- Lei, G'Sell, Rinaldo, Tibshirani & Wasserman (2018) JASA — split conformal.
- Romano, Patterson & Candès (2019) NeurIPS — CQR.
- Lei & Candès (2021) JRSS-B 83(5):911–938 — conformal counterfactuals/ITE.
- Tibshirani, Foygel Barber, Candès, Ramdas (2019) — weighted conformal.
