## =============================================================================
## hte_bench_plots.R — Visualization for omni_hetero() Monte Carlo benchmark
##
## Run AFTER hte_bench.R has produced:
##   inst/sim/results/hte_bench_raw.rds
##   inst/sim/results/hte_bench_summary.csv
##
## Usage (from package root):
##   Rscript inst/sim/hte_bench_plots.R
##
## Outputs (all in inst/sim/results/):
##   hte_bench_ecdf.pdf       -- p-value ECDF under null (5 panels)
##   hte_bench_power_n.pdf    -- Power vs. n (one panel per DGP, 5 lines)
##   hte_bench_power_sigma.pdf -- Power vs. sigma.tau for nw1 (5 lines)
## =============================================================================

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)   # loads blp_rate_theme()
  library(ggplot2)
})

OUT_DIR <- file.path("inst", "sim", "results")

## -- Load results ------------------------------------------------------------

raw_path <- file.path(OUT_DIR, "hte_bench_raw.rds")
sum_path <- file.path(OUT_DIR, "hte_bench_summary.csv")

if (!file.exists(raw_path)) {
  stop("hte_bench_raw.rds not found. Run hte_bench.R first.")
}

raw     <- readRDS(raw_path)
summary_df <- read.csv(sum_path, stringsAsFactors = FALSE)

cat("Loaded", nrow(raw), "raw replication rows.\n")

## -- Palette and labels -------------------------------------------------------

test_order <- c("seq_rate", "calibration", "high_low", "oob_2sided", "oob_1sided")

TEST_PRETTY <- c(
  seq_rate    = "Sequential RATE",
  calibration = "Calibration/BLP",
  high_low    = "High vs. Low CATE",
  oob_2sided  = "OOB RATE (2-sided)",
  oob_1sided  = "OOB RATE (1-sided)"
)

# Named by pretty label (for ggplot scale_*_manual matching against factor levels)
TEST_COLORS <- setNames(
  c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a"),
  TEST_PRETTY[test_order]
)

TEST_LINETYPE <- setNames(
  c("solid", "solid", "dashed", "dotdash", "dotted"),
  TEST_PRETTY[test_order]
)

DGP_PRETTY <- c(
  "null"   = "Null (tau = 0)",
  "nw1"    = "Weak HTE (nw1)",
  "sparse" = "Sparse nonlinear HTE",
  "strong" = "Strong linear HTE"
)

## -- Shared theme (matches R/plot_blp.R::blp_rate_theme) ---------------------
## blp_rate_theme() is exported from the package via devtools::load_all().

sim_theme <- function() {
  blp_rate_theme() +
    ggplot2::theme(
      legend.position      = "bottom",
      legend.text          = ggplot2::element_text(size = 7),
      legend.title         = ggplot2::element_text(size = 7, face = "bold"),
      legend.key.width     = ggplot2::unit(1.2, "cm"),
      strip.text           = ggplot2::element_text(size = 8, face = "bold"),
      strip.background     = ggplot2::element_rect(fill = "grey95",
                                                    colour = "grey70")
    )
}

## =============================================================================
## Plot 1: p-value ECDF under null (5 panels, one per test)
## =============================================================================

# Subset to null DGP only
null_raw <- raw[raw$rep_ok & !is.na(raw$p_value) &
                  raw$scenario_lbl == "null", ]

null_raw$test_pretty <- TEST_PRETTY[null_raw$test]
null_raw$test_pretty <- factor(null_raw$test_pretty,
                                levels = TEST_PRETTY[test_order])
null_raw$n_label <- paste0("n = ", null_raw$n)

# Build ECDF data manually so we can add the uniform reference
ecdf_rows <- lapply(split(null_raw, list(null_raw$test, null_raw$n)), function(d) {
  if (nrow(d) == 0) return(NULL)
  p_sorted <- sort(d$p_value)
  data.frame(
    p_value    = p_sorted,
    ecdf_val   = seq_along(p_sorted) / length(p_sorted),
    test       = d$test[1],
    test_pretty = d$test_pretty[1],
    n          = d$n[1],
    n_label    = d$n_label[1],
    stringsAsFactors = FALSE
  )
})
ecdf_df <- do.call(rbind, Filter(Negate(is.null), ecdf_rows))

p_ecdf <- ggplot2::ggplot(ecdf_df,
  ggplot2::aes(x = p_value, y = ecdf_val, colour = n_label, group = n_label)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                       colour = "grey50", linewidth = 0.5) +
  ggplot2::geom_step(linewidth = 0.7) +
  ggplot2::facet_wrap(~ test_pretty, nrow = 2, ncol = 3) +
  ggplot2::scale_colour_manual(
    name   = "Sample size",
    values = c("n = 200" = "#2166ac", "n = 300" = "#d6604d")
  ) +
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  ggplot2::labs(
    title    = "p-value calibration under the null (tau = 0)",
    subtitle = paste0("ECDF vs. Uniform(0,1) reference (dashed). ",
                      "Each panel: one of 5 omni_hetero() tests. ",
                      "Mock scaffold: reps = 20."),
    x        = "p-value",
    y        = "Empirical CDF",
    caption  = paste0(
      "Note: Ideal calibration follows the dashed 45-degree line. ",
      "Curves bowing above the diagonal indicate anti-conservative tests ",
      "(excess null rejections); curves below indicate conservative tests. ",
      "OOB RATE (2-sided) is documented as anti-conservative (~30% null ",
      "rejection at large n); the mock scaffold has too few replications ",
      "(reps = 20) to resolve this precisely."
    )
  ) +
  sim_theme()

ecdf_path <- file.path(OUT_DIR, "hte_bench_ecdf.pdf")
ggplot2::ggsave(ecdf_path, p_ecdf, width = 9, height = 6, device = "pdf")
cat(sprintf("Saved: %s\n", ecdf_path))

## =============================================================================
## Plot 2: Power vs. n (main grid, 4 DGPs × 2 n-values)
## =============================================================================

main_sum <- summary_df[summary_df$sweep_type == "main" & !is.na(summary_df$rej_rate), ]
main_sum$dgp_pretty <- DGP_PRETTY[main_sum$scenario]
# Fix: dgp label for nw1 in summary uses "nw1", not "nw1_s0.15"
main_sum$dgp_pretty[is.na(main_sum$dgp_pretty)] <-
  main_sum$scenario[is.na(main_sum$dgp_pretty)]

main_sum$dgp_pretty <- factor(
  main_sum$dgp_pretty,
  levels = DGP_PRETTY
)
main_sum$test_pretty <- TEST_PRETTY[main_sum$test]
main_sum$test_pretty <- factor(main_sum$test_pretty, levels = TEST_PRETTY[test_order])

p_power_n <- ggplot2::ggplot(main_sum,
  ggplot2::aes(x = n, y = rej_rate,
               colour = test_pretty,
               linetype = test_pretty,
               group = test_pretty)) +
  ggplot2::geom_hline(yintercept = 0.05, linetype = "dotted",
                      colour = "grey40", linewidth = 0.4) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
    width = 8, linewidth = 0.4
  ) +
  ggplot2::facet_wrap(~ dgp_pretty, nrow = 2, ncol = 2) +
  ggplot2::scale_colour_manual(name = "Test", values = TEST_COLORS) +
  ggplot2::scale_linetype_manual(name = "Test", values = TEST_LINETYPE) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.2),
    labels = function(x) paste0(round(100 * x), "%")
  ) +
  ggplot2::scale_x_continuous(breaks = c(200, 300)) +
  ggplot2::labs(
    title    = "Rejection rate by DGP and sample size",
    subtitle = paste0("Dotted horizontal line: nominal alpha = 5%. ",
                      "Error bars: 95% MC CI. Mock scaffold: reps = 20, ",
                      "MC SE at p=0.05 approx 4.9%."),
    x        = "Sample size (n)",
    y        = "Rejection rate",
    caption  = paste0(
      "Note: With reps = 20, the 95% MC confidence interval at p = 0.05 ",
      "spans [0%, 14.9%]. Differences between tests are not interpretable ",
      "at this replication count. Top-left (Null) panel shows type-I error; ",
      "remaining panels show power. OOB RATE (2-sided) is anti-conservative ",
      "by design; its null rejection rate should exceed 5% systematically at large n."
    )
  ) +
  sim_theme()

power_n_path <- file.path(OUT_DIR, "hte_bench_power_n.pdf")
ggplot2::ggsave(power_n_path, p_power_n, width = 9, height = 7, device = "pdf")
cat(sprintf("Saved: %s\n", power_n_path))

## =============================================================================
## Plot 3: Power vs. sigma.tau (nw1 signal sweep, n = 300)
## =============================================================================

# Combine signal sweep rows + nw1 main grid row at n=300
sweep_sum <- summary_df[
  !is.na(summary_df$sigma_tau) &
  !is.na(summary_df$rej_rate) &
  summary_df$n == 300 &
  (summary_df$sweep_type == "signal_sweep" |
   (summary_df$sweep_type == "main" & summary_df$scenario == "nw1")),
]

sweep_sum$test_pretty <- TEST_PRETTY[sweep_sum$test]
sweep_sum$test_pretty <- factor(sweep_sum$test_pretty, levels = TEST_PRETTY[test_order])
sweep_sum <- sweep_sum[order(sweep_sum$sigma_tau, sweep_sum$test), ]

p_power_sig <- ggplot2::ggplot(sweep_sum,
  ggplot2::aes(x = sigma_tau, y = rej_rate,
               colour = test_pretty, linetype = test_pretty,
               group = test_pretty)) +
  ggplot2::geom_hline(yintercept = 0.05, linetype = "dotted",
                      colour = "grey40", linewidth = 0.4) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
    width = 0.02, linewidth = 0.4
  ) +
  ggplot2::scale_colour_manual(name = "Test", values = TEST_COLORS) +
  ggplot2::scale_linetype_manual(name = "Test", values = TEST_LINETYPE) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.2),
    labels = function(x) paste0(round(100 * x), "%")
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 0.15, 0.5, 1.0),
    labels = c("0\n(null)", "0.15\n(weak)", "0.50\n(moderate)", "1.0\n(strong)")
  ) +
  ggplot2::labs(
    title    = "Power vs. signal strength: nw1 DGP, n = 300",
    subtitle = paste0("sigma.tau controls CATE variation in the nw1 DGP. ",
                      "Dotted line: nominal alpha = 5%. Error bars: 95% MC CI. ",
                      "reps = 20; MC SE ~ 4.9% at p = 0.05."),
    x        = "sigma.tau (CATE standard deviation in nw1 DGP)",
    y        = "Rejection rate",
    caption  = paste0(
      "Note: sigma.tau = 0 is the null-equivalent for nw1. ",
      "Leftmost points show type-I error; rightward points show power. ",
      "Preferred tests (solid lines): Sequential RATE and Calibration/BLP. ",
      "Heuristic tests (dashed/dotted): High vs. Low CATE, OOB RATE variants. ",
      "With reps = 20, no power differences between tests are interpretable."
    )
  ) +
  sim_theme()

power_sig_path <- file.path(OUT_DIR, "hte_bench_power_sigma.pdf")
ggplot2::ggsave(power_sig_path, p_power_sig, width = 8, height = 5.5, device = "pdf")
cat(sprintf("Saved: %s\n", power_sig_path))

## =============================================================================
## Plot 4: Rejection rate heatmap table (test x scenario x n)
## =============================================================================

main_sum2 <- summary_df[summary_df$sweep_type == "main" & !is.na(summary_df$rej_rate), ]
main_sum2$scenario_n <- paste0(main_sum2$scenario, "\nn=", main_sum2$n)
main_sum2$test_pretty <- factor(TEST_PRETTY[main_sum2$test], levels = TEST_PRETTY[test_order])

# Order scenario_n sensibly
sc_order <- paste0(rep(c("null", "nw1", "sparse", "strong"), each = 2),
                   "\nn=", rep(c(200, 300), 4))
main_sum2$scenario_n <- factor(main_sum2$scenario_n,
                                levels = intersect(sc_order, main_sum2$scenario_n))

p_heat <- ggplot2::ggplot(main_sum2,
  ggplot2::aes(x = scenario_n, y = test_pretty, fill = rej_rate)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(round(100 * rej_rate), "%")),
    size = 3, colour = "white", fontface = "bold"
  ) +
  ggplot2::scale_fill_gradientn(
    colours  = c("#2166ac", "#f7f7f7", "#d6604d"),
    values   = c(0, 0.05, 1),
    limits   = c(0, 1),
    breaks   = c(0, 0.05, 0.25, 0.50, 0.75, 1),
    labels   = function(x) paste0(round(100 * x), "%"),
    name     = "Rejection\nrate"
  ) +
  ggplot2::labs(
    title    = "Rejection rate heatmap: test x scenario x n",
    subtitle = paste0("Mock scaffold (reps = 20); cell values = empirical ",
                      "rejection rate at alpha = 0.05."),
    x        = "Scenario (DGP x sample size)",
    y        = NULL,
    caption  = paste0(
      "Note: Blue = low rejection rate; red = high. ",
      "Null scenarios (left) show type-I error; others show power. ",
      "Preferred tests: rows 1-2. Heuristic tests: rows 3-5."
    )
  ) +
  sim_theme() +
  ggplot2::theme(
    axis.text.x  = ggplot2::element_text(size = 6.5),
    axis.text.y  = ggplot2::element_text(size = 7),
    legend.position = "right"
  )

heat_path <- file.path(OUT_DIR, "hte_bench_heatmap.pdf")
ggplot2::ggsave(heat_path, p_heat, width = 9, height = 4.5, device = "pdf")
cat(sprintf("Saved: %s\n", heat_path))

## -- Done --------------------------------------------------------------------

cat("\n=================================================================\n")
cat("hte_bench_plots.R complete.\n")
cat(sprintf("Output directory: %s\n", normalizePath(OUT_DIR)))
cat("Files:\n")
cat(sprintf("  %s\n", ecdf_path))
cat(sprintf("  %s\n", power_n_path))
cat(sprintf("  %s\n", power_sig_path))
cat(sprintf("  %s\n", heat_path))
cat("=================================================================\n")
