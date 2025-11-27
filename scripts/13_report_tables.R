# R/13_report_tables.R
# Goal: Export neat, publication-ready tables for the thesis.
#
# This script ONLY formats / exports tables.
# All heavy lifting (scoring, modelling, SenticNet, etc.) is done earlier.
#
# Inputs (produced by earlier scripts):
#   data/processed/sent_stop_summary.rds
#   data/processed/sent_chapter_summary.rds
#   data/processed/sent_stop_modelled.rds
#   outputs/tables/model_coefficients.csv
#   outputs/tables/model_fit.csv
#   outputs/tables/partial_correlations.csv
#   outputs/tables/lexicon_coverage_overall.csv
#   outputs/tables/lexicon_coverage_by_stop.csv
#   outputs/tables/senticnet_top_words.csv
#
# Outputs (all to outputs/tables/):
#   table_stop_sentiment_main.csv
#   table_chapter_sentiment_main.csv
#   table_model_coefficients_clean.csv
#   table_model_fit_clean.csv
#   table_partial_correlations_clean.csv
#   table_lexicon_coverage_overall_clean.csv
#   table_lexicon_coverage_by_stop_clean.csv
#   table_senticnet_examples_clean.csv

# ---- packages -----------------------------------------------------------

stopifnot(requireNamespace("here",   quietly = TRUE))
stopifnot(requireNamespace("readr",  quietly = TRUE))
stopifnot(requireNamespace("dplyr",  quietly = TRUE))
stopifnot(requireNamespace("stringr",quietly = TRUE))

# ---- output dir ---------------------------------------------------------

dir_tables <- here::here("outputs/tables")
dir.create(dir_tables, showWarnings = FALSE, recursive = TRUE)

# helper for safe round
rnd <- function(x, k = 3) round(x, digits = k)

# ========================================================================
# 1. Per-stop sentiment summary (main thesis table)
# ========================================================================

p_stop <- here::here("data/processed/sent_stop_summary.rds")
if (file.exists(p_stop)) {
  
  sent_stop <- readr::read_rds(p_stop)
  
  has_ci_stop <- all(c("ci_lo", "ci_hi") %in% names(sent_stop))
  
  tab_stop <- sent_stop %>%
    dplyr::arrange(order) %>%
    {
      if (has_ci_stop) {
        dplyr::transmute(
          .,
          Order    = order,
          Location = location,
          Mean     = rnd(mean_val_base_scored, 3),
          `95% CI` = sprintf("[%.3f, %.3f]", ci_lo, ci_hi),
          Coverage = sprintf("%.2f", coverage)
        )
      } else {
        dplyr::transmute(
          .,
          Order    = order,
          Location = location,
          Mean     = rnd(mean_val_base_scored, 3),
          Coverage = sprintf("%.2f", coverage)
        )
      }
    }
  
  readr::write_csv(
    tab_stop,
    file.path(dir_tables, "table_stop_sentiment_main.csv")
  )
  
  message("Saved: table_stop_sentiment_main.csv")
  
} else {
  warning("sent_stop_summary.rds not found; skipping stop-level table.")
}

# ========================================================================
# 2. Per-chapter sentiment summary (for appendix / robustness)
# ========================================================================

p_chap <- here::here("data/processed/sent_chapter_summary.rds")
if (file.exists(p_chap)) {
  
  sent_chap <- readr::read_rds(p_chap)
  
  has_ci_chap <- all(c("ci_lo", "ci_hi") %in% names(sent_chap))
  
  tab_chap <- sent_chap %>%
    dplyr::arrange(chapter) %>%
    {
      if (has_ci_chap) {
        dplyr::transmute(
          .,
          Chapter  = chapter,
          Mean     = rnd(mean_val_base_scored, 3),
          `95% CI` = sprintf("[%.3f, %.3f]", ci_lo, ci_hi),
          Coverage = sprintf("%.2f", coverage)
        )
      } else {
        dplyr::transmute(
          .,
          Chapter  = chapter,
          Mean     = rnd(mean_val_base_scored, 3),
          Coverage = sprintf("%.2f", coverage)
        )
      }
    }
  
  readr::write_csv(
    tab_chap,
    file.path(dir_tables, "table_chapter_sentiment_main.csv")
  )
  
  message("Saved: table_chapter_sentiment_main.csv")
  
} else {
  warning("sent_chapter_summary.rds not found; skipping chapter-level table.")
}

# ========================================================================
# 3. Regression model coefficients (region + plot proxy)
# ========================================================================

p_mod_coef <- here::here("outputs/tables/model_coefficients.csv")
if (file.exists(p_mod_coef)) {
  
  mod_coef <- readr::read_csv(p_mod_coef, show_col_types = FALSE)
  
  # Friendly labels for terms (light touch; you can tweak in Word/LaTeX)
  term_nice <- function(term) {
    term <- stringr::str_replace(term, "^\\(Intercept\\)$", "Intercept")
    term <- stringr::str_replace(term, "^region", "Region: ")
    term
  }
  
  tab_mod_coef <- mod_coef %>%
    dplyr::mutate(
      Term        = term_nice(term),
      Estimate    = rnd(estimate, 3),
      `Std. Error`= rnd(std.error, 3),
      `2.5%`      = rnd(conf.low, 3),
      `97.5%`     = rnd(conf.high, 3),
      `p-value`   = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    dplyr::select(
      Term, Estimate, `Std. Error`, `2.5%`, `97.5%`, `p-value`
    )
  
  readr::write_csv(
    tab_mod_coef,
    file.path(dir_tables, "table_model_coefficients_clean.csv")
  )
  
  message("Saved: table_model_coefficients_clean.csv")
  
} else {
  warning("model_coefficients.csv not found; skipping model coefficient table.")
}

# ========================================================================
# 4. Overall model fit (R² etc.)
# ========================================================================

p_mod_fit <- here::here("outputs/tables/model_fit.csv")
if (file.exists(p_mod_fit)) {
  
  mod_fit <- readr::read_csv(p_mod_fit, show_col_types = FALSE)
  
  tab_fit <- mod_fit %>%
    dplyr::transmute(
      `R-squared`      = rnd(r.squared, 3),
      `Adj. R-squared` = rnd(adj.r.squared, 3),
      `Residual SE`    = rnd(sigma, 3),
      `F statistic`    = rnd(statistic, 3),
      `Num. obs.`      = as.integer(df.residual + dplyr::n()) # rough count
    )
  
  readr::write_csv(
    tab_fit,
    file.path(dir_tables, "table_model_fit_clean.csv")
  )
  
  message("Saved: table_model_fit_clean.csv")
  
} else {
  warning("model_fit.csv not found; skipping model fit table.")
}

# ========================================================================
# 5. Partial correlations
# ========================================================================

p_partial <- here::here("outputs/tables/partial_correlations.csv")
if (file.exists(p_partial)) {
  
  partial <- readr::read_csv(p_partial, show_col_types = FALSE)
  # expected cols: comparison, estimate, p_value, statistic, n
  
  tab_partial <- partial %>%
    dplyr::transmute(
      Comparison  = comparison,
      Correlation = rnd(estimate, 3),
      `p-value`   = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)),
      Statistic   = rnd(statistic, 3),
      n           = n
    )
  
  readr::write_csv(
    tab_partial,
    file.path(dir_tables, "table_partial_correlations_clean.csv")
  )
  
  message("Saved: table_partial_correlations_clean.csv")
  
} else {
  warning("partial_correlations.csv not found; skipping partial correlation table.")
}

# ========================================================================
# 6. Lexicon coverage (overall + by stop)
# ========================================================================

# ---- overall coverage ----
p_cov_overall <- here::here("outputs/tables/lexicon_coverage_overall.csv")
if (file.exists(p_cov_overall)) {
  
  cov_overall <- readr::read_csv(p_cov_overall, show_col_types = FALSE)
  
  has_token_cov  <- "token_coverage" %in% names(cov_overall)
  has_type_cov   <- "type_coverage"  %in% names(cov_overall)
  has_type_counts <- all(c("n_scored_types", "n_types") %in% names(cov_overall))
  
  cov_overall <- cov_overall %>%
    dplyr::mutate(
      token_coverage = if (has_token_cov) token_coverage
      else n_scored_tokens / n_tokens,
      type_coverage  = if (has_type_cov) {
        type_coverage
      } else if (has_type_counts) {
        n_scored_types / n_types
      } else {
        NA_real_
      }
    )
  
  tab_cov_overall <- cov_overall %>%
    dplyr::transmute(
      Lexicon          = lexicon,
      `Tokens (total)` = n_tokens,
      `Types (total)`  = if ("n_types" %in% names(.)) n_types else NA_integer_,
      `Scored tokens`  = n_scored_tokens,
      `Scored types`   = if ("n_scored_types" %in% names(.)) n_scored_types else NA_integer_,
      `Token coverage` = sprintf("%.3f", token_coverage),
      `Type coverage`  = ifelse(is.na(type_coverage),
                                NA_character_,
                                sprintf("%.3f", type_coverage))
    )
  
  readr::write_csv(
    tab_cov_overall,
    file.path(dir_tables, "table_lexicon_coverage_overall_clean.csv")
  )
  
  message("Saved: table_lexicon_coverage_overall_clean.csv")
  
} else {
  warning("lexicon_coverage_overall.csv not found; skipping overall coverage table.")
}

# ---- by-stop coverage ----
p_cov_stop <- here::here("outputs/tables/lexicon_coverage_by_stop.csv")
if (file.exists(p_cov_stop)) {
  
  cov_stop <- readr::read_csv(p_cov_stop, show_col_types = FALSE)
  
  has_token_cov_stop  <- "token_coverage" %in% names(cov_stop)
  has_type_cov_stop   <- "type_coverage"  %in% names(cov_stop)
  has_type_counts_stop <- all(c("n_scored_types", "n_types") %in% names(cov_stop))
  
  cov_stop <- cov_stop %>%
    dplyr::mutate(
      token_coverage = if (has_token_cov_stop) token_coverage
      else n_scored_tokens / n_tokens,
      type_coverage  = if (has_type_cov_stop) {
        type_coverage
      } else if (has_type_counts_stop) {
        n_scored_types / n_types
      } else {
        NA_real_
      }
    )
  
  tab_cov_stop <- cov_stop %>%
    dplyr::arrange(order, lexicon) %>%
    dplyr::transmute(
      Order            = order,
      Location         = location,
      Lexicon          = lexicon,
      `Token coverage` = sprintf("%.3f", token_coverage),
      `Type coverage`  = ifelse(is.na(type_coverage),
                                NA_character_,
                                sprintf("%.3f", type_coverage))
    )
  
  readr::write_csv(
    tab_cov_stop,
    file.path(dir_tables, "table_lexicon_coverage_by_stop_clean.csv")
  )
  
  message("Saved: table_lexicon_coverage_by_stop_clean.csv")
  
} else {
  warning("lexicon_coverage_by_stop.csv not found; skipping by-stop coverage table.")
}


# ========================================================================
# 7. SenticNet examples table
# ========================================================================

p_sentic <- here::here("outputs/tables/senticnet_top_words.csv")
if (file.exists(p_sentic)) {
  
  sentic_top <- readr::read_csv(p_sentic, show_col_types = FALSE)
  
  tab_sentic <- sentic_top %>%
    dplyr::mutate(
      mean_score = rnd(mean_score, 2),
      min_score  = rnd(min_score, 2),
      max_score  = rnd(max_score, 2)
    ) %>%
    dplyr::rename(
      Word         = word,
      `n tokens`   = n_tokens,
      `Mean score` = mean_score,
      `Min score`  = min_score,
      `Max score`  = max_score,
      Polarity     = polarity
    )
  
  readr::write_csv(
    tab_sentic,
    file.path(dir_tables, "table_senticnet_examples_clean.csv")
  )
  
  message("Saved: table_senticnet_examples_clean.csv")
  
} else {
  warning("senticnet_top_words.csv not found; skipping SenticNet examples table.")
}

# ========================================================================
message("All available tables exported to outputs/tables/.")
