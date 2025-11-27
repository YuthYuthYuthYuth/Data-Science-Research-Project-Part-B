# R/10_diagnostics.R
# Goal: QA/diagnostics for the sentiment pipeline before plotting.
# Inputs :
#   data/processed/tokens_stop_scored.rds
#   data/processed/tokens_chapter_scored.rds
#   data/processed/sent_stop_summary.rds
#   data/processed/sent_chapter_summary.rds
# Outputs:
#   data/processed/diag_stop_overview.rds
#   data/processed/diag_chap_overview.rds
#   data/processed/diag_lexicon_agreement.rds
#   data/processed/diag_negation_stats.rds
#   data/processed/diag_unscored_top.rds
#   data/processed/diag_extreme_sentences.rds
#   (optional CSVs commented at bottom)

# load
p_tok_stop <- here::here("data/processed/tokens_stop_scored.rds")
p_tok_chap <- here::here("data/processed/tokens_chapter_scored.rds")
p_sum_stop <- here::here("data/processed/sent_stop_summary.rds")
p_sum_chap <- here::here("data/processed/sent_chapter_summary.rds")

stopifnot(file.exists(p_tok_stop), file.exists(p_tok_chap),
          file.exists(p_sum_stop), file.exists(p_sum_chap))

tok_stop <- readr::read_rds(p_tok_stop)
tok_chap <- readr::read_rds(p_tok_chap)
sum_stop <- readr::read_rds(p_sum_stop)
sum_chap <- readr::read_rds(p_sum_chap)

# quick overview tables
diag_stop_overview <- sum_stop %>%
  dplyr::arrange(order) %>%
  dplyr::select(order, location, tokens_total, tokens_scored, coverage,
                mean_val_base_scored, mean_val_afinn_scored,
                mean_val_bing_scored,  mean_val_nrc_scored,
                dplyr::starts_with("rate_"))  # emotion rates if present

diag_chap_overview <- sum_chap %>%
  dplyr::arrange(chapter) %>%
  dplyr::select(chapter, tokens_total, tokens_scored, coverage,
                mean_val_base_scored, mean_val_afinn_scored,
                mean_val_bing_scored,  mean_val_nrc_scored,
                dplyr::starts_with("rate_"))

# flag low-coverage stops/chapters
low_cov_thr <- 0.18  # tweak if needed
diag_stop_overview <- diag_stop_overview %>%
  dplyr::mutate(flag_low_coverage = coverage < low_cov_thr)
diag_chap_overview <- diag_chap_overview %>%
  dplyr::mutate(flag_low_coverage = coverage < low_cov_thr)

# lexicon agreement (per-stop)
diag_lexicon_agreement <- diag_stop_overview %>%
  dplyr::select(order, location,
                afinn = mean_val_afinn_scored,
                bing  = mean_val_bing_scored,
                nrc   = mean_val_nrc_scored) %>%
  dplyr::summarise(
    r_afinn_bing = stats::cor(afinn, bing, use = "complete.obs"),
    r_afinn_nrc  = NA_real_,  # NRC temporarily disabled
    r_bing_nrc   = NA_real_
  ) %>%
  dplyr::mutate(n_stops = dplyr::n_distinct(diag_stop_overview$order))

 

# modifier usage & effects
# tokens_stop_scored contains neg_w, amp_w, down_w from step 08
has_mod_cols <- all(c("neg_w","amp_w","down_w","val_base","val_base_adj") %in% names(tok_stop))

diag_negation_stats <- tok_stop %>%
  dplyr::group_by(order, location) %>%
  dplyr::summarise(
    tokens = dplyr::n(),
    # average number of cues in the lookback window per token
    mean_neg_w  = mean(neg_w,  na.rm = TRUE),
    mean_amp_w  = mean(amp_w,  na.rm = TRUE),
    mean_down_w = mean(down_w, na.rm = TRUE),
    # how often sign flips occurred (odd-count negation in window)
    flip_rate   = mean((neg_w %% 2L) == 1L),
    # magnitude change factor (|adj| / (|base| + eps)) among scored tokens
    mag_change  = mean(
      ifelse(abs(val_base) > 0, abs(val_base_adj) / abs(val_base), NA_real_),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(order)

# top unscored tokens (to see gaps in lexicons)
diag_unscored_top <- tok_stop %>%
  dplyr::filter(!has_valence) %>%
  dplyr::count(word, sort = TRUE, name = "n") %>%
  dplyr::slice_head(n = 100L)  # tune for longer list if you like

# extreme sentences (most pos/neg base-adjusted)
# group by stop + sentence; skip sentences with no scored tokens
sent_summ <- tok_stop %>%
  dplyr::group_by(order, location, sent_id) %>%
  dplyr::summarise(
    tokens_scored = sum(has_valence),
    mean_val      = ifelse(tokens_scored > 0, mean(val_base_adj[has_valence]), NA_real_),
    .groups = "drop"
  ) %>% dplyr::filter(!is.na(mean_val))

extreme_pos <- sent_summ %>%
  dplyr::slice_max(mean_val, n = 20, with_ties = FALSE) %>%
  dplyr::mutate(kind = "top_positive")

extreme_neg <- sent_summ %>%
  dplyr::slice_min(mean_val, n = 20, with_ties = FALSE) %>%
  dplyr::mutate(kind = "top_negative")

diag_extreme_sentences <- dplyr::bind_rows(extreme_pos, extreme_neg) %>%
  dplyr::arrange(kind, dplyr::desc(mean_val))

# save
readr::write_rds(diag_stop_overview,     here::here("data/processed/diag_stop_overview.rds"))
readr::write_rds(diag_chap_overview,     here::here("data/processed/diag_chap_overview.rds"))
readr::write_rds(diag_lexicon_agreement, here::here("data/processed/diag_lexicon_agreement.rds"))
readr::write_rds(diag_negation_stats,    here::here("data/processed/diag_negation_stats.rds"))
readr::write_rds(diag_unscored_top,      here::here("data/processed/diag_unscored_top.rds"))
readr::write_rds(diag_extreme_sentences, here::here("data/processed/diag_extreme_sentences.rds"))
