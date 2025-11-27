# R/09_scoring.R
# Goal: Aggregate token-level adjusted valence to stops/chapters; bootstrap CIs; LOESS smooth.
# Inputs :
#   data/processed/tokens_stop_scored.rds
#   data/processed/tokens_chapter_scored.rds
# Outputs:
#   data/processed/sent_stop_summary.rds
#   data/processed/sent_chapter_summary.rds
#   data/processed/sent_stop_boot.rds
#   data/processed/sent_stop_loess.rds
#   (optional CSVs for quick inspection)

# load
p_stop <- here::here("data/processed/tokens_stop_scored.rds")
p_chap <- here::here("data/processed/tokens_chapter_scored.rds")
stopifnot(file.exists(p_stop), file.exists(p_chap))

tok_stop <- readr::read_rds(p_stop)
tok_chap <- readr::read_rds(p_chap)

# required cols
req <- c("word","sent_id","token_id","has_valence","val_base_adj","val_afinn_adj","val_bing_adj","val_nrc_adj")
stopifnot(all(req %in% names(tok_stop)))
stopifnot(all(req %in% names(tok_chap)))
stopifnot(all(c("order","location") %in% names(tok_stop)))
stopifnot("chapter" %in% names(tok_chap))

# emotion columns (if present from step 07)
emo_cols <- grep("^emo_", names(tok_stop), value = TRUE)

# helpers
summarise_group <- function(df, group_cols) {
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      tokens_total   = dplyr::n(),
      tokens_scored  = sum(has_valence),
      coverage       = tokens_scored / pmax(tokens_total, 1L),
      
      # means over ALL tokens (unscored tokens contribute 0 via val_base_adj)
      mean_val_base  = mean(val_base_adj),
      mean_val_afinn = mean(val_afinn_adj),
      mean_val_bing  = mean(val_bing_adj),
      mean_val_nrc   = mean(val_nrc_adj),
      
      # means over ONLY scored tokens (more comparable across groups)
      mean_val_base_scored  = ifelse(tokens_scored > 0, sum(val_base_adj)/tokens_scored, NA_real_),
      mean_val_afinn_scored = ifelse(tokens_scored > 0, sum(val_afinn_adj)/tokens_scored, NA_real_),
      mean_val_bing_scored  = ifelse(tokens_scored > 0, sum(val_bing_adj)/tokens_scored, NA_real_),
      mean_val_nrc_scored   = ifelse(tokens_scored > 0, sum(val_nrc_adj)/tokens_scored, NA_real_),
      
      # emotion rates per 1000 tokens (word can have multiple emotions)
      dplyr::across(dplyr::all_of(emo_cols), ~ 1000 * sum(.x, na.rm = TRUE) / pmax(tokens_total, 1L),
                    .names = "rate_{.col}_per1k"),
      .groups = "drop"
    )
}

# simple bootstrap mean for val_base_adj (over scored tokens)
boot_ci <- function(vals, n_boot = 1000L) {
  if (length(vals) == 0) return(c(NA_real_, NA_real_))
  m <- replicate(n_boot, mean(sample(vals, replace = TRUE)))
  stats::quantile(m, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)
}

bootstrap_groups <- function(df, group_cols, n_boot = 1000L) {
  df %>%
    dplyr::filter(has_valence) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(ci = list(boot_ci(val_base_adj, n_boot = n_boot)), .groups = "drop") %>%
    tidyr::unnest_wider(ci, names_sep = "_") %>%      # -> ci_1, ci_2
    dplyr::rename(ci_lo = ci_1, ci_hi = ci_2)
}


# aggregate
sent_stop <- summarise_group(tok_stop, c("order","location")) %>%
  dplyr::arrange(order)

sent_chap <- summarise_group(tok_chap, "chapter") %>%
  dplyr::arrange(chapter)

# bootstrap CIs at stop level (val_base_adj over scored tokens)
set.seed(2025)
boot_stop <- bootstrap_groups(tok_stop, c("order","location"), n_boot = 1500L)

sent_stop <- sent_stop %>%
  dplyr::left_join(boot_stop, by = c("order","location"))

# LOESS smooth (journey curve
# Use the scored-token mean (more comparable across stops of different sizes)
loess_span <- 0.6  # tweakable
loess_fit <- stats::loess(mean_val_base_scored ~ order, data = sent_stop, span = loess_span)
pred_df <- tibble::tibble(order = sent_stop$order) %>%
  dplyr::mutate(val_loess = as.numeric(stats::predict(loess_fit, newdata = data.frame(order = order))))

sent_stop_loess <- sent_stop %>%
  dplyr::select(order, location, mean_val_base_scored) %>%
  dplyr::left_join(pred_df, by = "order")

# save
readr::write_rds(sent_stop,        here::here("data/processed/sent_stop_summary.rds"))
readr::write_rds(sent_chap,        here::here("data/processed/sent_chapter_summary.rds"))
readr::write_rds(boot_stop,        here::here("data/processed/sent_stop_boot.rds"))
readr::write_rds(sent_stop_loess,  here::here("data/processed/sent_stop_loess.rds"))

