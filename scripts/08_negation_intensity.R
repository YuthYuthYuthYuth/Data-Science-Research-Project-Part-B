# R/08_negation_intensity.R
# Goal: Apply negation and intensity (amplifiers/downtoners) to token valence.
# Inputs :
#   data/processed/tokens_stop_lex.rds
#   data/processed/tokens_chapter_lex.rds
# Outputs:
#   data/processed/tokens_stop_scored.rds
#   data/processed/tokens_chapter_scored.rds

# settings (tweakable)
window_k       <- 3      # look-back window within the same sentence (tokens)
amp_strength   <- 0.6    # per-amplifier multiplicative increase
down_strength  <- 0.5    # per-downtoner multiplicative decrease
min_mag_factor <- 0.0    # lower bound after downtoning (0..1)

negators <- c(
  "not","no","never","neither","nor","without","none","nobody","nothing",
  "n't","don't","doesn't","didn't","can't","cannot","couldn't","won't","wouldn't",
  "shan't","shouldn't","isn't","aren't","wasn't","weren't","hasn't","haven't",
  "hadn't","ain't"
)

amplifiers <- c(
  "very","really","extremely","so","too","highly","totally","absolutely",
  "utterly","completely","incredibly","especially","remarkably","particularly",
  "awfully","terribly","hugely","deeply","decidedly","profoundly","vastly","super"
)

downtoners <- c(
  "slightly","somewhat","rather","fairly","barely","hardly","scarcely",
  "little","a","bit" # "a bit" will appear as "a" then "bit"—both included
)

# ---- load tokens with lexicons ---------------------------------------------

p_stop <- here::here("data/processed/tokens_stop_lex.rds")
p_chap <- here::here("data/processed/tokens_chapter_lex.rds")
if (!file.exists(p_stop) || !file.exists(p_chap)) {
  stop("Missing tokens_*_lex.rds. Run R/07_lexicons.R first.")
}
tok_stop <- readr::read_rds(p_stop)
tok_chap <- readr::read_rds(p_chap)

# TEMP fix: if NRC or SenticNet columns are missing, create them with zeros
if (!"lex_NRC" %in% names(tok_stop)) {
  tok_stop <- dplyr::mutate(tok_stop, lex_NRC = 0)
}
if (!"lex_NRC" %in% names(tok_chap)) {
  tok_chap <- dplyr::mutate(tok_chap, lex_NRC = 0)
}
if (!"lex_SenticNet" %in% names(tok_stop)) {
  tok_stop <- dplyr::mutate(tok_stop, lex_SenticNet = 0)
}
if (!"lex_SenticNet" %in% names(tok_chap)) {
  tok_chap <- dplyr::mutate(tok_chap, lex_SenticNet = 0)
}

# Required columns
req_val_cols <- c("lex_AFINN","lex_Bing","lex_NRC","lex_SenticNet",
                  "word","sent_id","token_id")
stopifnot(all(req_val_cols %in% names(tok_stop)))
stopifnot(all(req_val_cols %in% names(tok_chap)))

# ---- helper: score one table (stop-level or chapter-level) -----------------

score_tokens <- function(df, id_cols) {
  
  # Ensure ordering before sliding-window ops
  df <- df %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(id_cols)), sent_id, token_id)
  
  # Flag cue words
  df <- df %>%
    dplyr::mutate(
      neg_i  = as.integer(word %in% negators),
      amp_i  = as.integer(word %in% amplifiers),
      down_i = as.integer(word %in% downtoners)
    )
  
  # Within each sentence, count cues in the previous window_k tokens
  df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, "sent_id")))) %>%
    dplyr::mutate(
      cneg  = cumsum(neg_i),
      camp  = cumsum(amp_i),
      cdown = cumsum(down_i),
      
      neg_w  = dplyr::lag(cneg,  1, default = 0) -
        dplyr::lag(cneg,  window_k + 1, default = 0),
      amp_w  = dplyr::lag(camp,  1, default = 0) -
        dplyr::lag(camp,  window_k + 1, default = 0),
      down_w = dplyr::lag(cdown, 1, default = 0) -
        dplyr::lag(cdown, window_k + 1, default = 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      neg_w  = pmax(0L, as.integer(neg_w)),
      amp_w  = pmax(0L, as.integer(amp_w)),
      down_w = pmax(0L, as.integer(down_w))
    ) %>%
    dplyr::select(-c(cneg, camp, cdown)) # drop helpers
  
  # Base valence from each source
  df <- df %>%
    dplyr::mutate(
      val_afinn  = lex_AFINN / 5,      # scale AFINN to ±1
      val_bing   = lex_Bing,
      val_nrc    = lex_NRC,
      val_sentic = lex_SenticNet
    )
  
  # Mean of non-zero sources (0 if none)
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      val_base = {
        v <- c(val_afinn, val_bing, val_nrc, val_sentic)
        nz <- v[v != 0]
        if (length(nz) == 0) 0 else mean(nz)
      }
    ) %>%
    dplyr::ungroup()
  
  # Modifiers: amplification/downtoning factor and negation flip
  df <- df %>%
    dplyr::mutate(
      factor_amp   = 1 + amp_strength  * amp_w,
      factor_down  = pmax(1 - down_strength * down_w, min_mag_factor),
      mag_factor   = factor_amp * factor_down,
      flip_sign    = (neg_w %% 2L) == 1L,
      sign_factor  = ifelse(flip_sign, -1, 1)
    )
  
  # Apply modifiers to each source and to the base score
  apply_adj <- function(v, mag, sgn) v * mag * sgn
  
  df <- df %>%
    dplyr::mutate(
      val_afinn_adj  = apply_adj(val_afinn,  mag_factor, sign_factor),
      val_bing_adj   = apply_adj(val_bing,   mag_factor, sign_factor),
      val_nrc_adj    = apply_adj(val_nrc,    mag_factor, sign_factor),
      val_sentic_adj = apply_adj(val_sentic, mag_factor, sign_factor),
      val_base_adj   = apply_adj(val_base,   mag_factor, sign_factor),
      has_valence    = (val_afinn != 0) | (val_bing != 0) |
        (val_nrc != 0)   | (val_sentic != 0)
    )
  
  df
}

# ---- score stop-level and chapter-level -----------------------------------

scored_stop    <- score_tokens(tok_stop,    id_cols = c("order","location"))
scored_chapter <- score_tokens(tok_chap,    id_cols = c("chapter"))

# quick integrity checks (optional)
stopifnot(all(scored_stop$val_base_adj <=  5))
stopifnot(all(scored_stop$val_base_adj >= -5))

# save
readr::write_rds(scored_stop,    here::here("data/processed/tokens_stop_scored.rds"))
readr::write_rds(scored_chapter, here::here("data/processed/tokens_chapter_scored.rds"))
message("Saved: tokens_stop_scored.rds and tokens_chapter_scored.rds")
