# R/07_lexicons.R
# Goal: Prepare AFINN, Bing (and optional NRC, SenticNet) lexicons and
#       join to tokens (stops + chapters).
# Inputs :
#   data/processed/tokens_stop.rds
#   data/processed/tokens_chapter.rds
#   data/external/senticnet_wordlist.csv   (optional, from 18_prepare_senticnet.R)
# Outputs:
#   data/processed/lexicon_valence.rds
#   data/processed/lexicon_emotions.rds
#   data/processed/tokens_stop_lex.rds
#   data/processed/tokens_chapter_lex.rds

# project-local cache for lexicons (avoids ~/Library/Caches issues) 
cache_dir <- here::here("data/external/textdata")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
options(textdata.home = cache_dir, textdata.download = TRUE)

# ---- load tokens -----------------------------------------------------------
p_stop <- here::here("data/processed/tokens_stop.rds")
p_chap <- here::here("data/processed/tokens_chapter.rds")
if (!file.exists(p_stop) || !file.exists(p_chap)) {
  stop("Missing tokens. Run R/06_tokenise_lemmatise.R first.")
}
tokens_stop    <- readr::read_rds(p_stop)
tokens_chapter <- readr::read_rds(p_chap)

stopifnot(all(c("word","order","location") %in% names(tokens_stop)))
stopifnot(all(c("word","chapter")          %in% names(tokens_chapter)))

# enforce lowercase (06 already does this; keep as safety)
tokens_stop    <- dplyr::mutate(tokens_stop,    word = stringr::str_to_lower(word))
tokens_chapter <- dplyr::mutate(tokens_chapter, word = stringr::str_to_lower(word))

# ---- lexicons from {textdata} ----------------------------------------------

afinn_raw <- textdata::lexicon_afinn(dir = cache_dir)
bing_raw  <- textdata::lexicon_bing(dir  = cache_dir)
# nrc_raw   <- textdata::lexicon_nrc(dir   = cache_dir)   # still disabled for now

# AFINN: numeric valence (–5..+5)
lex_afinn <- afinn_raw %>%
  dplyr::transmute(
    word   = stringr::str_to_lower(word),
    source = "AFINN",
    score  = as.numeric(value)
  )

# Bing: positive/negative → ±1
lex_bing <- bing_raw %>%
  dplyr::transmute(
    word   = stringr::str_to_lower(word),
    source = "Bing",
    score  = dplyr::case_when(
      sentiment == "positive" ~  1,
      sentiment == "negative" ~ -1,
      TRUE ~ 0
    )
  )

# NRC: polarity & emotions still disabled – keep empty placeholders so
# downstream code continues to work without NRC.
lex_nrc_polarity  <- tibble::tibble(
  word   = character(),
  source = character(),
  score  = numeric()
)
lex_nrc_emotions  <- tibble::tibble(
  word    = character(),
  emotion = character(),
  value   = integer()
)

# ---- SenticNet (from 18_prepare_senticnet.R) -------------------------------

p_sentic <- here::here("data/external/senticnet_wordlist.csv")

if (file.exists(p_sentic)) {
  sentic_raw <- readr::read_csv(p_sentic, show_col_types = FALSE)
  stopifnot(all(c("word", "sentic_score") %in% names(sentic_raw)))
  
  lex_sentic <- sentic_raw %>%
    dplyr::transmute(
      word   = stringr::str_to_lower(word),
      source = "SenticNet",
      score  = as.numeric(sentic_score)
    )
  
  message("SenticNet found: using ", nrow(lex_sentic), " word-level entries.")
} else {
  message("No SenticNet wordlist found at data/external/senticnet_wordlist.csv. ",
          "Continuing without SenticNet.")
  lex_sentic <- tibble::tibble(word = character(),
                               source = character(),
                               score = numeric())
}

# ---- build wide valence dictionary ----------------------------------------

# Valence: one row per word, columns for each source (AFINN/Bing/NRC/SenticNet)
lex_valence <- dplyr::bind_rows(
  lex_afinn,
  lex_bing,
  lex_nrc_polarity,
  lex_sentic
) %>%
  dplyr::group_by(word, source) %>%
  dplyr::summarise(score = mean(score), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = source,
    values_from = score,
    values_fill = 0,
    names_prefix = "lex_"
  ) %>%
  dplyr::arrange(word)
# -> columns: word, lex_AFINN, lex_Bing, (lex_NRC), lex_SenticNet (if present)

# Emotions: one row per word, 0/1 for eight NRC emotions (currently empty)
lex_emotions <- lex_nrc_emotions %>%
  dplyr::distinct(word, emotion, .keep_all = TRUE) %>%
  tidyr::pivot_wider(
    names_from  = emotion,
    values_from = value,
    values_fill = 0L,
    names_prefix = "emo_"
  ) %>%
  dplyr::arrange(word)

# Save the dictionaries
readr::write_rds(lex_valence,  here::here("data/processed/lexicon_valence.rds"))
readr::write_rds(lex_emotions, here::here("data/processed/lexicon_emotions.rds"))
message("Saved: lexicon_valence.rds and lexicon_emotions.rds")

# ---- join to tokens -------------------------------------------------------

val_cols <- setdiff(names(lex_valence),  "word")
emo_cols <- setdiff(names(lex_emotions), "word")

join_lex <- function(tokens_df) {
  tokens_df %>%
    dplyr::left_join(lex_valence,  by = "word") %>%
    dplyr::left_join(lex_emotions, by = "word") %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(val_cols), ~ ifelse(is.na(.), 0, .)),
      dplyr::across(dplyr::all_of(emo_cols), ~ ifelse(is.na(.), 0L, .))
    )
}

tokens_stop_lex    <- join_lex(tokens_stop)
tokens_chapter_lex <- join_lex(tokens_chapter)

# save
readr::write_rds(tokens_stop_lex,    here::here("data/processed/tokens_stop_lex.rds"))
readr::write_rds(tokens_chapter_lex, here::here("data/processed/tokens_chapter_lex.rds"))
message("Saved: tokens_stop_lex.rds and tokens_chapter_lex.rds")
