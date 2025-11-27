# R/20_senticnet_examples.R
# Goal: Give concrete examples of how SenticNet behaves in the novel.
#       We summarise the most frequent SenticNet-scored words.
#
# Inputs:
#   data/processed/tokens_stop_lex.rds   (from 07_lexicons.R)
#
# Outputs:
#   outputs/tables/senticnet_top_words.csv

# --- packages (no library(), use ::) ---------------------------------------
stopifnot(requireNamespace("here",  quietly = TRUE))
stopifnot(requireNamespace("readr", quietly = TRUE))
stopifnot(requireNamespace("dplyr", quietly = TRUE))

# --- load token data with SenticNet column --------------------------------
p_tok <- here::here("data/processed/tokens_stop_lex.rds")
stopifnot(file.exists(p_tok))

tok <- readr::read_rds(p_tok)

stopifnot("word" %in% names(tok))
stopifnot("lex_SenticNet" %in% names(tok))

# SenticNet scores are in roughly [-1, 1]; 0 = not in lexicon
# Keep only tokens that SenticNet actually scored
tok_sentic <- tok |>
  dplyr::filter(!is.na(lex_SenticNet), lex_SenticNet != 0)

# --- summarise by word -----------------------------------------------------
sentic_top <- tok_sentic |>
  dplyr::group_by(word) |>
  dplyr::summarise(
    n_tokens   = dplyr::n(),
    mean_score = mean(lex_SenticNet, na.rm = TRUE),
    min_score  = min(lex_SenticNet, na.rm = TRUE),
    max_score  = max(lex_SenticNet, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  dplyr::mutate(
    polarity = dplyr::case_when(
      mean_score >  0 ~ "positive",
      mean_score <  0 ~ "negative",
      TRUE            ~ "neutral"
    )
  ) |>
  dplyr::arrange(dplyr::desc(n_tokens)) |>
  dplyr::slice(1:20)

# --- save table ------------------------------------------------------------
outdir <- here::here("outputs/tables")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

out_path <- file.path(outdir, "senticnet_top_words.csv")
readr::write_csv(sentic_top, out_path)

# quick console preview
print(sentic_top)

message("Saved SenticNet examples to: ", out_path)
