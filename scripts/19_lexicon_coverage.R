# R/19_lexicon_coverage.R
# Goal: Summarise coverage for each lexicon (AFINN, Bing, NRC, SenticNet, ...)
# Inputs :
#   data/processed/tokens_stop_lex.rds
# Outputs:
#   outputs/tables/lexicon_coverage_overall.csv
#   outputs/tables/lexicon_coverage_by_stop.csv

stopifnot(requireNamespace("here",  quietly = TRUE))
stopifnot(requireNamespace("readr", quietly = TRUE))
stopifnot(requireNamespace("dplyr", quietly = TRUE))
stopifnot(requireNamespace("stringr", quietly = TRUE))

# ---- load token-level data with lexicons -----------------------------------
p_tok <- here::here("data/processed/tokens_stop_lex.rds")
stopifnot(file.exists(p_tok))

tok <- readr::read_rds(p_tok)

stopifnot(all(c("word", "order", "location") %in% names(tok)))

# identify lexicon columns generically: any column starting with "lex_"
lex_cols <- grep("^lex_", names(tok), value = TRUE)

if (length(lex_cols) == 0) {
  stop("No lexicon columns found (names starting with 'lex_'). ",
       "Check that 07_lexicons.R has been run.")
}

message("Found lexicon columns: ", paste(lex_cols, collapse = ", "))

# helper to turn "lex_SenticNet" -> "SenticNet", "lex_AFINN" -> "AFINN", etc.
clean_lex_name <- function(x) stringr::str_remove(x, "^lex_")

# -------------------------------------------------------------------
# 1. Overall coverage per lexicon (token + type coverage)
# -------------------------------------------------------------------
n_tokens <- nrow(tok)
n_types  <- dplyr::n_distinct(tok$word)

overall_cov <- purrr::map_dfr(lex_cols, function(col) {
  vals <- tok[[col]]
  scored <- !is.na(vals) & vals != 0
  
  n_scored_tokens <- sum(scored)
  n_scored_types  <- tok$word[scored] |> unique() |> length()
  
  dplyr::tibble(
    lexicon          = clean_lex_name(col),
    n_tokens         = n_tokens,
    n_types          = n_types,
    n_scored_tokens  = n_scored_tokens,
    n_scored_types   = n_scored_types,
    token_coverage   = n_scored_tokens / n_tokens,
    type_coverage    = n_scored_types  / n_types
  )
})

# -------------------------------------------------------------------
# 2. Per-stop coverage per lexicon
# -------------------------------------------------------------------
by_stop_cov <- tok |>
  dplyr::select(order, location, dplyr::all_of(lex_cols)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(lex_cols),
    names_to  = "lex_col",
    values_to = "score"
  ) |>
  dplyr::mutate(
    lexicon = clean_lex_name(lex_col),
    scored  = !is.na(score) & score != 0
  ) |>
  dplyr::group_by(order, location, lexicon) |>
  dplyr::summarise(
    n_tokens_stop     = dplyr::n(),
    n_scored_tokens   = sum(scored),
    token_coverage    = n_scored_tokens / n_tokens_stop,
    .groups = "drop"
  ) |>
  dplyr::arrange(order, lexicon)

# -------------------------------------------------------------------
# 3. Save tables
# -------------------------------------------------------------------
outdir <- here::here("outputs/tables")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(overall_cov,
                 file.path(outdir, "lexicon_coverage_overall.csv"))
readr::write_csv(by_stop_cov,
                 file.path(outdir, "lexicon_coverage_by_stop.csv"))

message("Saved: lexicon_coverage_overall.csv and lexicon_coverage_by_stop.csv")
message("Overall coverage (first few rows):")
print(overall_cov)
