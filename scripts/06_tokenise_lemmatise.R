# R/06_tokenise_lemmatise.R
# Goal: Tokenise narrator text by sentences and words (stops + chapters).
# Inputs  (from 05_location_map.R):
#   data/processed/segments_by_location.rds
#   data/processed/segments_by_chapter.rds
# Outputs:
#   data/processed/tokens_stop.rds
#   data/processed/tokens_chapter.rds

# settings
do_stem <- FALSE   # set TRUE to add SnowballC::wordStem(word) as word_stem

# load data
p_stop <- here::here("data/processed/segments_by_location.rds")
p_chap <- here::here("data/processed/segments_by_chapter.rds")

if (!file.exists(p_stop) || !file.exists(p_chap)) {
  stop("Missing inputs from step 05. Run R/05_location_map.R first.")
}

seg_stop    <- readr::read_rds(p_stop)     # has: order, location, text_narr, ...
seg_chapter <- readr::read_rds(p_chap)     # has: chapter, text_narr, ...

# sanity checks
stopifnot(all(c("order", "location", "text_narr") %in% names(seg_stop)))
stopifnot(all(c("chapter", "text_narr") %in% names(seg_chapter)))

# helpers
tokenise_one <- function(df, id_cols, text_col = "text_narr") {
  # 1) split to sentences
  sents <- df |>
    dplyr::select(dplyr::all_of(c(id_cols, text_col))) |>
    tidytext::unnest_tokens(sentence, !!rlang::sym(text_col), token = "sentences") |>
    dplyr::filter(!is.na(sentence), nzchar(sentence)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::mutate(sent_id = dplyr::row_number()) |>
    dplyr::ungroup()
  
  # 2) split sentences to words (lowercase) and keep alphabetic/apostrophes
  toks <- sents |>
    tidytext::unnest_tokens(word, sentence, token = "words", to_lower = TRUE) |>
    dplyr::filter(stringr::str_detect(word, "^[a-z']+$")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, "sent_id")))) |>
    dplyr::mutate(token_id = dplyr::row_number()) |>
    dplyr::ungroup()
  
  if (isTRUE(do_stem)) {
    toks <- toks |> dplyr::mutate(word_stem = SnowballC::wordStem(word, language = "en"))
  }
  
  toks
}

# tokenise: stop-level (primary)
tokens_stop <- tokenise_one(
  df = seg_stop,
  id_cols = c("order", "location")
) |>
  dplyr::group_by(order, location) |>
  dplyr::mutate(token_id_global = dplyr::row_number()) |>
  dplyr::ungroup()

# tokenise: chapter-level (diagnostics/appendix)
tokens_chapter <- tokenise_one(
  df = seg_chapter,
  id_cols = c("chapter")
) |>
  dplyr::group_by(chapter) |>
  dplyr::mutate(token_id_global = dplyr::row_number()) |>
  dplyr::ungroup()

# quick summaries
counts_stop <- tokens_stop %>% dplyr::count(order, location, name = "n")
summ_stop <- tibble::tibble(
  n_stops   = dplyr::n_distinct(tokens_stop$order),
  n_tokens  = nrow(tokens_stop),
  median_tokens_per_stop = stats::median(counts_stop$n)
)
print(summ_stop)

counts_chap <- tokens_chapter %>% dplyr::count(chapter, name = "n")
summ_chap <- tibble::tibble(
  n_chapters = dplyr::n_distinct(tokens_chapter$chapter),
  n_tokens   = nrow(tokens_chapter),
  median_tokens_per_chapter = stats::median(counts_chap$n)
)
print(summ_chap)


# save
readr::write_rds(tokens_stop,    here::here("data/processed/tokens_stop.rds"))
readr::write_rds(tokens_chapter, here::here("data/processed/tokens_chapter.rds"))
message("Saved: data/processed/tokens_stop.rds and data/processed/tokens_chapter.rds")
