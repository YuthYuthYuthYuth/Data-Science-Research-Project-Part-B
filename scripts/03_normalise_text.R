# R/03_normalise_text.R
# Goal: Normalise punctuation/Unicode and tidy whitespace.
# Input:  data/processed/novel_chapters.rds  (from 02_strip_boiler_segment.R)
# Output: data/processed/novel_chapters_norm.rds

chapters <- readr::read_rds(here::here("data/processed/novel_chapters.rds"))

# Normalisation helpers

normalise_text <- function(x) {
  x |>
    # Curly quotes/apostrophes → straight
    stringr::str_replace_all("[\u2018\u2019]", "'") |>
    stringr::str_replace_all("[\u201C\u201D]", "\"") |>
    # Dashes/ellipsis/nbsp/soft hyphen
    stringr::str_replace_all("\u2014", " - ") |>  # em dash
    stringr::str_replace_all("\u2013", "-")   |>  # en dash
    stringr::str_replace_all("\u2026", "...") |>  # ellipsis
    stringr::str_replace_all("\u00A0", " ")   |>  # non-breaking space
    stringr::str_replace_all("\u00AD", "")    |>  # soft hyphen
    # Collapse repeated whitespace and trim
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_squish()
}

# Apply normalisation

chapters_norm <- chapters |>
  dplyr::mutate(
    text_clean = normalise_text(text),
    word_count = stringr::str_count(text_clean, stringr::boundary("word")),
    nonascii_ratio = stringr::str_count(text_clean, "[^\\x00-\\x7F]") /
      pmax(nchar(text_clean), 1L),
    any_unicode_left = stringr::str_detect(
      text_clean, "[\u2018\u2019\u201C\u201D\u2014\u2013\u00A0\u2026\u00AD]"
    )
  ) |>
  dplyr::select(chapter, text_clean, word_count, nonascii_ratio, any_unicode_left)

# Light checks & console summary

stopifnot(nrow(chapters_norm) == 37L)
stopifnot(all(nchar(chapters_norm$text_clean) > 0))

summary_list <- list(
  chapters            = nrow(chapters_norm),
  min_words           = min(chapters_norm$word_count),
  median_words        = stats::median(chapters_norm$word_count),
  max_words           = max(chapters_norm$word_count),
  max_nonascii_ratio  = max(chapters_norm$nonascii_ratio),
  any_unicode_left    = any(chapters_norm$any_unicode_left)
)
print(summary_list)

# Save

readr::write_rds(chapters_norm, here::here("data/processed/novel_chapters_norm.rds"))
message("Saved: data/processed/novel_chapters_norm.rds")
