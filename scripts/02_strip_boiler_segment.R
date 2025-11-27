# R/02_strip_boiler_segment.R

novel_lines <- readr::read_rds(here::here("data/processed/novel_lines.rds"))

# 1) Strip Gutenberg boilerplate
text_stripped <- tryCatch(
  gutenbergr::gutenberg_strip(novel_lines$text),
  error = function(e) novel_lines$text
)

df <- tibble::tibble(line = seq_along(text_stripped), text = text_stripped) |>
  dplyr::mutate(text = stringr::str_replace_all(text, "\\s+$", "")) |>
  dplyr::filter(!stringr::str_detect(text, "^\\s*$"))

# 2) Detect chapter headings robustly (skip Table of Contents)
chap_pat  <- stringr::regex("^\\s*chapter\\s+(?:[ivxlcdm]+|\\d+)\\b", ignore_case = TRUE)
is_heading <- stringr::str_detect(df$text, chap_pat)

heading_followed_by_content <- which(is_heading & dplyr::lead(!is_heading, default = FALSE))
start_idx <- heading_followed_by_content[1]
if (is.na(start_idx)) stop("Could not locate the first real chapter heading.")

df2 <- df |>
  dplyr::slice(start_idx:dplyr::n()) |>
  dplyr::mutate(is_heading = stringr::str_detect(text, chap_pat),
                chapter    = cumsum(is_heading))

# 3) Remove heading lines & all-caps artefacts
df_ch_clean <- df2 |>
  dplyr::group_by(chapter) |>
  dplyr::mutate(row_in_chapter = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::filter(row_in_chapter > 1) |>
  dplyr::filter(!stringr::str_detect(text, "^([[:upper:][:digit:][:punct:][:space:]]+)$"))

# 4) Renumber from 1 (insurance against ToC offsets)
first_real <- min(df_ch_clean$chapter, na.rm = TRUE)
df_ch_clean <- df_ch_clean |>
  dplyr::mutate(chapter = chapter - (first_real - 1))

# 5) Collapse to one row per chapter
chapters <- df_ch_clean |>
  dplyr::group_by(chapter) |>
  dplyr::summarise(text = paste(text, collapse = " "), .groups = "drop") |>
  dplyr::mutate(
    text = stringr::str_remove(text, "^\\s*(?i:chapter\\s+(?:[ivxlcdm]+|\\d+)\\b\\.?\\s*)"),
    text = stringr::str_squish(text)
  )

# 6) Checks & quick summary
if (nrow(chapters) != 37L) {
  warning("Expected 37 chapters; found ", nrow(chapters), ". Inspect chapter detection.")
}
stopifnot(all(nchar(chapters$text) > 0))

chap_wc <- chapters |>
  dplyr::mutate(word_count = stringr::str_count(text, stringr::boundary("word")))

print(list(
  n_chapters   = nrow(chapters),
  min_words    = min(chap_wc$word_count),
  median_words = stats::median(chap_wc$word_count),
  max_words    = max(chap_wc$word_count)
))

# Shortest & longest (explicit prints)
shortest <- chap_wc |> dplyr::slice_min(word_count, with_ties = TRUE) |> dplyr::select(chapter, word_count)
longest  <- chap_wc |> dplyr::slice_max(word_count, with_ties = TRUE) |> dplyr::select(chapter, word_count)
print(shortest)
print(longest)

# Save
readr::write_rds(chapters, here::here("data/processed/novel_chapters.rds"))

