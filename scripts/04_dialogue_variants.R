# R/04_dialogue_variants.R
# Goal: Create narrator-only (strict + conservative) and dialogue-only text; QA; save.
# Input : data/processed/novel_chapters_norm.rds
# Output: data/processed/novel_clean_master.rds

chapters_norm <- readr::read_rds(here::here("data/processed/novel_chapters_norm.rds"))

# Safety: ensure quotes are straight double quotes (03 should have done this already)
chapters_norm <- chapters_norm %>%
  dplyr::mutate(text_clean = stringr::str_replace_all(text_clean, "[\u201C\u201D]", "\""))

#  Helpers
# Find all quoted spans as start/end positions (only properly closed quotes)
.locate_quotes <- function(s) stringr::str_locate_all(s, '"[^"]*"')[[1]]

# Replace selected spans with a single space, processing spans from right to left
.replace_spans_with_space <- function(s, spans_idx, spans_mat) {
  if (length(spans_idx) == 0L) return(stringr::str_squish(s))
  # Process in reverse order so earlier indices remain valid
  for (i in rev(spans_idx)) {
    start_i <- spans_mat[i, 1]
    end_i   <- spans_mat[i, 2]
    s <- paste0(substr(s, 1L, start_i - 1L), " ", substr(s, end_i + 1L, nchar(s)))
  }
  stringr::str_squish(s)
}

# STRICT: remove ANY quoted span
drop_quoted_speech_strict <- function(x) {
  purrr::map_chr(x, function(s) {
    spans <- .locate_quotes(s)
    if (is.null(dim(spans)) || nrow(spans) == 0L) return(s)
    .replace_spans_with_space(s, seq_len(nrow(spans)), spans)
  })
}

# CONSERVATIVE: remove ONLY quotes that contain at least one space (≈ ≥ 2 words)
drop_quoted_speech_conservative <- function(x) {
  purrr::map_chr(x, function(s) {
    spans <- .locate_quotes(s)
    if (is.null(dim(spans)) || nrow(spans) == 0L) return(s)
    inner <- stringr::str_sub(s, spans[,1] + 1L, spans[,2] - 1L)
    keep_idx <- which(stringr::str_detect(inner, "\\s"))  # has a space inside
    .replace_spans_with_space(s, keep_idx, spans)
  })
}

# DIALOGUE-ONLY: keep quoted spans and collapse them
keep_quoted_speech <- function(x) {
  # extract all spans like " ... " (straight double quotes only)
  qs <- stringr::str_extract_all(x, "\"[^\"]*\"")
  purrr::map_chr(qs, function(z) {
    if (length(z) == 0L) "" else stringr::str_squish(paste(z, collapse = " "))
  })
}


# Build variants
variants <- chapters_norm %>%
  dplyr::transmute(
    chapter,
    text_clean,
    text_no_dialogue_strict = drop_quoted_speech_strict(text_clean),
    text_no_dialogue_cons   = drop_quoted_speech_conservative(text_clean),
    text_dialogue_only      = keep_quoted_speech(text_clean),
    wc_full = stringr::str_count(text_clean,              stringr::boundary("word")),
    wc_str  = stringr::str_count(text_no_dialogue_strict, stringr::boundary("word")),
    wc_cons = stringr::str_count(text_no_dialogue_cons,   stringr::boundary("word")),
    wc_dial = stringr::str_count(text_dialogue_only,      stringr::boundary("word"))
  ) %>%
  dplyr::mutate(
    removed_ratio_strict = pmax((wc_full - wc_str)  / pmax(wc_full, 1L), 0),
    removed_ratio_cons   = pmax((wc_full - wc_cons) / pmax(wc_full, 1L), 0),
    dialogue_ratio       = pmax(wc_dial / pmax(wc_full, 1L), 0)
  )

# QA & sanity checks
# Console summary
print(
  variants %>% dplyr::summarise(
    chapters              = dplyr::n(),
    median_removed_strict = stats::median(removed_ratio_strict),
    max_removed_strict    = max(removed_ratio_strict),
    median_removed_cons   = stats::median(removed_ratio_cons),
    max_removed_cons      = max(removed_ratio_cons)
  )
)

# Conservative must never remove more than strict (allow tiny float tolerance)
viol <- variants %>%
  dplyr::filter(removed_ratio_cons > removed_ratio_strict + 1e-9) %>%
  dplyr::select(chapter, removed_ratio_strict, removed_ratio_cons)
if (nrow(viol) > 0) {
  stop("Conservative filter removed more than strict in these chapters:\n",
       paste0("ch ", viol$chapter, collapse = ", "),
       "\nPlease re-run 03_normalise_text.R or check quoting.")
}

stopifnot(nrow(variants) == 37L)

# Save master
novel_clean_master <- variants %>%
  dplyr::select(
    chapter, text_clean,
    text_no_dialogue_strict, text_no_dialogue_cons, text_dialogue_only,
    wc_full, wc_str, wc_cons, wc_dial,
    removed_ratio_strict, removed_ratio_cons, dialogue_ratio
  )

readr::write_rds(novel_clean_master, here::here("data/processed/novel_clean_master.rds"))
message("Saved: data/processed/novel_clean_master.rds")
