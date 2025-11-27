# R/05_location_map.R
# Goal: Map chapters to journey locations (suggest → optionally curate),
#       then attach locations and create location-level segments.
# Inputs :
#   data/processed/novel_clean_master.rds  (from 04_dialogue_variants.R)
# Outputs:
#   data/raw/location_map_suggested.csv    
#   data/processed/segments_by_chapter.rds
#   data/processed/segments_by_location.rds  (collapsed per journey stop)

d <- readr::read_rds(here::here("data/processed/novel_clean_master.rds"))

# I'll use the conservative narrator text as the primary analysis text.
text_for_locs <- tibble::tibble(chapter = d$chapter, text = d$text_clean)

# Define journey locations & simple regex patterns
# Tweak these if you like. They’re intentionally simple and robust.
loc_patterns <- list(
  "London"         = "\\bLondon\\b|\\bSaville\\s+Row\\b",
  "Suez/Aden"      = "\\bSuez\\b|\\bAden\\b|\\bRed\\s+Sea\\b",
  "Bombay"         = "\\bBombay\\b",
  "Calcutta"       = "\\bCalcutta\\b",
  "Hong Kong"      = "\\bHong[- ]?Kong\\b",
  "Yokohama/Japan" = "\\bYokohama\\b|\\bJapan\\b|\\bNagasaki\\b",
  "San Francisco"  = "\\bSan\\s+Francisco\\b",
  "New York"       = "\\bNew[- ]?York\\b|\\bManhattan\\b",
  "Atlantic/Liverpool" = "\\bAtlantic\\b|\\bLiverpool\\b|\\bHenrietta\\b"
)

count_loc <- function(txt, pat) {
  stringr::str_count(stringr::str_to_lower(txt),
                     stringr::regex(pat, ignore_case = TRUE))
}

# Count location mentions per chapter
loc_counts <- purrr::imap_dfr(
  loc_patterns,
  ~ tibble::tibble(
    chapter  = text_for_locs$chapter,
    location = .y,
    n        = count_loc(text_for_locs$text, .x)
  )
)

# Suggest a location for each chapter
suggested <- loc_counts %>%
  dplyr::group_by(chapter) %>%
  dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%  # pick the top match
  dplyr::ungroup() %>%
  dplyr::mutate(
    location = dplyr::if_else(n == 0, "Transit", location),
    # Journey order: increment when label changes as we move through chapters
    order = 1L + cumsum(dplyr::coalesce(location != dplyr::lag(location), FALSE))
  ) %>%
  dplyr::select(chapter, location, order, loc_hits = n)

# Write suggested CSV
if (!dir.exists(here::here("data/raw"))) dir.create(here::here("data/raw"), recursive = TRUE)

suggested_path <- here::here("data/raw/location_map_suggested.csv")
readr::write_csv(suggested, suggested_path)
message("Suggested map written to: data/raw/location_map_suggested.csv")

curated_path <- here::here("data/raw/location_map.csv")

use_map <- if (file.exists(curated_path)) {
  message("Using curated map: data/raw/location_map.csv")
  readr::read_csv(curated_path, show_col_types = FALSE) %>% janitor::clean_names()
} else {
  message("No curated map found. Using suggested map for now.")
  suggested
}

# Validate map
required_cols <- c("chapter", "location")
stopifnot(all(required_cols %in% names(use_map)))

# Ensure all chapters 1..37 are present exactly once
stopifnot(setequal(use_map$chapter, 1:37))

# If 'order' is missing, make one (increase when location changes)
if (!"order" %in% names(use_map) || any(is.na(use_map$order))) {
  use_map <- use_map %>%
    dplyr::arrange(chapter) %>%
    dplyr::mutate(order = 1L + cumsum(dplyr::coalesce(location != dplyr::lag(location), FALSE)))
}

# Attach locations to chapters (keep narrator text)
segments_by_chapter <- d %>%
  dplyr::left_join(use_map, by = "chapter") %>%
  dplyr::arrange(chapter) %>%
  dplyr::mutate(
    location = dplyr::coalesce(location, "Transit"),
    order    = dplyr::coalesce(order, 999L),
    text_narr = text_no_dialogue_cons   # primary narrator text for analysis
  ) %>%
  dplyr::select(
    chapter, location, order,
    text_narr, text_clean,
    text_no_dialogue_strict, text_no_dialogue_cons, text_dialogue_only,
    wc_full, wc_str, wc_cons, wc_dial,
    removed_ratio_strict, removed_ratio_cons, dialogue_ratio
  )

stopifnot(nrow(segments_by_chapter) == 37L)

# Collapse chapters into location segments (one row per journey stop) 
segments_by_location <- segments_by_chapter %>%
  dplyr::arrange(order, chapter) %>%
  dplyr::group_by(order, location) %>%
  dplyr::summarise(
    chapters = paste(chapter, collapse = ","),
    text_narr = paste(text_narr, collapse = " "),
    text_clean = paste(text_clean, collapse = " "),
    wc_narr = stringr::str_count(text_narr, stringr::boundary("word")),
    .groups = "drop"
  ) %>%
  dplyr::arrange(order)

# Console summary
print(segments_by_location %>%
        dplyr::summarise(
          n_locations = dplyr::n(),
          min_tokens  = min(wc_narr),
          median_tokens = stats::median(wc_narr),
          max_tokens  = max(wc_narr)
        ))
print(segments_by_location %>% dplyr::count(location, sort = TRUE))

# Save outputs
readr::write_rds(segments_by_chapter,  here::here("data/processed/segments_by_chapter.rds"))
readr::write_rds(segments_by_location, here::here("data/processed/segments_by_location.rds"))
message("Saved: data/processed/segments_by_chapter.rds and .../segments_by_location.rds")
