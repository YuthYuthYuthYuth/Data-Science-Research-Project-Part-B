# 18_prepare_senticnet.R
# Goal: convert raw SenticNet text file to a simple word-level lexicon
#       data/external/senticnet_wordlist.csv  (cols: word, sentic_score)

stopifnot(requireNamespace("here",    quietly = TRUE))
stopifnot(requireNamespace("readr",   quietly = TRUE))
stopifnot(requireNamespace("dplyr",   quietly = TRUE))
stopifnot(requireNamespace("stringr", quietly = TRUE))

# path to the raw SenticNet txt file you downloaded
p_raw <- here::here("data/external/senticnet.txt")
stopifnot(file.exists(p_raw))

# SenticNet layout in your file is:
# POLARITY <tab> <tab> CONCEPT
# negative <tab> <tab> aah
# ...
# -> so we read THREE columns and throw away the empty middle one.
sentic_raw <- readr::read_delim(
  file           = p_raw,
  delim          = "\t",
  col_names      = c("polarity", "dummy", "concept"),
  skip           = 1,              # skip header line "POLARITY  CONCEPT"
  comment        = "#",
  trim_ws        = TRUE,
  show_col_types = FALSE
)

# Quick sanity check (optional)
print(utils::head(sentic_raw, 10))

# Build a simple word-level polarity lexicon.
# Map "positive" -> +1, "negative" -> -1.
sentic_wordlist <- sentic_raw %>%
  dplyr::filter(!is.na(concept),
                polarity %in% c("positive", "negative")) %>%
  dplyr::mutate(
    word = stringr::str_to_lower(concept),
    # keep only single-word concepts (no underscores)
    word = stringr::str_replace_all(word, "_", " "),
    sentic_score = dplyr::case_when(
      polarity == "positive" ~  1,
      polarity == "negative" ~ -1,
      TRUE                   ~  0
    )
  ) %>%
  dplyr::filter(!stringr::str_detect(word, "\\s")) %>%  # only single tokens
  dplyr::select(word, sentic_score) %>%
  dplyr::distinct()

out_path <- here::here("data/external/senticnet_wordlist.csv")
readr::write_csv(sentic_wordlist, out_path)

message(
  "Saved SenticNet wordlist to: ", out_path,
  " (", nrow(sentic_wordlist), " rows)"
)

sentic <- readr::read_csv(
  here::here("data/external/senticnet_wordlist.csv"),
  show_col_types = FALSE
)

dplyr::glimpse(sentic)
dplyr::sample_n(sentic, 10)
summary(sentic$sentic_score)

