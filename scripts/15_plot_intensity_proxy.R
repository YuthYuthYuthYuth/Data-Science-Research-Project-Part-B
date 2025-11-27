# R/15_plot_intensity_proxy.R
# Goal: Create a plot-intensity proxy per stop (via UDPipe POS tagging)
#       and join it to the stop-level sentiment summary.
#
# Inputs:
#   data/processed/segments_by_location.rds   # per-stop narrator text
#   data/processed/sent_stop_summary.rds      # stop-level sentiment from 09_scoring.R
#
# Outputs:
#   data/processed/stop_plot_proxy.rds
#   data/processed/sent_stop_with_proxy.rds
#   outputs/tables/stop_sentiment_with_proxy.csv

stopifnot(requireNamespace("here", quietly = TRUE))
stopifnot(requireNamespace("readr", quietly = TRUE))
stopifnot(requireNamespace("dplyr", quietly = TRUE))
stopifnot(requireNamespace("udpipe", quietly = TRUE))

# -------------------------------------------------------------------
# 1. Load data
# -------------------------------------------------------------------
p_seg  <- here::here("data/processed/segments_by_location.rds")
p_sent <- here::here("data/processed/sent_stop_summary.rds")

stopifnot(file.exists(p_seg), file.exists(p_sent))

seg_loc   <- readr::read_rds(p_seg)
sent_stop <- readr::read_rds(p_sent)

# We expect at least: order, location, and some narrator text column
stopifnot(all(c("order", "location") %in% names(seg_loc)))

text_col <- dplyr::case_when(
  "text_narr" %in% names(seg_loc) ~ "text_narr",
  "text"      %in% names(seg_loc) ~ "text",
  TRUE ~ NA_character_
)

if (is.na(text_col)) {
  stop("Could not find a text column in segments_by_location.rds (expected 'text_narr' or 'text').")
}

seg_loc <- seg_loc |>
  dplyr::mutate(text_for_udpipe = .data[[text_col]])

# -------------------------------------------------------------------
# 2. Load or download UDPipe English model
# -------------------------------------------------------------------
model_dir <- here::here("data/external/udpipe")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# We will use the English EWT model (good general-purpose model)
model_path <- list.files(model_dir, pattern = "\\.udpipe$", full.names = TRUE)

if (length(model_path) == 0L) {
  message("No UDPipe model found in data/external/udpipe/. Downloading english-ewt model...")
  dl <- udpipe::udpipe_download_model(language = "english-ewt", model_dir = model_dir)
  model_path <- dl$file_model
} else {
  model_path <- model_path[1]
}

ud_model <- udpipe::udpipe_load_model(model_path)

# -------------------------------------------------------------------
# 3. Run UDPipe annotation per stop
# -------------------------------------------------------------------
# Use 'order' as doc_id so we can join back easily
anno <- udpipe::udpipe_annotate(
  ud_model,
  x      = seg_loc$text_for_udpipe,
  doc_id = seg_loc$order
)
anno <- as.data.frame(anno)

# We expect columns: doc_id, token, upos, etc.
stopifnot(all(c("doc_id", "token", "upos") %in% names(anno)))

# -------------------------------------------------------------------
# 4. Compute a simple "plot intensity" proxy
#    Example: proportion of VERB tokens per stop
# -------------------------------------------------------------------
plot_proxy <- anno |>
  dplyr::group_by(doc_id) |>
  dplyr::summarise(
    n_tokens   = dplyr::n(),
    n_verbs    = sum(upos == "VERB", na.rm = TRUE),
    prop_verbs = ifelse(n_tokens > 0, n_verbs / n_tokens, NA_real_),
    .groups    = "drop"
  ) |>
  dplyr::mutate(
    order      = as.integer(doc_id),
    plot_proxy = prop_verbs
  ) |>
  dplyr::select(order, plot_proxy, n_tokens, n_verbs)

# Quick sanity check
stopifnot(all(plot_proxy$order %in% seg_loc$order))

# Save the proxy alone
readr::write_rds(plot_proxy, here::here("data/processed/stop_plot_proxy.rds"))

# -------------------------------------------------------------------
# 5. Join proxy to stop-level sentiment summary
# -------------------------------------------------------------------
stopifnot("order" %in% names(sent_stop))

sent_stop_with_proxy <- sent_stop |>
  dplyr::left_join(plot_proxy |> dplyr::select(order, plot_proxy),
                   by = "order")

# Save RDS + CSV for modelling (Block B, step 3)
readr::write_rds(
  sent_stop_with_proxy,
  here::here("data/processed/sent_stop_with_proxy.rds")
)

outdir_tables <- here::here("outputs/tables")
dir.create(outdir_tables, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  sent_stop_with_proxy,
  file.path(outdir_tables, "stop_sentiment_with_proxy.csv")
)

# -------------------------------------------------------------------
# 6. Console preview
# -------------------------------------------------------------------
cat("=== Stop-level sentiment + plot intensity proxy (first 10 rows) ===\n")
print(utils::head(
  sent_stop_with_proxy |>
    dplyr::select(order, location, mean_val_base_scored, plot_proxy, coverage),
  10
), row.names = FALSE)

message(
  "\nSaved: data/processed/stop_plot_proxy.rds, ",
  "data/processed/sent_stop_with_proxy.rds, ",
  "and outputs/tables/stop_sentiment_with_proxy.csv"
)
