# R/14_tables.R
# Create publication tables for per-stop sentiment and extremes.

# packages (no attach)
stopifnot(requireNamespace("here", quietly = TRUE))
stopifnot(requireNamespace("readr", quietly = TRUE))
stopifnot(requireNamespace("dplyr", quietly = TRUE))
stopifnot(requireNamespace("knitr", quietly = TRUE))

# load data
p_stop <- here::here("data/processed/sent_stop_summary.rds")
stopifnot(file.exists(p_stop))
df <- readr::read_rds(p_stop)

# required cols
need <- c("order","location","mean_val_base_scored","ci_lo","ci_hi","coverage")
stopifnot(all(need %in% names(df)))

# prepare outputs dir
outdir <- here::here("outputs/tables")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# build full per-stop table
tab <- df |>
  dplyr::arrange(order) |>
  dplyr::transmute(
    Order    = order,
    Location = location,
    Mean     = round(mean_val_base_scored, 3),
    `95% CI` = sprintf("[%.3f, %.3f]", ci_lo, ci_hi),
    Coverage = sprintf("%.2f", coverage)  # decimals (e.g., 0.11)
  )
print(tab)
# Print to console
cat("\n=== Per-stop sentiment summary (first 10 rows) ===\n")
print(utils::head(tab, 10), row.names = FALSE)
