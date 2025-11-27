# R/17_partial_correlations.R
# Goal: Compute partial Spearman correlations:
#       (1) sentiment ⟂ region | plot_proxy
#       (2) sentiment ⟂ plot_proxy | region
# Inputs:
#   data/processed/sent_stop_with_proxy.rds
# Output:
#   outputs/tables/partial_correlations.csv

stopifnot(requireNamespace("here", quietly = TRUE))
stopifnot(requireNamespace("readr", quietly = TRUE))
stopifnot(requireNamespace("dplyr", quietly = TRUE))
stopifnot(requireNamespace("ppcor", quietly = TRUE))

# ---- load ------------------------------------------------------
p_dat <- here::here("data/processed/sent_stop_with_proxy.rds")
stopifnot(file.exists(p_dat))

dat <- readr::read_rds(p_dat)

# ---- remove Transit & define regions (same as in 16_modelling.R) ----
dat2 <- dat %>%
  dplyr::filter(location != "Transit") %>%
  dplyr::mutate(
    region = dplyr::case_when(
      location %in% c("London", "Atlantic/Liverpool") ~ "Europe",
      location %in% c("Suez/Aden")                    ~ "Middle East",
      location %in% c("Bombay", "Calcutta")           ~ "South Asia",
      location %in% c("Hong Kong", "Yokohama/Japan")  ~ "East Asia",
      location %in% c("San Francisco", "New York")    ~ "North America",
      TRUE                                            ~ "Other"
    ),
    region = factor(region)
  )

# sanity check
stopifnot(all(!is.na(dat2$region)))

# ---- prepare numeric region coding for correlation ----------
# Spearman requires ordinal/numeric input
dat2$region_num <- as.numeric(dat2$region)

# ---- extract variables ---------------------------------------
sentiment  <- dat2$mean_val_base_scored
region_num <- dat2$region_num
plot_proxy <- dat2$plot_proxy

# ---- compute partial Spearman correlations --------------------
pcor_region <- ppcor::pcor.test(
  x = sentiment,
  y = region_num,
  z = plot_proxy,
  method = "spearman"
)

pcor_plot <- ppcor::pcor.test(
  x = sentiment,
  y = plot_proxy,
  z = region_num,
  method = "spearman"
)

# ---- build output table ---------------------------------------
out <- tibble::tibble(
  comparison = c("sentiment ~ region | plot_proxy",
                 "sentiment ~ plot_proxy | region"),
  estimate   = c(pcor_region$estimate, pcor_plot$estimate),
  p_value    = c(pcor_region$p.value, pcor_plot$p.value),
  statistic  = c(pcor_region$statistic, pcor_plot$statistic),
  n          = c(pcor_region$n,        pcor_plot$n)
)

# ---- save -----------------------------------------------------
outdir <- here::here("outputs/tables")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

readr::write_csv(out, file.path(outdir, "partial_correlations.csv"))

message("Saved: partial_correlations.csv")
