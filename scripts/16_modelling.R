# R/16_modelling.R
# Goal: Fit a controlled model:
#       mean sentiment ~ region + plot_proxy
#       and export tidy tables for the report.
# Inputs:
#   data/processed/sent_stop_with_proxy.rds
# Outputs:
#   data/processed/sent_stop_modelled.rds
#   outputs/tables/model_coefficients.csv
#   outputs/tables/model_fit.csv
#   outputs/tables/stop_sentiment_with_model.csv

# ---- packages (no library(), use :: instead) ------------------------------
stopifnot(requireNamespace("here",   quietly = TRUE))
stopifnot(requireNamespace("readr",  quietly = TRUE))
stopifnot(requireNamespace("dplyr",  quietly = TRUE))
stopifnot(requireNamespace("broom",  quietly = TRUE))

# Make dplyr pipe available locally (without using library())
`%>%` <- dplyr::`%>%`

# ---- load data ------------------------------------------------------------
p_stop_proxy <- here::here("data/processed/sent_stop_with_proxy.rds")
stopifnot(file.exists(p_stop_proxy))

dat <- readr::read_rds(p_stop_proxy)

stopifnot(all(c("order", "location",
                "mean_val_base_scored",
                "plot_proxy", "coverage") %in% names(dat)))

# ---- drop Transit and define regions -------------------------------------
# Transit is not a true narrative stop and has no clear region,
# so we omit it from the regression model.

dat_mod <- dat %>%
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

# quick sanity check
if (any(is.na(dat_mod$region))) {
  warning("Some stops have NA region labels; check 'region' case_when().")
}

# ---- fit linear model -----------------------------------------------------
# Response: mean_val_base_scored (per-stop sentiment, adjusted)
# Predictors: categorical region + plot_proxy (intensity proxy)

mod <- stats::lm(
  mean_val_base_scored ~ region + plot_proxy,
  data = dat_mod
)

# tidy tables for report
mod_tidy   <- broom::tidy(mod, conf.int = TRUE)
mod_glance <- broom::glance(mod)
mod_aug    <- broom::augment(mod, dat_mod)

# ---- combine augmented results with stop info -----------------------------
dat_modelled <- dat_mod %>%
  dplyr::select(order, location, region,
                mean_val_base_scored, plot_proxy, coverage) %>%
  dplyr::bind_cols(
    dplyr::select(mod_aug, .fitted, .resid)
  )

# ---- save outputs ---------------------------------------------------------
outdir <- here::here("outputs/tables")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# coefficient-level results
readr::write_csv(mod_tidy,
                 file.path(outdir, "model_coefficients.csv"))

# overall model fit (R^2, adj R^2, etc.)
readr::write_csv(mod_glance,
                 file.path(outdir, "model_fit.csv"))

# stop-level table with fitted values & residuals
readr::write_csv(dat_modelled,
                 file.path(outdir, "stop_sentiment_with_model.csv"))

# also keep an RDS for later plotting / diagnostics
readr::write_rds(dat_modelled,
                 here::here("data/processed/sent_stop_modelled.rds"))

message(
  "Saved: model_coefficients.csv, model_fit.csv, ",
  "stop_sentiment_with_model.csv, model_fit.csv, ",
  "and sent_stop_modelled.rds"
)
