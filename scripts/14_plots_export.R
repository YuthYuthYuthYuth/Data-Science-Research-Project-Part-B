# R/14_plots_export.R
# Goal: Export final, publication-ready plots for the thesis
#  - Journey sentiment curve (per stop)
#  - Lexicon comparison curves (AFINN, Bing, SenticNet)
#
# Inputs (already produced earlier in the pipeline):
#   data/processed/sent_stop_summary.rds
#   data/processed/tokens_stop_scored.rds
#
# Outputs (to outputs/figs/):
#   fig_journey_sentiment_stops.png
#   fig_lexicon_compare_curve.png
#   fig_lexicon_compare_facet.png

# ---- packages -----------------------------------------------------------

stopifnot(requireNamespace("here",    quietly = TRUE))
stopifnot(requireNamespace("readr",   quietly = TRUE))
stopifnot(requireNamespace("dplyr",   quietly = TRUE))
stopifnot(requireNamespace("stringr", quietly = TRUE))
stopifnot(requireNamespace("ggplot2", quietly = TRUE))
stopifnot(requireNamespace("tidyr",   quietly = TRUE))

# ---- output dir ---------------------------------------------------------

dir_figs <- here::here("outputs/figs")
dir.create(dir_figs, recursive = TRUE, showWarnings = FALSE)

# Helper theme: consistent minimal style for thesis
base_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      legend.position = "top",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

# ========================================================================
# 1. Journey sentiment curve (per stop)
# ========================================================================

p_stop <- here::here("data/processed/sent_stop_summary.rds")
stopifnot(file.exists(p_stop))

sent_stop <- readr::read_rds(p_stop)

stopifnot(all(c("order", "location", "mean_val_base_scored") %in% names(sent_stop)))

has_ci <- all(c("ci_lo", "ci_hi") %in% names(sent_stop))

# x-axis labels from locations
labs_tbl <- sent_stop %>%
  dplyr::distinct(order, location) %>%
  dplyr::arrange(order)

labs <- labs_tbl %>%
  dplyr::mutate(
    label = stringr::str_replace_all(location, "/", " / "),
    label = stringr::str_trunc(label, 18)
  ) %>%
  dplyr::pull(label)

plt_journey <- ggplot2::ggplot(
  sent_stop,
  ggplot2::aes(x = order, y = mean_val_base_scored)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype  = "dashed",
    linewidth = 0.3,
    colour    = "#9ca3af"
  ) +
  {
    if (has_ci) {
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
        alpha = 0.18
      )
    } else {
      ggplot2::geom_blank()
    }
  } +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_continuous(
    breaks = labs_tbl$order,
    labels = labs
  ) +
  ggplot2::labs(
    x = "Journey order",
    y = "Mean sentiment (negation/intensity-adjusted)",
    title = "Sentiment Along Phileas Fogg's Journey"
  ) +
  base_theme()

ggplot2::ggsave(
  filename = file.path(dir_figs, "fig_journey_sentiment_stops.png"),
  plot     = plt_journey,
  width    = 9,
  height   = 5.2,
  dpi      = 300
)

message("Saved: fig_journey_sentiment_stops.png")

# ========================================================================
# 2. Lexicon comparison curves (AFINN, Bing, SenticNet)
# ========================================================================

p_tok <- here::here("data/processed/tokens_stop_scored.rds")
stopifnot(file.exists(p_tok))

tok <- readr::read_rds(p_tok)

stopifnot(all(c("order", "location") %in% names(tok)))

# Work out which adjusted valence columns are present
val_map <- c()

if ("val_afinn_adj" %in% names(tok))      val_map["val_afinn_adj"]      <- "AFINN"
if ("val_bing_adj"  %in% names(tok))      val_map["val_bing_adj"]       <- "Bing"
if ("val_sentic_adj" %in% names(tok))     val_map["val_sentic_adj"]     <- "SenticNet"
if ("val_senticnet_adj" %in% names(tok))  val_map["val_senticnet_adj"]  <- "SenticNet"

if (length(val_map) < 2L) {
  stop("Expected at least two lexicon columns (e.g. val_afinn_adj, val_bing_adj, val_sentic*_adj).")
}

long <- tidyr::pivot_longer(
  tok,
  cols      = dplyr::all_of(names(val_map)),
  names_to  = "lexicon_raw",
  values_to = "val"
) %>%
  dplyr::mutate(
    lexicon = unname(val_map[lexicon_raw]),
    has_lex = val != 0
  )

summ_lex <- long %>%
  dplyr::group_by(order, location, lexicon) %>%
  dplyr::summarise(
    tokens_total        = dplyr::n(),
    tokens_scored_lex   = sum(has_lex),
    mean_val_lex_scored = dplyr::if_else(
      tokens_scored_lex > 0,
      sum(val[has_lex]) / tokens_scored_lex,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(order, lexicon)

# axis labels (reuse same as journey curve)
labs_tbl_lex <- summ_lex %>%
  dplyr::distinct(order, location) %>%
  dplyr::arrange(order)

labs_lex <- labs_tbl_lex %>%
  dplyr::mutate(
    label = stringr::str_replace_all(location, "/", " / "),
    label = stringr::str_trunc(label, 18)
  ) %>%
  dplyr::pull(label)

# colour palette (handles up to 3 lexicons)
lex_levels <- sort(unique(summ_lex$lexicon))
pal_default <- c(
  "AFINN"    = "#2563eb",
  "Bing"     = "#f59e0b",
  "SenticNet"= "#10b981"
)
pal_use <- pal_default[lex_levels]

# ---- Plot A: all lexicons in one panel ----------------------------------

plt_lex_curve <- ggplot2::ggplot(
  summ_lex,
  ggplot2::aes(order, mean_val_lex_scored, colour = lexicon)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype  = "dashed",
    linewidth = 0.3,
    colour    = "#9ca3af"
  ) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_colour_manual(values = pal_use, name = "Lexicon") +
  ggplot2::scale_x_continuous(
    breaks = labs_tbl_lex$order,
    labels = labs_lex
  ) +
  ggplot2::labs(
    x = "Journey order",
    y = "Mean valence (per-lexicon, scored tokens)",
    title = "Lexicon Comparison Across Journey Stops"
  ) +
  base_theme()

ggplot2::ggsave(
  filename = file.path(dir_figs, "fig_lexicon_compare_curve.png"),
  plot     = plt_lex_curve,
  width    = 9,
  height   = 5.2,
  dpi      = 300
)

message("Saved: fig_lexicon_compare_curve.png")

# ---- Plot B: facets per lexicon ----------------------------------------

plt_lex_facet <- ggplot2::ggplot(
  summ_lex,
  ggplot2::aes(order, mean_val_lex_scored, colour = lexicon)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype  = "dashed",
    linewidth = 0.3,
    colour    = "#9ca3af"
  ) +
  ggplot2::geom_line(linewidth = 0.9, show.legend = FALSE) +
  ggplot2::geom_point(size = 2, show.legend = FALSE) +
  ggplot2::scale_colour_manual(values = pal_use, guide = "none") +
  ggplot2::scale_x_continuous(
    breaks = labs_tbl_lex$order,
    labels = labs_lex
  ) +
  ggplot2::labs(
    x = "Journey order",
    y = "Mean valence (scored tokens)",
    title = "Lexicon-Specific Sentiment Trends"
  ) +
  ggplot2::facet_wrap(~ lexicon, ncol = 1, scales = "free_y") +
  base_theme()

ggplot2::ggsave(
  filename = file.path(dir_figs, "fig_lexicon_compare_facet.png"),
  plot     = plt_lex_facet,
  width    = 7.5,
  height   = 8,
  dpi      = 300
)

message("Saved: fig_lexicon_compare_facet.png")
message("All core plots exported to outputs/figs/.")
