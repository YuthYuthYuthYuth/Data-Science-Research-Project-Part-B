# R/11b_plot_lexicon_compare.R
# Goal: Compare AFINN vs Bing vs NRC vs SenticNet per stop (clean lines, no ribbons),
#       with location labels.
# Input :
#   data/processed/tokens_stop_scored.rds
# Outputs:
#   outputs/figs/lexicon_compare_curve_clean.png
#   outputs/figs/lexicon_compare_facet_clean.png

# load
p_tok <- here::here("data/processed/tokens_stop_scored.rds")
stopifnot(file.exists(p_tok))
tok <- readr::read_rds(p_tok)

need <- c("order","location",
          "val_afinn_adj","val_bing_adj","val_nrc_adj","val_sentic_adj")
stopifnot(all(need %in% names(tok)))

# reshape to long and keep per-lexicon scored tokens
long <- tidyr::pivot_longer(
  tok,
  cols = c(val_afinn_adj, val_bing_adj, val_nrc_adj, val_sentic_adj),
  names_to = "lexicon", values_to = "val"
) |>
  dplyr::mutate(
    lexicon = dplyr::recode(
      lexicon,
      val_afinn_adj  = "AFINN",
      val_bing_adj   = "Bing",
      val_nrc_adj    = "NRC",
      val_sentic_adj = "SenticNet"
    ),
    has_lex = val != 0
  )

# per-stop, per-lexicon means over tokens that lexicon scored
summ_lex <- long |>
  dplyr::group_by(order, location, lexicon) |>
  dplyr::summarise(
    tokens_total         = dplyr::n(),
    tokens_scored_lex    = sum(has_lex),
    mean_val_lex_scored  = ifelse(tokens_scored_lex > 0,
                                  sum(val[has_lex]) / tokens_scored_lex,
                                  NA_real_),
    .groups = "drop"
  ) |>
  dplyr::arrange(order, lexicon)

# (still compute CIs for potential appendix; not plotted here)
boot_ci <- function(x, n_boot = 1200L) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(NA_real_, NA_real_))
  m <- replicate(n_boot, mean(sample(x, replace = TRUE)))
  stats::quantile(m, c(0.025, 0.975), names = FALSE, na.rm = TRUE)
}
boot_tbl <- long |>
  dplyr::filter(has_lex) |>
  dplyr::group_by(order, location, lexicon) |>
  dplyr::summarise(ci = list(boot_ci(val)), .groups = "drop") |>
  tidyr::unnest_wider(ci, names_sep = "_") |>
  dplyr::rename(ci_lo = ci_1, ci_hi = ci_2)

summ_lex <- dplyr::left_join(summ_lex, boot_tbl,
                             by = c("order","location","lexicon"))

# x-axis labels from locations
labs_tbl <- summ_lex %>%
  dplyr::distinct(order, location) %>%
  dplyr::arrange(order)

labs <- labs_tbl %>%
  dplyr::mutate(
    label = stringr::str_replace_all(location, "/", " / "),
    label = stringr::str_trunc(label, 18)
  ) %>%
  dplyr::pull(label)

# output dir
outdir <- here::here("outputs/figs")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# colours
pal <- c("AFINN"    = "#2563eb",
         "Bing"     = "#f59e0b",
         #"NRC"      = "#10b981",
         "SenticNet"= "#ef4444")

# Plot A: all lexicons in one panel (no ribbons)
plt_compare <- ggplot2::ggplot(
  summ_lex, ggplot2::aes(order, mean_val_lex_scored, colour = lexicon)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                      linewidth = 0.3, colour = "#9ca3af") +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_colour_manual(values = pal, name = "Lexicon") +
  ggplot2::scale_x_continuous(breaks = labs_tbl$order, labels = labs) +
  ggplot2::labs(
    x = "Journey order",
    y = "Mean valence (per-lexicon, scored tokens)",
    title = "Lexicon Comparison Across Journey Stops",
    subtitle = "AFINN, Bing, NRC, and SenticNet (clean lines, no ribbons)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 legend.position = "top",
                 plot.title.position = "plot",
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggplot2::ggsave(file.path(outdir, "lexicon_compare_curve_clean.png"),
                plt_compare, width = 9, height = 5.2, dpi = 300)

# Plot B: facets per lexicon (no ribbons)
plt_facet <- ggplot2::ggplot(
  summ_lex, ggplot2::aes(order, mean_val_lex_scored, colour = lexicon)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                      linewidth = 0.3, colour = "#9ca3af") +
  ggplot2::geom_line(linewidth = 0.9, show.legend = FALSE) +
  ggplot2::geom_point(size = 2, show.legend = FALSE) +
  ggplot2::scale_colour_manual(values = pal, guide = "none") +
  ggplot2::scale_x_continuous(breaks = labs_tbl$order, labels = labs) +
  ggplot2::labs(x = "Journey order", y = "Mean valence (scored tokens)",
                title = "Lexicon-Specific Trends (AFINN, Bing, NRC, SenticNet)") +
  ggplot2::facet_wrap(~ lexicon, ncol = 1, scales = "free_y") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 plot.title.position = "plot",
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggplot2::ggsave(file.path(outdir, "lexicon_compare_facet_clean.png"),
                plt_facet, width = 7.5, height = 8.0, dpi = 300)

# Preview in RStudio
if (interactive()) { print(plt_compare); print(plt_facet) }
