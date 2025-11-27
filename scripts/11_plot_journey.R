# R/11_plot_journey.R
# Goal: Visualise the emotional journey across locations (stops).
# Inputs :
#   data/processed/sent_stop_summary.rds
#   data/processed/sent_stop_loess.rds
# Outputs:
#   outputs/figs/journey_curve.png
#   outputs/figs/journey_bars.png

# load
p_stop  <- here::here("data/processed/sent_stop_summary.rds")
p_loess <- here::here("data/processed/sent_stop_loess.rds")
stopifnot(file.exists(p_stop), file.exists(p_loess))

sent_stop       <- readr::read_rds(p_stop)
sent_stop_loess <- readr::read_rds(p_loess)

need1 <- c("order","location","mean_val_base_scored","coverage")
need2 <- c("order","val_loess")
stopifnot(all(need1 %in% names(sent_stop)))
stopifnot(all(need2 %in% names(sent_stop_loess)))

# add empty CI cols if absent (keeps layering code simple)
if (!("ci_lo" %in% names(sent_stop))) sent_stop$ci_lo <- NA_real_
if (!("ci_hi" %in% names(sent_stop))) sent_stop$ci_hi <- NA_real_

# output dir
outdir <- here::here("outputs/figs")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# common helpers
# short labels for x-axis (truncate & prettify slashes)
labs <- sent_stop %>%
  dplyr::arrange(order) %>%
  dplyr::mutate(label = stringr::str_replace_all(location, "/", " / "),
                label = stringr::str_trunc(label, 18)) %>%
  dplyr::pull(label)

# Figure 1: Journey curve (coloured)
df_curve <- sent_stop %>%
  dplyr::select(order, location, mean_val_base_scored, ci_lo, ci_hi) %>%
  dplyr::left_join(sent_stop_loess %>% dplyr::select(order, val_loess), by = "order")

plt_curve <- ggplot2::ggplot(df_curve, ggplot2::aes(x = order)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "#9ca3af") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
                       fill = "#93c5fd", alpha = 0.25, linewidth = 0, na.rm = TRUE) +
  ggplot2::geom_line(ggplot2::aes(y = mean_val_base_scored, colour = "Per-stop mean"), linewidth = 0.9) +
  ggplot2::geom_point(ggplot2::aes(y = mean_val_base_scored, colour = "Per-stop mean"), size = 2) +
  ggplot2::geom_line(ggplot2::aes(y = val_loess, colour = "LOESS smooth"), linewidth = 1.1) +
  ggplot2::scale_color_manual(
    NULL,
    values = c("Per-stop mean" = "#2563eb",  # blue
               "LOESS smooth" = "#ef4444")   # red
  ) +
  ggplot2::scale_x_continuous(breaks = sent_stop$order, labels = labs) +
  ggplot2::labs(
    x = "Journey order (location sequence)",
    y = "Mean valence (scored tokens)",
    title = "Emotional Journey Across Stops",
    subtitle = "Blue = per-stop mean; Red = LOESS smooth; shaded = bootstrap 95% CI"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    axis.text.x         = ggplot2::element_text(angle = 45, hjust = 1),
    panel.grid.minor    = ggplot2::element_blank(),
    legend.position     = "top",
    plot.title.position = "plot"
  )

ggplot2::ggsave(file.path(outdir, "journey_curve.png"), plt_curve, width = 9, height = 5.2, dpi = 300)
print(plt_curve)

# Figure 2: Bars by stop (fill by valence, alpha by coverage)
# Figure 2 (clean): bars colored by valence; coverage as % labels
labs <- sent_stop |>
  dplyr::arrange(order) |>
  dplyr::mutate(label = stringr::str_replace_all(location, "/", " / "),
                label = stringr::str_trunc(label, 18)) |>
  dplyr::pull(label)

plt_bars_dec <- ggplot2::ggplot(
  sent_stop, ggplot2::aes(x = factor(order), y = mean_val_base_scored)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "#9ca3af") +
  ggplot2::geom_col(ggplot2::aes(fill = mean_val_base_scored),
                    width = 0.7, colour = "white", linewidth = 0.25) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
                         width = 0.15, linewidth = 0.3, na.rm = TRUE) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = pmax(mean_val_base_scored, 0) + 0.03,
      label = scales::number(coverage, accuracy = 0.01)  # <-- decimals
    ),
    size = 3, colour = "grey30"
  ) +
  ggplot2::scale_fill_gradient2(name = "Valence", midpoint = 0,
                                low = "#ef4444", mid = "grey92", high = "#2563eb") +
  ggplot2::scale_x_discrete(labels = labs) +
  ggplot2::labs(x = "Journey order", y = "Mean valence (scored tokens)",
                title = "Per-Stop Sentiment (coverage shown as decimals)") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.position = "top",
                 plot.title.position = "plot")

ggplot2::ggsave(here::here("outputs/figs/journey_bars.png"),
                plt_bars_dec, width = 7.5, height = 4.2, dpi = 300)
print(plt_bars_dec)

