# R/12_geo_map.R
# Goal: Produce a publication-ready world map showing Fogg’s journey route.
#
# Inputs (one of):
#   data/processed/location_map.rds          # preferred (from 05_location_map.R)
#     - expected columns: order, location, lon, lat
#   OR
#   data/processed/sent_stop_summary.rds     # fallback if it also has lon/lat
#
# Output:
#   outputs/figs/fig_world_route.png

# ---- packages (use ::, no library()) ---------------------------------------

stopifnot(requireNamespace("here",          quietly = TRUE))
stopifnot(requireNamespace("readr",         quietly = TRUE))
stopifnot(requireNamespace("dplyr",         quietly = TRUE))
stopifnot(requireNamespace("ggplot2",       quietly = TRUE))
stopifnot(requireNamespace("sf",            quietly = TRUE))
stopifnot(requireNamespace("rnaturalearth", quietly = TRUE))

# ---- helper: load stop locations -------------------------------------------

p_loc_map   <- here::here("data/processed/location_map.rds")
p_stop_summ <- here::here("data/processed/sent_stop_summary.rds")

if (file.exists(p_loc_map)) {
  # Preferred: explicit location map from 05_location_map.R
  stops <- readr::read_rds(p_loc_map)
} else if (file.exists(p_stop_summ)) {
  # Fallback: use stop summary if it already has lon/lat
  stops <- readr::read_rds(p_stop_summ)
} else {
  stop(
    "Could not find either 'location_map.rds' or 'sent_stop_summary.rds'.\n",
    "Run 05_location_map.R (and 09_scoring.R if needed) before 12_geo_map.R."
  )
}

# ---- sanity checks on columns ----------------------------------------------

needed_cols <- c("order", "location", "lon", "lat")

if (!all(needed_cols %in% names(stops))) {
  stop(
    "Location data is missing one or more required columns: ",
    paste(needed_cols, collapse = ", "), ".\n",
    "Check that 05_location_map.R created 'lon' and 'lat' columns."
  )
}

# Keep one row per stop, ordered by journey order
stops_df <- stops |>
  dplyr::arrange(order) |>
  dplyr::distinct(order, .keep_all = TRUE)

# ---- convert to sf objects --------------------------------------------------

stops_sf <- sf::st_as_sf(
  stops_df,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# world basemap (Natural Earth)
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# ---- build the map ----------------------------------------------------------

# Simple margins around the route
x_range <- range(stops_df$lon, na.rm = TRUE)
y_range <- range(stops_df$lat, na.rm = TRUE)
x_pad   <- diff(x_range) * 0.15
y_pad   <- diff(y_range) * 0.20

plt_route <- ggplot2::ggplot() +
  # world outline
  ggplot2::geom_sf(
    data   = world_sf,
    fill   = "#f9fafb",
    colour = "#9ca3af",
    linewidth = 0.2
  ) +
  # journey path (in order)
  ggplot2::geom_path(
    data = stops_df,
    ggplot2::aes(x = lon, y = lat, group = 1),
    linewidth = 0.8,
    colour    = "#ef4444"
  ) +
  # stop points
  ggplot2::geom_point(
    data = stops_df,
    ggplot2::aes(x = lon, y = lat),
    size   = 2.3,
    colour = "#ef4444",
    fill   = "#fee2e2",
    shape  = 21,
    stroke = 0.4
  ) +
  # stop labels (shortened if needed)
  ggplot2::geom_text(
    data = stops_df,
    ggplot2::aes(
      x = lon,
      y = lat,
      label = location
    ),
    size  = 3,
    vjust = -0.7
  ) +
  ggplot2::coord_sf(
    xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
    ylim = c(y_range[1] - y_pad, y_range[2] + y_pad),
    expand = FALSE
  ) +
  ggplot2::labs(
    title    = "Around the World in Eighty Days: Journey Route",
    subtitle = "Route of Phileas Fogg with major narrative stops",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(colour = "#e5e7eb", linewidth = 0.3),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text        = ggplot2::element_blank(),
    axis.ticks       = ggplot2::element_blank(),
    plot.title.position = "plot",
    plot.title       = ggplot2::element_text(face = "bold"),
    plot.subtitle    = ggplot2::element_text(colour = "#4b5563")
  )

# ---- save to outputs/figs ---------------------------------------------------

outdir_figs <- here::here("outputs/figs")
dir.create(outdir_figs, showWarnings = FALSE, recursive = TRUE)

outfile <- file.path(outdir_figs, "fig_world_route.png")

ggplot2::ggsave(
  filename = outfile,
  plot     = plt_route,
  width    = 9,
  height   = 5.5,
  dpi      = 300
)

message("Saved: ", outfile)

# Preview in RStudio if running interactively
if (interactive()) print(plt_route)
