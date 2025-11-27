# R/12_geo_map.R
# Goal: Plot Phileas Fogg's itinerary on a world map.
#
# Inputs :
#   data/processed/segments_by_location.rds
# Optional input (to override coords):
#   data/raw/stop_coords.csv   # columns: location, lat, lon
#
# Outputs:
#   outputs/figs/journey_route.png
#   outputs/figs/stop_key_with_coords.csv

# ---- packages -----------------------------------------------------------

stopifnot(requireNamespace("here",          quietly = TRUE))
stopifnot(requireNamespace("readr",         quietly = TRUE))
stopifnot(requireNamespace("dplyr",         quietly = TRUE))
stopifnot(requireNamespace("tibble",        quietly = TRUE))
stopifnot(requireNamespace("janitor",       quietly = TRUE))
stopifnot(requireNamespace("ggplot2",       quietly = TRUE))
stopifnot(requireNamespace("sf",            quietly = TRUE))
stopifnot(requireNamespace("rnaturalearth", quietly = TRUE))

# ---- load segments-by-location -----------------------------------------

p_locs <- here::here("data/processed/segments_by_location.rds")
stopifnot(file.exists(p_locs))

seg_loc <- readr::read_rds(p_locs)   # has: order, location, text_narr, etc.

# ---- coordinate lookup table -------------------------------------------

# Default coordinates (you can tweak these if you like).
coords_default <- tibble::tibble(
  location = c(
    "London",
    "Suez/Aden",
    "Bombay",
    "Calcutta",
    "Hong Kong",
    "Yokohama/Japan",
    "San Francisco",
    "New York",
    "Atlantic/Liverpool",
    "Transit"               # will be dropped from map
  ),
  lat = c(
    51.5074,   # London
    29.9668,   # Suez (proxy for "Suez/Aden")
    19.0760,   # Mumbai (Bombay)
    22.5726,   # Kolkata (Calcutta)
    22.3193,   # Hong Kong
    35.4437,   # Yokohama
    37.7749,   # San Francisco
    40.7128,   # New York
    53.4084,   # Liverpool (proxy for "Atlantic/Liverpool")
    NA_real_   # Transit (no point shown)
  ),
  lon = c(
    -0.1278,
    32.5498,
    72.8777,
    88.3639,
    114.1694,
    139.6380,
    -122.4194,
    -74.0060,
    -2.9916,
    NA_real_
  )
)

# Optional override via CSV
p_coords_csv <- here::here("data/raw/stop_coords.csv")

coords <- if (file.exists(p_coords_csv)) {
  readr::read_csv(p_coords_csv, show_col_types = FALSE) |>
    janitor::clean_names()
} else {
  coords_default
}

# sanity check
stopifnot(all(c("location", "lat", "lon") %in% names(coords)))

# ---- join coordinates to journey order ---------------------------------

stops <- seg_loc |>
  dplyr::select(order, location) |>
  dplyr::distinct() |>
  dplyr::arrange(order) |>
  dplyr::left_join(coords, by = "location")

missing <- stops |>
  dplyr::filter(is.na(lat) | is.na(lon)) |>
  dplyr::pull(location) |>
  unique()

if (length(missing) > 0) {
  message(
    "No coordinates for: ",
    paste(missing, collapse = ", "),
    ". These will be omitted from the map."
  )
}

stops_map <- stops |>
  dplyr::filter(!is.na(lat) & !is.na(lon))

# ---- build sf objects ---------------------------------------------------

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

pts   <- sf::st_as_sf(stops_map, coords = c("lon", "lat"), crs = 4326)

route <- pts |>
  dplyr::arrange(order) |>
  sf::st_union() |>
  sf::st_cast("MULTIPOINT") |>
  sf::st_cast("LINESTRING")

# ---- plot map -----------------------------------------------------------

# x/y limits tuned to keep the journey centred
xlim <- c(-140, 150)
ylim <- c(-5, 65)

base_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data   = world,
    fill   = "#f3f4f6",
    colour = "white",
    linewidth = 0.2
  ) +
  ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  ggplot2::theme_void(base_size = 11) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = NA)
  )

plt_route <- base_map +
  ggplot2::geom_sf(
    data   = route,
    colour = "#2563eb",
    linewidth = 1.1,
    alpha  = 0.9
  ) +
  ggplot2::geom_sf(
    data   = pts,
    colour = "white",
    fill   = "#ef4444",
    size   = 2.6,
    shape  = 21,
    stroke = 0.3
  ) +
  # order labels (use ggrepel if available)
  {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      ggrepel::geom_text_repel(
        data  = stops_map,
        ggplot2::aes(x = lon, y = lat, label = order),
        size  = 3,
        seed  = 2025,
        point.padding = 0.2,
        max.overlaps  = 100
      )
    } else {
      ggplot2::geom_text(
        data  = stops_map,
        ggplot2::aes(x = lon, y = lat, label = order),
        size  = 3,
        vjust = -1,
        colour = "grey20"
      )
    }
  } +
  ggplot2::labs(
    title    = "Route of Phileas Fogg’s Journey",
    subtitle = "Numbered by location sequence",
    caption  = "Basemap: Natural Earth"
  )

# ---- save outputs -------------------------------------------------------

outdir <- here::here("outputs/figs")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

ggplot2::ggsave(
  file.path(outdir, "journey_route.png"),
  plt_route,
  width  = 9,
  height = 5.2,
  dpi    = 300
)

# also save the stop key (useful for appendix)
readr::write_csv(
  stops,
  here::here("outputs/figs/stop_key_with_coords.csv")
)

# preview in interactive sessions
if (interactive()) print(plt_route)
