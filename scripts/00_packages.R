# R/00_packages.R
# Simple setup: install/load packages, set options, theme, and seed.

# 1) Install pacman (if missing) and load everything in one go
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  # Core wrangling + I/O
  tidyverse, janitor, here, slider,
  # Text / sentiment foundations
  tidytext, textclean, SnowballC,
  # Plotting
  ggtext, patchwork, cowplot, ggridges, scales,
  # Tables
  gt, gtsummary,
  # Document/render helpers
  knitr,
  ggplot2,
  # Used in the next script to grab the book
  gutenbergr,
  # For plot-intensity proxy
  udpipe,
  lexicon
)


# 2) Reproducibility + friendlier defaults
set.seed(2025)
options(
  stringsAsFactors = FALSE,
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  tibble.print_max = 50,
  readr.show_col_types = FALSE
)

# 3) Plot theme (clean, minimal)
ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title.position = "plot",
      axis.title = ggplot2::element_text(),
      panel.grid.minor = ggplot2::element_blank()
    )
)

# 4) Knitr defaults (safe to set even outside Quarto)
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE,
  fig.align = "center", fig.width = 8, fig.height = 5, dpi = 300
)

# 5) Project root (works best inside an .Rproj)
# here::i_am(".")   # <- not needed now; commented out for Project B
message("Loaded 00_packages.R ✔  Seed = 2025")

