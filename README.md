# Data-Science-Research-Project-Part-B
I Will try and comment as much as I can regarding what I have done for the Codes for my research project A. To make it easy to read and follow, I will outline it with numbers in numerically order!

1) What this project does
This repository provides a completely replicable R pipeline to examine if the narrator's tone in Jules Verne's Around the World in 80 Days fluctuates with location. We acquire the public-domain text from Project Gutenberg, normalise and de-paratext the novel, eliminate dialogue using quote-aware filters to preserve the narrator's voice, segment the narrative into 19 geographic locations, tokenize, evaluate tokens with three sentiment lexicons (AFINN, Bing, NRC) employing minimal contextual rules (negation/intensifiers/downtoners), summarise by location with bootstrap confidence intervals and an LOESS smooth, and produce the figures and tables utilised in the paper.

2) Quick start
Open the project in RStudio (recommended).

Run scripts in numeric order from 00_...R through 14_...R.
Each script writes its results to data/processed and/or outputs/*, so you can resume from any point.

Outputs will appear in:

outputs/figures/ (PNG figures for the paper)

outputs/tables/ (CSV and LaTeX table snippets)

data/processed/ (.rds intermediates used by later steps)

Paths are relative via here::here(); you can run from the project root regardless of OS.

3) Software & packages
R (≥ 4.2 recommended)

CRAN packages: tidyverse, here, readtext or readr, stringr, tidytext, textdata, tokenizers, dplyr, purrr, ggplot2, scales, forcats, broom, modelr, cowplot, ggtext

For the route map: sf, rnaturalearth, rnaturalearthdata, geosphere (or ggplot2::map_data fallback)

4) Repository layout
/data
  /raw           <- original inputs (downloaded Gutenberg text, etc.)
  /processed     <- intermediate .rds objects (token lists, scores, summaries)
  
/outputs
  /figures       <- final PNGs used in the paper
  /tables        <- final CSVs and LaTeX table snippets

/scripts (if you keep them here) or project root
  00_setup.R
  01_download_text.R
  02_normalise_paratext.R
  03_quote_filters.R
  04_segment_stops_and_route_map.R
  05_tokenise.R
  06_join_lexicons.R
  07_context_rules.R
  08_aggregate_by_stop.R
  09_bootstrap_and_loess.R
  10_fig_journey_curve.R
  11_fig_bars_with_CIs.R
  12_fig_lexicon_facets.R
  13_fig_lexicon_overlay.R
  14_tables.R

  5) Script-by-script guide (00–14)
00_setup.R

Installs/loads packages, sets global options, seeds resampling for reproducibility.

Creates data/raw, data/processed, outputs/figures, outputs/tables if missing.

01_download_text.R

Downloads Around the World in 80 Days from Project Gutenberg to data/raw/.
