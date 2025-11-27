# Data-Science-Research-Project-Part-B
I Will try and comment as much as I can regarding what I have done for the Codes for my research project. To make it easy to read and follow, I will outline it with numbers in numerically order!


# Sentiment Analysis of Narrative Tone Throughout Different Geographic Locations in *Around the World in 80 Days*

This repository contains all code and outputs for my Research Project B (Master of Data Science). The project analyses how the narrator’s tone in Jules Verne’s *Around the World in 80 Days* changes as the story moves across different geographic locations. Everything is written in R and runs as a reproducible, ordered pipeline from raw text to final report figures and tables.

---

## Project Overview

The workflow performs the following:

- Loads and cleans the raw novel text.
- Strips boilerplate and Project Gutenberg artefacts.
- Removes dialogue with robust quote/dash filters.
- Normalises chapter structure and text segmentation.
- Maps each paragraph or segment to a geographic location (“stops”).
- Tokenises and lemmatises using tidytext/tokenizers.
- Loads multiple sentiment lexicons (AFINN, Bing, NRC, SenticNet).
- Applies negation and intensity rules.
- Computes sentiment scores by location.
- Produces plot diagnostics, journey curves, lexicon comparisons, and geographic maps.
- Generates final figures and tables for use in the written report.
- Includes optional modelling, partial correlations, and lexicon coverage diagnostics.

All paths use the `here` package for reproducibility.

---

## Folder Structure

├── data/
│ ├── raw/ # Raw text and any lookup files
│ ├── external
│ └── processed/ # Output .rds/.csv files generated during the pipeline
│
├── outputs/
│ ├── figs/ # Final PNG/JPEG visualisations
│ ├── meta
│ └── tables/ # Exported CSV/LaTeX tables
│
├── scripts/ # Main R pipeline (00–20)
│
└── README.md


---

## How to Run the Project

1. Clone the repository:

   ```bash
   git clone https://github.com/YuthYuthYuthYuth/Data-Science-Research-Project-Part-B.git
   cd Data-Science-Research-Project-Part-B
2. Open the project folder in RStudio.
3. Run the scripts in numeric order from 00_packages.R through 20_senticnet_examples.R.
4. Outputs will be created automatically in:
   - data/processed/
   - outputs/figures/
   - outputs/tables/
If you need only certain plots/tables, you can run scripts from the relevant point as long as required intermediate files already exist.

## Script-by-Script Guide

All scripts are inside scripts/.

00_packages.R

Loads/installs packages, sets global options, ensures the folder structure exists.

01_load_text.R

Loads the raw novel text into memory (from data/raw/).

02_strip_boiler_segment.R

Removes Gutenberg boilerplate, licensing text, headers/footers, and other unwanted metadata.

03_normalise_text.R

Normalises text structure, fixes spacing, corrects irregularities, prepares clean paragraph/segment units.

04_dialogue_variants.R

Applies multiple dialogue filters (quotes, em-dash dialogue markers, constructed variants) to extract only narrator text.

05_location_map.R

Defines the geographic lookup table and maps text segments to story “stops”.

06_tokenise_lemmatise.R

Tokenises narrator text into words and performs lemmatisation.

07_lexicons.R

Loads sentiment lexicons (AFINN, Bing, NRC, SenticNet if applicable) and prepares them for scoring.

08_negation_intensity.R

Implements contextual sentiment modifications such as:

Negation handling

Intensifiers

Down-toners

09_scoring.R

Combines tokens + lexicons + context rules to compute sentiment scores at token, paragraph, and stop levels.

10_diagnostics.R

Generates diagnostic outputs, sanity checks, sample excerpts, and intermediate summaries.

11_plot_journey.R

Creates the main story “journey curve” — sentiment across the novel’s geographic route.

11b_plot_lexicon_compare.R

Compares sentiment across lexicons (AFINN vs Bing vs NRC vs SenticNet), including overlays and facets.

12_geo_map.R

Plots the world map with the novel’s travel route and sentiment by location.

12B_geo_map.R

Additional or alternative geographic visualisation (e.g., per-stop markers or sentiment heat mapping).

13_report_tables.R

Creates summary tables used directly in the written report (exports CSV/LaTeX).

14_plots_export.R

Collects and saves all major plots in final form into outputs/figures/.

14_tables.R

Exports final table versions for report inclusion.

15_plot_intensity_proxy.R

Creates plots showing sentiment intensity proxies, scaled scores, or alternative sentiment visualisations.

16_modelling.R

Optional modelling (e.g., linear models, smoothed fits, exploratory regression on predictors).

17_partial_correlations.R

Computes partial correlations between sentiment, intensity, lexicon variants, and other variables.

18_prepare_senticnet.R

Prepares SenticNet lexicon data for use in scoring (formatting, joining, normalisation).

19_lexicon_coverage.R

Generates diagnostics showing lexicon coverage — proportion of tokens each lexicon can score.

20_senticnet_examples.R

Produces illustrative examples of SenticNet-based sentiment scoring for inclusion in the report/appendix.

## Reproducibility

The project:
- Uses an ordered script pipeline (00 → 20).
- Stores intermediate files to avoid re-running early stages unnecessarily.
- Uses the here package for stable paths.
- Produces deterministic outputs given identical inputs.
- Anyone can reproduce the full analysis by running the scripts in order.

For issues or questions, please use the GitHub Issues tab. Thank you :)

