# R/01_load_text.R
# Download "Around the World in 80 Days" (Gutenberg ID 103) and save raw lines.

# 1) Download the book
## Load the novel Around the world in 80 days from Project Gutenberg
novel <- gutenbergr::gutenberg_download(103, mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")

# 2) Convert to a simple lines table
novel_lines <- tibble::tibble(
  line = seq_along(novel$text),
  text = novel$text
)

# 3) Light check
stopifnot(nrow(novel_lines) > 0)

# 4) Save raw lines for downstream steps
readr::write_rds(novel_lines, here::here("data/processed/novel_lines.rds"))
