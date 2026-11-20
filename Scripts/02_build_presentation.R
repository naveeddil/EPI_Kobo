# Build the PowerPoint decks from the cached data (no app). Run from the project folder:
#   Rscript Scripts/02_build_presentation.R [deck] [region,region] [year,year] [report month]
# deck = dashboard | review (the EPI supervision deck, deck B) | both (default both)
# Dashboard deck: region + year filters (default: current year).
# Review deck: region filter only; report month YYYY-MM (default: last complete month)
# and its year to date.
# e.g. Rscript Scripts/02_build_presentation.R review SOUTH "" 2026-09
source(file.path(Sys.getenv("EPI_PROJECT_ROOT", getwd()), "R", "epi_load.R"))
epi_source_all()
if (file.exists(".Renviron")) readRenviron(".Renviron")

args <- commandArgs(TRUE)
deck <- if (length(args) >= 1 && nzchar(args[1])) tolower(args[1]) else "both"
split_arg <- function(i) if (length(args) >= i && nzchar(args[i])) strsplit(args[i], ",")[[1]] else NULL
regions <- toupper(split_arg(2))
years <- split_arg(3) %||% format(Sys.Date(), "%Y")
ref_month <- if (length(args) >= 4 && nzchar(args[4])) args[4] else NULL

paths <- epi_paths()
d <- epi_load_data(paths)
tabs <- d[c("health", "session", "community")]
dir.create(paths$decks, recursive = TRUE, showWarnings = FALSE)
stamp <- format(Sys.time(), "%Y%m%d_%H%M")
if (deck %in% c("dashboard", "both")) {
  f <- list(regions = regions, years = years)
  out <- file.path(paths$decks, sprintf("Routine_EPI_Dashboard_%s.pptx", stamp))
  epi_build_deck(lapply(tabs, epi_filter, f = f), out, filters = f, as_of = d$meta$fetched_at, paths = paths,
                 progress = message)
  message("Wrote ", out)
}
if (deck %in% c("review", "both")) {
  f <- list(regions = regions)
  out <- file.path(paths$decks, sprintf("EPI_Supervision_Deck_%s.pptx", stamp))
  epi_build_review_deck(lapply(tabs, epi_filter, f = f), out, filters = f, as_of = d$meta$fetched_at, paths = paths,
                        ref_month = ref_month, progress = message)
  message("Wrote ", out)
}
