# epi_load.R --------------------------------------------------------------
# Source every project helper in dependency order. Scripts and app.R call
# epi_source_all() so each entry point is independently runnable.

epi_required_packages <- c("shiny", "bslib", "DT", "plotly", "httr2", "readr", "readxl", "dplyr",
                           "tidyr", "sf", "ggplot2", "ragg", "officer", "mschart", "flextable",
                           "writexl", "commonmark", "zip")

epi_source_all <- function(root = Sys.getenv("EPI_PROJECT_ROOT", getwd())) {
  miss <- epi_required_packages[!vapply(epi_required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("Missing R packages: ", paste(miss, collapse = ", "),
                         "\nInstall with install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))")
  Sys.setenv(EPI_PROJECT_ROOT = normalizePath(root, winslash = "/"))
  for (f in c("epi_config.R", "epi_kobo.R", "epi_indicators.R", "epi_historic.R", "epi_geo.R", "epi_charts.R", "epi_deck.R",
              "epi_deck_review.R", "epi_deck_review_v7.R")) {
    sys.source(file.path(root, "R", f), envir = globalenv())
  }
  invisible(TRUE)
}
