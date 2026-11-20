# epi_config.R ------------------------------------------------------------
# Paths, Kobo source, colours. Environment variables (project .Renviron, see
# .Renviron.example) override the defaults.

epi_root <- function() normalizePath(Sys.getenv("EPI_PROJECT_ROOT", getwd()), winslash = "/", mustWork = FALSE)

epi_paths <- function(root = epi_root()) {
  list(
    root      = root,
    cache     = file.path(root, "app_data", "kobo_cache.rds"),   # prepared tables + refresh info
    downloads = file.path(root, "archive", "kobo_downloads"),    # last raw Kobo exports (kept 5)
    seed_xlsx = file.path(root, "data", "EPI_Kobo_Data.xlsx"),   # snapshot used until the first refresh
    historic  = file.path(root, "data", "Historic"),             # old files added to every load (R/epi_historic.R)
    historic_cache = file.path(root, "app_data", "historic_raw.rds"),
    geography = file.path(root, "reference", "geography", "afg_geography.rds"),
    template  = file.path(root, "reference", "epi_template.pptx"),
    decks     = file.path(root, "outputs", "presentations"),
    changelog = file.path(root, "CHANGELOG.md"),
    version   = file.path(root, "VERSION")
  )
}

epi_version <- function(root = epi_root()) {
  f <- file.path(root, "VERSION")
  if (file.exists(f)) trimws(readLines(f, warn = FALSE)[1]) else "0.0.0"
}

# Kobo export setting "EPI_BACKUP" of the Routine EPI checklist form. The form
# data is shared publicly, so no credentials are needed. KOBO_TOKEN (API token)
# is optional, for the case the sharing is switched off later.
epi_kobo_url <- function() {
  Sys.getenv("KOBO_EXPORT_URL",
             "https://eu.kobotoolbox.org/api/v2/assets/a5aJnksV6mK3Fgese2DPMy/export-settings/esKvGvapbsM4y4zohJ2AuRL/data.csv")
}

# Power BI "StartDate" parameter: visits before this date are not shown. Was
# 2025-01-01; 2020-01-01 since v0.3.0, 2019-01-01 since v0.4.0 so the historic
# files (data/Historic, from 2019) show.
epi_start_date <- function() as.Date(Sys.getenv("EPI_START_DATE", "2019-01-01"))

epi_keep_downloads <- 5L

# ---- Colours ----------------------------------------------------------------
# Yes / No = first two slots of a colour-blind-checked categorical palette.
# Coverage markers use the same palette in fixed order (colour follows marker).
epi_colors <- list(
  yes = "#2a78d6",
  no  = "#eb6834",
  series = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948"),
  bar_single   = "#0072BC",
  covered      = "#86b6ef",
  not_covered  = "#FAFAFA",
  district_ln  = "#C8C8C8",
  province_ln  = "#9A9A9A",
  region_ln    = "#4D4D4D",
  region_label = "#0d366b",
  ink   = "#17324d",
  muted = "#64748b"
)

epi_font <- "Segoe UI"

# Text colour that stays readable on each fill (labels inside bars)
epi_text_on <- function(fill) {
  rgb <- grDevices::col2rgb(fill)
  lum <- (0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) / 255
  ifelse(lum < 0.6, "#FFFFFF", "#333333")
}
