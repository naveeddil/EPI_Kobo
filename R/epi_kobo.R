# epi_kobo.R --------------------------------------------------------------
# Download the Kobo export, read it, prepare the three tables and cache them.
#
# Kobo's synchronous export URL (".../export-settings/<uid>/data.csv") answers
# 503 + Retry-After while it builds the file, then redirects (302) to the
# finished export. httr2 retries on 503 using Retry-After and follows the
# redirect. The export is shared publicly, so no login is needed.

epi_download_kobo <- function(url = epi_kobo_url(), paths = epi_paths(), max_wait = 900) {
  dir.create(paths$downloads, recursive = TRUE, showWarnings = FALSE)
  ext <- if (grepl("[.]xlsx$", url, ignore.case = TRUE)) "xlsx" else "csv"
  out <- file.path(paths$downloads, sprintf("EPI_BACKUP_%s.%s", format(Sys.time(), "%Y%m%d_%H%M%S"), ext))
  part <- paste0(out, ".part")

  req <- httr2::request(url) |>
    httr2::req_user_agent("EPI-Kobo-Shiny (R httr2)") |>
    httr2::req_timeout(max_wait) |>
    httr2::req_retry(
      max_tries = 20, max_seconds = max_wait,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 502L, 503L, 504L),
      after = function(resp) {
        ra <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After")))
        if (length(ra) != 1 || is.na(ra)) 15 else min(max(ra, 5), 120)
      }
    )
  tok <- Sys.getenv("KOBO_TOKEN")
  if (nzchar(tok)) req <- httr2::req_headers(req, Authorization = paste("Token", tok), .redact = "Authorization")

  resp <- httr2::req_perform(req, path = part)
  type <- httr2::resp_content_type(resp)
  if (grepl("html|json", type)) {
    unlink(part)
    stop("Kobo did not return a data file (content type ", type, "). Check that the form data is still shared ",
         "publicly, or set KOBO_TOKEN in .Renviron.")
  }
  file.rename(part, out)
  epi_prune_downloads(paths)
  out
}

epi_prune_downloads <- function(paths = epi_paths(), keep = epi_keep_downloads) {
  f <- list.files(paths$downloads, pattern = "^EPI_BACKUP_.*[.](csv|xlsx)$", full.names = TRUE)
  f <- f[order(file.mtime(f), decreasing = TRUE)]
  if (length(f) > keep) unlink(f[-seq_len(keep)])
  unlink(list.files(paths$downloads, pattern = "[.]part$", full.names = TRUE))
  invisible(TRUE)
}

# Every column is read as text; the preparation step converts types. Duplicate
# headers (Kobo repeats "Name and Quantiry of the vaccine ?") get a _1 suffix,
# as in the old Power Query.
epi_read_export <- function(file) {
  if (grepl("[.]xlsx$", file, ignore.case = TRUE)) {
    tmp <- tempfile(fileext = ".xlsx"); on.exit(unlink(tmp))
    file.copy(file, tmp)              # a copy can be read while the file is open in Excel
    sh <- readxl::excel_sheets(tmp)
    x <- readxl::read_excel(tmp, sheet = sh[1], col_types = "text", .name_repair = "minimal")
  } else {
    first <- readLines(file, n = 1, warn = FALSE, encoding = "UTF-8")
    n_semi <- lengths(regmatches(first, gregexpr(";", first, fixed = TRUE)))
    n_comma <- lengths(regmatches(first, gregexpr(",", first, fixed = TRUE)))
    x <- readr::read_delim(file, delim = if (n_semi > n_comma) ";" else ",",
                           col_types = readr::cols(.default = readr::col_character()),
                           na = c("", "NA"), name_repair = "minimal", progress = FALSE,
                           locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE)
  }
  names(x) <- make.unique(trimws(names(x)), sep = "_")
  x <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  if (!"Select Checklist is to be filled:" %in% names(x))
    stop("This file does not look like the EPI_BACKUP export: column 'Select Checklist is to be filled:' is missing.")
  x
}

# Download -> read -> prepare -> cache. Returns the prepared data list.
epi_refresh_from_kobo <- function(paths = epi_paths(), url = epi_kobo_url()) {
  t0 <- Sys.time()
  file <- epi_download_kobo(url, paths)
  raw <- epi_read_export(file)
  data <- epi_prepare_with_history(raw, paths)
  data$meta <- list(source = "Kobo API (export EPI_BACKUP)", fetched_at = Sys.time(),
                    file = basename(file), rows = nrow(raw),
                    seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  epi_save_cache(data, paths)
  data
}

epi_save_cache <- function(data, paths = epi_paths()) {
  dir.create(dirname(paths$cache), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(paths$cache, ".tmp")
  saveRDS(data, tmp)
  file.rename(tmp, paths$cache)
  invisible(paths$cache)
}

# What the app shows on start: the last refresh; if that cache is from an older
# version, it is rebuilt from the last raw download in archive/; otherwise the
# Excel snapshot copied from the old Power BI folder (until "Refresh from Kobo").
# Historic files (data/Historic) are added in every case.
epi_load_data <- function(paths = epi_paths()) {
  if (file.exists(paths$cache)) {
    d <- tryCatch(readRDS(paths$cache), error = function(e) NULL)
    historic_files <- .epi_historic_files(paths)
    if (!is.null(d) && identical(d$schema, epi_schema_version) &&
        (!length(historic_files) || identical(d$historic_sig, epi_historic_signature(paths)))) return(d)
  }
  dl <- list.files(paths$downloads, pattern = "^EPI_BACKUP_.*[.](csv|xlsx)$", full.names = TRUE)
  if (length(dl)) {
    file <- dl[which.max(file.mtime(dl))]
    raw <- epi_read_export(file)
    d <- epi_prepare_with_history(raw, paths)
    d$meta <- list(source = "Kobo API (export EPI_BACKUP)", fetched_at = file.mtime(file), file = basename(file),
                   rows = nrow(raw), seconds = NA)
    epi_save_cache(d, paths)
    return(d)
  }
  if (file.exists(paths$seed_xlsx)) {
    raw <- epi_read_export(paths$seed_xlsx)
    d <- epi_prepare_with_history(raw, paths)
    d$meta <- list(source = "Snapshot data/EPI_Kobo_Data.xlsx (press Refresh for live Kobo data)",
                   fetched_at = file.mtime(paths$seed_xlsx), file = basename(paths$seed_xlsx),
                   rows = nrow(raw), seconds = NA)
    epi_save_cache(d, paths)
    return(d)
  }
  d <- epi_prepare_with_history(NULL, paths)
  d$meta <- list(source = "No data yet - press Refresh from Kobo", fetched_at = NA, file = NA, rows = 0, seconds = NA)
  d
}
