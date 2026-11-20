# Download the latest EPI_BACKUP export from Kobo and update app_data/kobo_cache.rds
# (same as the "Refresh from Kobo" button). Run from the project folder:
#   Rscript Scripts/01_refresh_from_kobo.R
source(file.path(Sys.getenv("EPI_PROJECT_ROOT", getwd()), "R", "epi_load.R"))
epi_source_all()
if (file.exists(".Renviron")) readRenviron(".Renviron")

message("Downloading ", epi_kobo_url())
d <- epi_refresh_from_kobo()
message(sprintf("Done in %ss: %s rows -> Health center %s, Session %s, Community %s (file %s)",
                d$meta$seconds, format(d$meta$rows, big.mark = ","), nrow(d$health), nrow(d$session),
                nrow(d$community), d$meta$file))
