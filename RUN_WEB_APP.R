# Launch the Routine EPI dashboard. Run from the project folder:
#   source("RUN_WEB_APP.R")
local({
  root <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
              nzchar(rstudioapi::getActiveDocumentContext()$path)) {
    dirname(normalizePath(rstudioapi::getActiveDocumentContext()$path))
  } else getwd()
  if (!file.exists(file.path(root, "R", "epi_load.R"))) stop("Run this from the EPI_Kobo project folder.")
  Sys.setenv(EPI_PROJECT_ROOT = normalizePath(root, winslash = "/"))
  port <- as.integer(Sys.getenv("EPI_PORT", "3842"))
  shiny::runApp(root, port = port, launch.browser = interactive())
})
