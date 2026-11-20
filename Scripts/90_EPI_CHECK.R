# Core regression check: data preparation, indicator maths, map, deck build and
# app server. Uses the cached data (or the Excel snapshot) and writes only to a
# temp folder. Run from the project folder:
#   Rscript Scripts/90_EPI_CHECK.R
source(file.path(Sys.getenv("EPI_PROJECT_ROOT", getwd()), "R", "epi_load.R"))
epi_source_all()

results <- data.frame(check = character(), status = character(), detail = character())
check <- function(name, expr) {
  r <- tryCatch({ v <- expr; if (isTRUE(v)) list("PASS", "") else list("FAIL", paste(v, collapse = " ")) },
                error = function(e) list("FAIL", conditionMessage(e)))
  results[nrow(results) + 1, ] <<- c(name, r[[1]], r[[2]])
}

# 1. Yes/No maths on a tiny known table
toy <- data.frame(Region = c("A", "A", "A", "B", "B"), x = c("Yes", "No", NA, "Yes", "Yes"))
check("Yes/No share ignores blanks", {
  t <- epi_yn_by(toy, "x", "Region"); o <- epi_yn_overall(toy, "x")
  isTRUE(all.equal(t$pct, c(0.5, 1))) && o$Total == 4 && isTRUE(all.equal(o$pct, 0.75))
})
check("Date parsing (ISO + Excel serial)", {
  identical(epi_parse_date(c("2025-03-01", "45717", NA)), as.Date(c("2025-03-01", "2025-03-01", NA)))
})
check("Geography level: auto switches to province for one region", {
  epi_geo_level("Auto", "SOUTH") == "Province" && epi_geo_level("Auto", c("SOUTH", "WEST")) == "Region" &&
    epi_geo_level("Region", "SOUTH") == "Region"
})
check("Marker colours follow the marker, not its rank", {
  a <- epi_marker_colors(c("BCG", "Penta-3")); b <- epi_marker_colors(c("Penta-3", "BCG", "Measles-2"))
  identical(a[["Penta-3"]], b[["Penta-3"]]) && identical(a[["BCG"]], b[["BCG"]]) && !anyNA(b)
})

# 2. Real data
d <- epi_load_data()
check("Cached data has the three tables", all(c("health", "session", "community") %in% names(d)) && nrow(d$health) > 0)
check("All indicator columns found in the export", {
  miss <- c(epi_health_indicators$key[vapply(epi_health_indicators$key, function(k) all(is.na(d$health[[k]])), logical(1))],
            epi_session_indicators$key[vapply(epi_session_indicators$key, function(k) all(is.na(d$session[[k]])), logical(1))])
  if (length(miss)) paste("empty:", paste(miss, collapse = ", ")) else TRUE
})
check("District codes join to the boundary file", {
  u <- epi_unmatched_codes(c(d$health$DCode, d$session$DCode, d$community$DCode))
  if (length(u)) paste("unmatched:", paste(u, collapse = ", ")) else TRUE
})
check("Historic decoding: 1 = Yes, 2 = No, other = blank; Day/Mon/Year dates", {
  identical(.epi_yn12(c("1", "2", "0", NA)), c("Yes", "No", NA, NA)) &&
    identical(.epi_dmy(c("10", "31"), c("Jan", "2"), c("2022", "2023")), c("2022-01-10", NA))
})
check("Historic rows loaded and flagged (2020-2023, Source column)", {
  old <-d$health[d$health$Source != "Kobo", ]
  nrow(old) > 0 && all(grepl("^Historic", old$Source)) && all(d$health$Source[which(d$health$Year >= 2025)] == "Kobo") &&
    any(d$community$Source != "Kobo") && epi_historic_count(d) > 0
})
check("Every historic file is read; old Session sheets give session answers", {
  h <- d$historic
  hs <- d$session[d$session$Source != "Kobo", ]
  nrow(h) == length(.epi_historic_files()) && all(h$Read > 0) && any(!is.na(hs$vvm)) && any(!is.na(hs$msg_importance)) &&
    identical(d$historic_sig, epi_historic_signature())
})
check("No record twice (same _id within a table)", {
  all(vapply(d[c("health", "session", "community")], function(x) !anyDuplicated(x$kobo_id[!is.na(x$kobo_id)]), logical(1)))
})
check("Historic Kobo file (XML names) gives the same values as the live export for the same _id", {
  h <- epi_historic_raw()
  h <- h[grepl("KOBO", h$.source), , drop = FALSE]
  if (!nrow(h)) return(TRUE)
  p <- epi_prepare(h)
  bad <- character()
  for (tb in c("health", "session", "community")) {
    a <- p[[tb]]; b <- d[[tb]][match(a$kobo_id, d[[tb]]$kobo_id), ]
    keep <- !is.na(b$kobo_id); a <- a[keep, ]; b <- b[keep, ]
    # Historical extracts pre-date the expanded v3 indicator specification;
    # compare only fields that the historic decoder is designed to reproduce.
    v3_only <- c("catchment_map", "target_population", "temperature_ok", "fridge_tag", "hc_tally", "hc_cards",
      "hc_register", "hc_monthly_report", "penta3_calc", "dropout_calc", "defaulter_followup",
      "pemt_mobile_supervision", "ngo_mobile_supervision", "outreach_planned", "outreach_conducted",
      "mobile_planned", "mobile_conducted", "session_trained", "contact_person", "duration_3h", "supplies",
      "expired", "open_vial", "session_register", "aefi_aware", "next_contact", "card_retention", "caregiver_feedback")
    cols <- setdiff(names(a), c("Source", "Facility", "Supervisor", "Monitor", "NGO", "SessionType", "FacilityType", v3_only))
    for (cl in cols) if (!identical(as.character(a[[cl]]), as.character(b[[cl]]))) bad <- c(bad, paste(tb, cl))
  }
  if (length(bad)) paste("differ:", paste(bad, collapse = ", ")) else TRUE
})
f <- list(regions = "SOUTH")
data <- lapply(d[c("health", "session", "community")], epi_filter, f = f)
check("Region filter keeps only that region", all(data$health$Region == "SOUTH"))
check("Coverage between 0 and 150%", { cv <- epi_coverage(data$community); all(cv$pct >= 0 & cv$pct <= 1.5) })

# 3. Map and deck
tmp <- tempfile("epi_check_"); dir.create(tmp)
check("Map renders", file.exists(epi_save_map(epi_map(epi_visits_by_district(data$health), focus_regions = "SOUTH"),
                                              file.path(tmp, "map.png"), 6, 4.5, dpi = 72)))
check("Deck builds (all pages, SOUTH)", {
  out <- file.path(tmp, "deck.pptx")
  epi_build_deck(data, out, filters = f, as_of = d$meta$fetched_at)
  n <- length(officer::read_pptx(out))
  if (n >= 20) TRUE else paste("only", n, "slides")
})
check("Dashboard deck has no speaker notes", {
  !any(grepl("notesSlides/notesSlide", utils::unzip(file.path(tmp, "deck.pptx"), list = TRUE)$Name))
})
check("Deck builds with no data", {
  empty <- lapply(data, function(x) x[0, ])
  out <- file.path(tmp, "empty.pptx"); epi_build_deck(empty, out); file.exists(out)
})
check("Naveed Saeed EPI deck v7 builds (SOUTH): slides = manifest, 19-21 content slides + 4 dividers", {
  out <- file.path(tmp, "review.pptx")
  epi_build_review_deck(data, out, filters = f, as_of = d$meta$fetched_at)
  n <- length(officer::read_pptx(out)); m <- nrow(epi_review_manifest())
  nd <- sum(epi_review_manifest()$module == ".rv_m_divider")
  if (n == m && nd == 4 && (n - nd) >= 19 && (n - nd) <= 21) TRUE else paste(n, "slides, manifest", m)
})
check("Naveed Saeed EPI deck v7: shared safety / completion helpers match the v6 formulas", {
  s <- data$session[format(data$session$VisitDate, "%Y-%m") == "2026-10", ]
  sf <- .rv_safety(s); z <- .rv_safety_province(s)
  bad <- sapply(c("expired", "vvm", "icepacks", "safety_box", "technique", "open_vial"),
                function(k) if (k %in% c("expired", "vvm")) s[[k]] == "Yes" else s[[k]] == "No")
  h <- data$health; o <- .rv_completion(h, "outreach"); h <- h[!duplicated(paste(h$DCode, h$Facility, h$Year, h$Month)), ]
  ok <- !is.na(h$outreach_planned) & !is.na(h$outreach_conducted) & h$outreach_planned > 0
  pr <- .rv_completion_by(data$health, "outreach", "Province")
  sf$n == sum(rowSums(bad, na.rm = TRUE) > 0 & rowSums(!is.na(bad)) > 0) && sum(z$n) == sf$n &&
    isTRUE(all.equal(o$rate, sum(h$outreach_conducted[ok]) / sum(h$outreach_planned[ok]))) && sum(pr$planned) == o$planned
})
check("Report month = last complete month", {
  epi_ref_month(d[c("health", "session", "community")], today = as.Date("2026-11-18")) == "2026-10" &&
    identical(epi_ytd_months("2026-03"), c("2026-01", "2026-02", "2026-03"))
})
check("Compliance by region: denominator = provinces in the region, total row adds up", {
  t <- epi_compliance_by_region(lapply(d[c("health", "session", "community")], epi_filter), "2026-10")
  tot <- t[t$Region == "Total", ]
  tot$Provinces == nrow(epi_province_scope()) && tot$`Health center n` == sum(t$`Health center n`[t$Region != "Total"]) &&
    isTRUE(all.equal(tot$Session, tot$`Session n` / tot$Provinces))
})
check("Data page short headers are short", {
  x <- epi_export_table(d$health[1:3, ], "health", short = TRUE)
  max(nchar(names(x))) <= 25
})
check("Compliance grid: one row per province in scope, counts add up", {
  m <- epi_ytd_months("2026-10")
  g <- epi_compliance_grid(lapply(d[c("health", "session", "community")], epi_filter), m, regions = "SOUTH")
  s <- epi_subset_months(lapply(d[c("health", "session", "community")], epi_filter, f = list(regions = "SOUTH")), m)
  nrow(g) == nrow(epi_province_scope("SOUTH")) && sum(g$Total) == sum(vapply(s, nrow, integer(1)))
})
check("Naveed Saeed EPI deck has no speaker notes", {
  z <- utils::unzip(file.path(tmp, "review.pptx"), list = TRUE)$Name
  !any(grepl("notesSlides/notesSlide", z))
})
check("Historic rows: a visit already in Kobo or in an earlier file is not added twice", {
  h <- d$historic
  all(h$Read == h$InKobo + h$InEarlier + h$Added) &&
    !anyDuplicated(.epi_visit_key(data.frame(`Select Checklist is to be filled:` = "Health Center",
                                             `Date of Visit` = c("2024-01-05", "2024-01-06"), check.names = FALSE)))
})
check("District reporting: scope = boundary districts, counts add up", {
  m <- epi_ytd_months("2026-10")
  t <- epi_district_region_month(data, m, regions = "SOUTH")
  s <- epi_silent_districts(data, "2026-10", m, regions = "SOUTH", all = TRUE)
  tot <- t[t$Region == "Total", ]
  tot$Districts == nrow(epi_district_scope("SOUTH")) && all(t$`Any month` <= t$Districts) &&
    (is.null(s) || all(s$Reporting + s$`Not reporting` == s$Districts))
})
check("Compliance by region (districts): any checklist = region x month count, types <= any", {
  c1 <- epi_district_compliance_by_region(data, "2026-10", regions = "SOUTH")
  t <- epi_district_region_month(data, "2026-10", regions = "SOUTH")
  tot <- c1[c1$Region == "Total", ]
  tot$`Any checklist` == t$`Any month`[t$Region == "Total"] &&
    all(vapply(unname(epi_pages), function(p) all(c1[[p]] <= c1$`Any checklist`), logical(1)))
})
check("Management review deck builds with no data", {
  empty <- lapply(data, function(x) x[0, ])
  out <- file.path(tmp, "review_empty.pptx"); epi_build_review_deck(empty, out); file.exists(out)
})
check("Region x month table totals match the record count", {
  t <- epi_region_month_table(data)
  t$Total[t$Region == "Total"] == sum(vapply(data, function(x) sum(!is.na(x$Region)), integer(1)))
})

# 4. App server
check("App server starts and renders", {
  Sys.setenv(EPI_PROJECT_ROOT = epi_root())
  app <- shiny::shinyAppFile(file.path(epi_root(), "app.R"))
  ok <- TRUE
  shiny::testServer(app, {
    session$setInputs(f_region = "SOUTH", f_province = NULL, f_year = NULL, f_month = NULL, f_geo = "Auto",
                      c_markers = epi_default_markers, data_page = "health", ppt_pages = names(epi_pages))
    session$elapse(500)
    stopifnot(nrow(fdata()$health) == nrow(data$health), geo_level() == "Province")
    output$h_metrics; output$h_microplan_bar; output$c_overall; output$c_table; output$data_tbl
    session$setInputs(home_pages = "all", comp_pages = "all", f_year = format(Sys.Date(), "%Y"))
    session$elapse(500)
    output$home_metrics; output$home_month; output$home_region_month; output$home_region_type; output$home_region_lines
    output$home_last_metrics; output$home_last_compliance; output$home_comp_trend; output$home_comp_grid
    stopifnot(all(fdata()$health$Year == as.integer(format(Sys.Date(), "%Y"))))
    session$setInputs(f_year = c("2025", "2026")); session$elapse(500); output$c_overall
  })
  ok
})

print(results, right = FALSE)
cat(sprintf("\n%d passed, %d failed\n", sum(results$status == "PASS"), sum(results$status == "FAIL")))
unlink(tmp, recursive = TRUE)
if (any(results$status == "FAIL")) quit(status = 1)
