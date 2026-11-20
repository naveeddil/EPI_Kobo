# epi_historic.R ----------------------------------------------------------
# Historic data kept outside today's Kobo export (folder data/Historic/). Each
# file is turned into rows that look like the live EPI_BACKUP export (same
# column labels), so epi_prepare() treats them like Kobo rows. Every row gets
# a Source label ("Historic (<file>)"); live Kobo rows are "Kobo".
#
# Two file layouts are recognised (by content, not by file name):
#   1. Kobo export with XML column names (sheet with "select_Checklist"),
#      e.g. "Routine EPI Dashboard _ KOBO.xlsx" (Jul 2024 - 2026).
#   2. Old Excel dashboards: every sheet named Health* / *Community* / *Session*
#      is read, columns matched on header text, answers coded 1 = Yes, 2 = No.
#      e.g. "Dashboard of Routine EPI 2022-2023.xlsx" (East, 2020 - 2023; Health
#      = combined facility + session checklist, no Session sheet) and
#      "1_Power BI  EPI 2023.xlsx" (all regions; Session Oct 2023, Health and
#      Community Jan 2024).
#
# Duplicates: by _id within a file (row number when a sheet has no _id); a
# historic row whose _id is in the live export is dropped (live wins). Rows of
# an old dashboard identical in every field except the id are double entries.
# Files can repeat each other's rows under other ids (e.g. "RI Data 2019-2026"
# holds the 2020-23 East file and the 2023/24 file, ids made up), so rows are
# also matched on the visit itself (.epi_visit_key: checklist, date, district,
# facility, answers): a row already in the live export, or in a file read
# earlier (file-name order), is not added again.

epi_historic_version <- "4"   # bump when the readers change (rebuilds app_data/historic_raw.rds)

.epi_hist_cols <- c(checklist = "Select Checklist is to be filled:", region = "Region", province = "Province",
                    district = "District", dcode = "District Code", ngo = "Name of BPHS NGO",
                    facility = "Name of Health Facility/ Nearest Facility Name", ftype = "Type of Health Facility",
                    stype = "Session Type", supervisor = "Name & Title of Supervisor", monitor = "Name of Monitor",
                    date = "Date of Visit", id = "_id")

# ---- 1. Kobo export with XML names -------------------------------------------------
# XML name -> export label, for the columns the dashboards use.
.epi_kobo_xml_map <- function() {
  c(stats::setNames(unname(.epi_hist_cols),
                    c("select_Checklist", "Region", "Province", "District", "DCode", "Name_of_BPHS_NGO",
                      "Name_of_Health_Facility", "Type_of_Health_Facility", "sessiontype",
                      "Name_Title_of_Supervisor", "MonitorName", "VisitDate", "_id")),
    stats::setNames(epi_health_indicators$column,
                    c("Is_there_up_to_date_outreach_and_mobile", "Are_all_the_outreach_he_distance_of_5_km",
                      "Is_there_updated_EPI_ing_chart_on_display", "Has_vaccinator_been_r_the_last_12_months",
                      "Are_there_linkages_with_community", "Are_children_and_wom_Ws_and_practitioners",
                      "Is_cold_chain_equipm_ilable_at_the_center", "Is_the_outreach_supe_ogbook_register_book",
                      "Is_the_outreach_supe_ogbook_register_book_001")),
    stats::setNames(epi_session_indicators$column,
                    c("Are_children_and_wom_referred_to_session", "Did_the_team_select_mmended_by_community",
                      "Does_the_team_have_a_conditioned_icepacks", "Are_there_any_vaccin_rd_and_fourth_stages",
                      "Does_the_EPI_Health_EHW_use_safety_box", "Does_the_EHW_follow_nistration_technique",
                      "Does_the_EHW_fill_tally_sheet_correctly", "Importance_of_vaccines",
                      "Vaccine_probable_reactions", "Does_the_EHW_fill_im_tion_cards_correctly")),
    stats::setNames(epi_children_col, "sumchildrencheck"),
    stats::setNames(epi_markers$column,
                    c("svaccinationcardavailable", "sbcg", "sopv_0", "sopv_1", "spenta_1", "spcv_1", "sopv_3",
                      "sPenta_3", "spcv_3", "srota_1", "srota_2", "smeasles_1", "smeasles_2")),
    stats::setNames(epi_reasons$column,
                    paste0("SCommunity_Miss_Reason/", c("awareness", "belief", "refusalAEFI", "pardah", "sessionfar",
                                                        "timinginconvenient", "femaleNA", "security"))))
}

.epi_read_hist_kobo <- function(tmp, sheet) {
  x <- readxl::read_excel(tmp, sheet = sheet, col_types = "text", .name_repair = "minimal")
  x <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  map <- .epi_kobo_xml_map()
  miss <- setdiff(names(map), names(x))
  if (length(miss)) warning("Historic Kobo file: columns not found: ", paste(miss, collapse = ", "))
  out <- x[intersect(names(map), names(x))]
  names(out) <- unname(map[names(out)])
  ck <- c(healthcenter = "Health Center", session = "Session", communitysum = "Community",
          ppoesreport = "WHO Staff Monthly reports")
  k <- ck[tolower(out[[.epi_hist_cols[["checklist"]]]])]
  out[[.epi_hist_cols[["checklist"]]]] <- ifelse(is.na(k), out[[.epi_hist_cols[["checklist"]]]], k)
  st <- c(outreach = "Outreach", mobile = "Mobile", fixed = "Fixed", nosession = "No Session")
  s <- st[tolower(out[[.epi_hist_cols[["stype"]]]])]
  out[[.epi_hist_cols[["stype"]]]] <- ifelse(is.na(s), out[[.epi_hist_cols[["stype"]]]], s)
  out[!duplicated(out[["_id"]]) | is.na(out[["_id"]]), , drop = FALSE]
}

# ---- 2. Old Excel dashboard (2020-2023) ----------------------------------------------
.epi_norm <- function(x) trimws(gsub("[^a-z0-9<]+", " ", tolower(x)))

# First column whose normalised header matches the pattern (answers come before
# the "Formulas" Yes/No helper columns, so the first match is the answer).
.epi_hcol <- function(x, pattern) {
  j <- which(grepl(pattern, .epi_norm(names(x))))
  if (!length(j)) return(rep(NA_character_, nrow(x)))
  x[[j[1]]]
}

.epi_yn12 <- function(v) { v <- trimws(v); ifelse(v == "1", "Yes", ifelse(v == "2", "No", NA_character_)) }

.epi_dmy <- function(day, month, year) {
  m <- trimws(month)
  mi <- suppressWarnings(as.integer(m))
  mi[is.na(mi)] <- match(tolower(substr(m[is.na(mi)], 1, 3)), tolower(month.abb))
  # explicit format: a malformed value gives NA instead of stopping the read
  as.character(as.Date(sprintf("%s-%02d-%02d", trimws(year), mi, suppressWarnings(as.integer(day))), format = "%Y-%m-%d"))
}

.epi_hist_frame <- function(n, checklist) {
  out <- as.data.frame(stats::setNames(rep(list(rep(NA_character_, n)), length(.epi_hist_cols)), .epi_hist_cols),
                       check.names = FALSE, stringsAsFactors = FALSE)
  out[[.epi_hist_cols[["checklist"]]]] <- checklist
  out
}

# Read one sheet of an old Excel dashboard. The header is on row 1, or on row 2
# when row 1 holds section names (2020-23 Health sheet). Empty rows are dropped;
# ids = the _id column when there is one (duplicates dropped), else the row
# number. Rows identical in every field except the id are double entries.
.epi_read_dash_sheet <- function(tmp, sheet) {
  first <- .epi_norm(names(readxl::read_excel(tmp, sheet = sheet, n_max = 0, .name_repair = "minimal")))
  x <- as.data.frame(readxl::read_excel(tmp, sheet = sheet, skip = if ("day" %in% first) 0 else 1, col_types = "text",
                                        .name_repair = "minimal"), check.names = FALSE, stringsAsFactors = FALSE)
  x$.row <- seq_len(nrow(x))
  x <- x[rowSums(!is.na(x[, names(x) != ".row", drop = FALSE])) > 0, , drop = FALSE]
  id <- if ("_id" %in% names(x)) x[["_id"]] else as.character(x$.row)
  keep <- !is.na(id) & !duplicated(id)
  x <- x[keep, , drop = FALSE]; id <- id[keep]
  dup <- duplicated(x[, !names(x) %in% c("_id", ".row"), drop = FALSE])   # by position: some headers are blank
  list(x = x[!dup, , drop = FALSE], id = id[!dup])
}

# Fields every old sheet has (matched on header text, case/punctuation ignored)
.epi_dash_base <- function(s, checklist, id_prefix, facility_pat) {
  C <- .epi_hist_cols; x <- s$x
  o <- .epi_hist_frame(nrow(x), checklist)
  o[[C[["region"]]]] <- .epi_hcol(x, "^region$"); o[[C[["province"]]]] <- .epi_hcol(x, "^province$")
  o[[C[["district"]]]] <- .epi_hcol(x, "^district$"); o[[C[["dcode"]]]] <- .epi_hcol(x, "^dcode$|^district code$")
  o[[C[["ngo"]]]] <- .epi_hcol(x, "^name of bphs ngo")
  o[[C[["facility"]]]] <- .epi_hcol(x, facility_pat)
  o[[C[["ftype"]]]] <- .epi_hcol(x, "^type of health facility")
  o[[C[["supervisor"]]]] <- .epi_hcol(x, "^name title of supervisor")
  o[[C[["monitor"]]]] <- .epi_hcol(x, "^name of the monitor")
  o[[C[["date"]]]] <- .epi_dmy(.epi_hcol(x, "^day$"), .epi_hcol(x, "^month$"), .epi_hcol(x, "^year$"))
  o[[C[["id"]]]] <- paste0(id_prefix, s$id)
  o
}

.epi_set_yn <- function(o, x, defs, pats) {
  for (k in names(pats)) o[[defs$column[defs$key == k]]] <- .epi_yn12(.epi_hcol(x, pats[[k]]))
  o
}

# Health sheet: the old combined facility + session checklist. When the file has
# no Session sheet, session rows are derived from it (visits where a session was
# seen), with only the questions both forms share; the others stay blank.
.epi_dash_health <- function(tmp, sheet, tag, derive_session) {
  s <- .epi_read_dash_sheet(tmp, sheet)
  hc <- .epi_dash_base(s, "Health Center", paste0(tag, "-health-"), "^name of health facility")
  hc <- .epi_set_yn(hc, s$x, epi_health_indicators, c(
    microplan = "^is there up to date ?micro ?plan", within5km = "^are all the outreach sessions in the micro ?plan",
    coverage_chart = "^is there updated epi coverage monitoring chart", trained = "^has vaccinator been trained",
    community_link = "^are there linkages with community", referral = "^are children and women referred to epi center",
    cold_chain = "^is cold chain equi", pemt_supervision = "^is the outreach supervised regularly by the pemt",
    ngo_supervision = "^is the outreach supervised regularly by the ngos"))
  if (!derive_session) return(hc)
  ss <- .epi_dash_base(s, "Session", paste0(tag, "-health-"), "^epi team location")
  pats <- c(tally_sheet = "^tally sheet$", imm_cards = "^immunization cards$",
            msg_importance = "^1 importance of vaccines$", msg_reactions = "^2 vaccine probable reactions$")
  ss <- .epi_set_yn(ss, s$x, epi_session_indicators, pats)
  seen <- trimws(.epi_hcol(s$x, "^session during visit")) == "1"
  answered <- rowSums(!is.na(ss[epi_session_indicators$column[epi_session_indicators$key %in% names(pats)]])) > 0
  dplyr::bind_rows(hc, ss[!is.na(seen) & seen & answered, , drop = FALSE])
}

# Session sheet (1 = Yes, 2 = No). "Deliver communication messages" is one
# question there, not the two Kobo messages, so those two stay blank.
.epi_dash_session <- function(tmp, sheet, tag) {
  s <- .epi_read_dash_sheet(tmp, sheet)
  ss <- .epi_dash_base(s, "Session", paste0(tag, "-session-"), "^location$")
  st <- tolower(trimws(.epi_hcol(s$x, "^session$")))   # free text: keep the known types only
  ss[[.epi_hist_cols[["stype"]]]] <- ifelse(grepl("^out", st), "Outreach", ifelse(grepl("^fix", st), "Fixed",
                                            ifelse(grepl("^mob", st), "Mobile", NA_character_)))
  .epi_set_yn(ss, s$x, epi_session_indicators, c(
    referred = "^are children and women referred to session", suitable_place = "^did the team select a suitable place",
    icepacks = "^does the team have an appropriate vaccine carrier", vvm = "^are there any vaccines with vvm",
    safety_box = "^does the epi health worker ehw use safety box",
    technique = "^does the ehw follow proper administration technique",
    tally_sheet = "^does the ehw fill tally sheet correctly", imm_cards = "^does the ehw fill immunization cards correctly"))
}

# Community sheet: children seen + 13 markers. Markers are taken by header when
# the sheet names them; otherwise the 13 unheaded columns after "children seen"
# in the Kobo order (card, BCG, OPV-0 ... Measles-2), as in the 2020-23 file.
.epi_dash_community <- function(tmp, sheet, tag) {
  s <- .epi_read_dash_sheet(tmp, sheet)
  co <- .epi_dash_base(s, "Community", paste0(tag, "-community-"), "^nearest vaccination post")
  nm <- .epi_norm(names(s$x))
  ch <- which(grepl("^total number of 12 2[34] months children", nm))[1]
  if (is.na(ch)) return(co)
  co[[epi_children_col]] <- s$x[[ch]]
  by_name <- match(.epi_norm(epi_markers$column), nm)
  for (i in seq_len(nrow(epi_markers)))
    co[[epi_markers$column[i]]] <- s$x[[if (!anyNA(by_name)) by_name[i] else ch + i]]
  co
}

.epi_read_hist_dashboard <- function(tmp, sheets, tag) {
  hs <- sheets[grepl("^health", trimws(sheets), ignore.case = TRUE)]   # sheet names can end in a space
  cs <- grep("community", sheets, ignore.case = TRUE, value = TRUE)
  ss <- grep("session", sheets, ignore.case = TRUE, value = TRUE)
  dplyr::bind_rows(
    if (length(hs)) .epi_dash_health(tmp, hs[1], tag, derive_session = !length(ss)),
    if (length(ss)) .epi_dash_session(tmp, ss[1], tag),
    if (length(cs)) .epi_dash_community(tmp, cs[1], tag))
}

# ---- Read every file in data/Historic ------------------------------------------------
.epi_read_historic_file <- function(file) {
  tmp <- tempfile(fileext = ".xlsx"); on.exit(unlink(tmp))
  file.copy(file, tmp)
  sh <- readxl::excel_sheets(tmp)
  heads <- lapply(sh, function(s) names(readxl::read_excel(tmp, sheet = s, n_max = 0, .name_repair = "minimal")))
  kobo <- which(vapply(heads, function(h) "select_Checklist" %in% h, logical(1)))
  dash <- grepl("^health|community|session", trimws(sh), ignore.case = TRUE)
  tag <- paste0("hist-", tolower(gsub("[^A-Za-z0-9]", "", tools::file_path_sans_ext(basename(file)))))
  out <- if (length(kobo)) .epi_read_hist_kobo(tmp, sh[kobo[1]])
         else if (any(dash)) .epi_read_hist_dashboard(tmp, sh[dash], tag)
         else { warning("Historic file not recognised (skipped): ", basename(file)); NULL }
  if (!is.null(out) && nrow(out)) {
    out <- .epi_hist_fix_geo(out)
    out$.source <- paste0("Historic (", basename(file), ")")
  }
  out
}

# Old sheets use region codes (NE, SE, SR) and the odd province spelling.
# Province aliases are fixed; a region that is not one of the boundary file's
# regions is taken from the province (rows with a standard region are kept as is).
.epi_hist_province_alias <- c(kundoz = "Kunduz")

.epi_hist_fix_geo <- function(out) {
  pc <- .epi_hist_cols[["province"]]; rc <- .epi_hist_cols[["region"]]
  p <- trimws(out[[pc]])
  a <- .epi_hist_province_alias[tolower(p)]
  out[[pc]] <- ifelse(is.na(a), p, unname(a))
  g <- tryCatch(unique(sf::st_drop_geometry(readRDS(epi_paths()$geography)$districts)[, c("region", "province")]),
                error = function(e) NULL)
  if (is.null(g)) return(out)
  r <- toupper(trimws(out[[rc]]))
  from_prov <- g$region[match(tolower(out[[pc]]), tolower(g$province))]
  fix <- !(r %in% toupper(g$region)) & !is.na(from_prov)
  out[[rc]][fix] <- from_prov[fix]
  out
}

# One text key per visit, for rows with the export's column labels (live Kobo or
# historic): checklist | date | district | facility | answers. Session answers
# leave out the two messages (most old Session sheets do not have them).
.epi_visit_key <- function(raw) {
  n <- if (is.null(raw)) 0L else nrow(raw)
  if (!n) return(character())
  col <- function(nm) if (nm %in% names(raw)) raw[[nm]] else rep(NA_character_, n)
  nz <- function(v) tolower(gsub("[^A-Za-z0-9]", "", v))
  join <- function(cols, f) do.call(paste, c(lapply(cols, function(cc) f(col(cc))), sep = ","))
  C <- .epi_hist_cols
  ck <- .epi_clean_text(col(C[["checklist"]]))
  ses <- epi_session_indicators$column[!epi_session_indicators$key %in% c("msg_importance", "msg_reactions")]
  ans <- ifelse(ck == "Health Center", join(epi_health_indicators$column, epi_yes_no),
         ifelse(ck == "Session", join(ses, epi_yes_no),
         ifelse(ck == "Community", join(c(epi_children_col, epi_markers$column), epi_num), "")))
  paste(ck, format(epi_parse_date(col(C[["date"]]))), nz(col(C[["district"]])), nz(col(C[["facility"]])), ans, sep = "|")
}

# All historic rows (export labels, all text). Cached in app_data/historic_raw.rds
# and rebuilt only when a file in data/Historic changes.
.epi_historic_files <- function(paths = epi_paths()) {
  files <- sort(list.files(paths$historic, pattern = "[.]xlsx$", full.names = TRUE))
  files[!startsWith(basename(files), "~$")]
}

# Changes when a file is added, removed or edited, or the readers change.
epi_historic_signature <- function(paths = epi_paths()) {
  files <- .epi_historic_files(paths)
  info <- file.info(files)
  paste(epi_historic_version, basename(files), info$size, as.numeric(info$mtime), collapse = "|")
}

epi_historic_raw <- function(paths = epi_paths()) {
  files <- .epi_historic_files(paths)
  if (!length(files)) return(NULL)
  sig <- epi_historic_signature(paths)
  if (file.exists(paths$historic_cache)) {
    h <- tryCatch(readRDS(paths$historic_cache), error = function(e) NULL)
    if (!is.null(h) && identical(attr(h, "signature"), sig)) return(h)
  }
  h <- dplyr::bind_rows(lapply(files, .epi_read_historic_file))
  h <- h[!duplicated(paste(h[[.epi_hist_cols[["checklist"]]]], h[["_id"]])), , drop = FALSE]
  h$.key <- .epi_visit_key(h)
  attr(h, "signature") <- sig
  dir.create(dirname(paths$historic_cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(h, paths$historic_cache)
  h
}

# Live export + historic rows not already in it (by _id or by visit) and not in
# a file read earlier (by visit) -> prepared tables.
# d$historic = per file: rows read, already in Kobo, already in an earlier file, added.
epi_prepare_with_history <- function(raw, paths = epi_paths()) {
  failed <- FALSE
  h <- tryCatch(epi_historic_raw(paths), error = function(e) {
    warning("Historic data not loaded: ", conditionMessage(e)); failed <<- TRUE; NULL })
  summary <- data.frame(Source = character(), Read = integer(), InKobo = integer(), InEarlier = integer(),
                        Added = integer())
  if (!is.null(h) && nrow(h)) {
    if (is.null(h$.key)) h$.key <- .epi_visit_key(h)
    live_ids <- if (!is.null(raw)) raw[["_id"]] else character()
    in_kobo <- h[["_id"]] %in% live_ids[!is.na(live_ids)] | h$.key %in% .epi_visit_key(raw)
    ord <- match(h$.source, unique(h$.source))           # files in read (file-name) order
    first <- tapply(ord, h$.key, min)
    in_earlier <- !in_kobo & unname(first[h$.key]) < ord
    new <- !in_kobo & !in_earlier
    summary <- data.frame(Source = unique(h$.source))
    cnt <- function(v) vapply(summary$Source, function(s) sum(h$.source == s & v), integer(1), USE.NAMES = FALSE)
    summary$Read <- cnt(TRUE); summary$InKobo <- cnt(in_kobo); summary$InEarlier <- cnt(in_earlier)
    summary$Added <- cnt(new)
    h$.key <- NULL
    raw <- if (is.null(raw)) h[new, , drop = FALSE] else dplyr::bind_rows(raw, h[new, , drop = FALSE])
  }
  d <- epi_prepare(raw)
  d$historic <- summary
  # epi_load_data() rebuilds when the files change (or the last read failed)
  d$historic_sig <- if (failed) NA_character_ else epi_historic_signature(paths)
  d
}

# ---- Flags used by the app and the decks -------------------------------------------------
epi_historic_count <- function(data) {
  sum(vapply(data[intersect(names(data), names(epi_pages))],
             function(x) if (is.null(x$Source)) 0L else sum(x$Source != "Kobo"), integer(1)))
}

epi_source_note <- function(data, base = "Source: KoboToolbox, Routine EPI checklist (EPI_BACKUP)") {
  n <- epi_historic_count(data)
  if (n) sprintf("%s + %s historic records (files kept before Kobo)", base, format(n, big.mark = ",")) else base
}
