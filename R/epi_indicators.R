# epi_indicators.R --------------------------------------------------------
# Indicator definitions (taken from the Power BI report "Routine_EPI_Dashboard"
# and its Power Query), data preparation, filters and summaries.
#
# Each Yes/No indicator = share of "Yes" among Yes + No answers (blanks are not
# counted), the same as the Power BI measures %_<x>Yes = Yes / (Yes + No).

epi_schema_version <- "4"   # 4 = expanded indicators, implementation counts, composite session supplies

# ---- Definitions ------------------------------------------------------------
.epi_def <- function(...) {
  x <- data.frame(matrix(c(...), ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
  names(x) <- c("key", "title", "column")
  x
}

epi_health_indicators <- .epi_def(
  "catchment_map", "% of health facilities with a catchment-area map or sketch",
  "Is there a map/sketch of the catchment area?",
  "target_population", "% of catchment maps reflecting the current-year target population",
  "Does it reflect the target population of the current year?",
  "microplan", "% of availability of micro-plan for routine EPI (including outreach and mobile)",
  "Is there up-to-date micro plan for routine EPI (including outreach and mobile)?",
  "within5km", "% of all the outreach sessions in the micro-plan within a distance of <5 km",
  "Are all the outreach sessions in the micro plan in the distance of <5 km?",
  "coverage_chart", "% of health facilities displaying the EPI coverage monitoring chart",
  "Is there updated EPI coverage monitoring chart on display?",
  "trained", "% of vaccinators trained in the past 12 months",
  "Has vaccinator been trained over the last 12 months?",
  "community_link", "% of HF having linkages with community",
  "Are there linkages with community?",
  "referral", "% of HF referring children and women to RI",
  "Are children and women referred to EPI center by medical officer, CHWs and practitioners?",
  "cold_chain", "% of refrigerators, cold boxes and vaccine carriers available at the center",
  "Is cold chain equipment including refrigerator, cold boxes and vaccine carriers available at the center?",
  "temperature_ok", "% of refrigerators monitored twice daily and maintained at +2 to +8 C",
  "Is the temperature of refrigerator monitored twice a day and stays between (+2  and +8 C)?",
  "fridge_tag", "% of fridge-tags used properly with no alarm and report generated",
  "Is fridge-tag properly used (no alarm status, report is generated)",
  "hc_tally", "% of health centers completing tally sheets correctly",
  "Tally Sheet",
  "hc_cards", "% of health centers completing immunization cards correctly",
  "Immunization cards",
  "hc_register", "% of health centers completing registration books correctly",
  "Registration book",
  "hc_monthly_report", "% of health centers completing monthly reports correctly",
  "Monthly report",
  "penta3_calc", "% of vaccinators correctly calculating cumulative Penta-3 coverage",
  "Is vaccinator properly calculating cumulative Penta 3 coverage (number of children received Penta 3 vaccine from January until last month/annual target for Penta 3)",
  "dropout_calc", "% of vaccinators correctly calculating Penta-1 to Penta-3 dropout",
  "Is vaccinator properly calculating Penta 1- Penta 3 drop-out rate (Penta 1 -Penta 3/ Penta 1 x 100%)",
  "defaulter_followup", "% of health centers following up children due for subsequent vaccination",
  "Does EHW follow-up on the vaccinations due for the child, such as Penta-2.3, Measles 1,2?",
  "pemt_supervision", "% of outreach sessions supervised by PEMT supervisors",
  "Is the outreach supervised regularly by the PEMT/EPI supervisor (quarterly)? Verify from logbook/register book",
  "ngo_supervision", "% of outreach sessions supervised by NGO supervisors",
  "Is the outreach supervised regularly by the NGOs/EPI supervisor (quarterly)? Verify from logbook/register book",
  "pemt_mobile_supervision", "% of health-center assessments confirming monthly PEMT/EPI mobile supervision",
  "Are the mobile sessions supervised regularly by the PEMT/EPI supervisor every month? Verify from logbook/register book",
  "ngo_mobile_supervision", "% of health-center assessments confirming quarterly NGO/EPI mobile supervision",
  "Are the mobile sessions supervised regularly by the NGOs/EPI supervisor (quarterly)? Verify from logbook/register book"
)

epi_session_indicators <- .epi_def(
  "referred", "% of sessions where children and women are referred to the session",
  "Are children and women referred to session?",
  "suitable_place", "% of teams that selected a suitable outreach place recommended by the community",
  "Did the team select a suitable place for outreach recommended by community?",
  "icepacks", "% of teams with a vaccine carrier and 4 conditioned icepacks",
  "Does the team have an appropriate vaccine carrier with 4 conditioned icepacks?",
  "session_trained", "% of sessions with a recently trained vaccinator",
  "Has the vaccinator been trained recently?",
  "contact_person", "% of sessions with a community contact person",
  "Is there community contact person?",
  "duration_3h", "% of outreach/mobile sessions lasting at least three hours",
  "Does the vaccinator spend at least three hours for outreach and mobile sessions in the community?",
  "supplies", "% of sessions with vaccines and supplies available",
  "Are vaccines/ supplies available for the current session?",
  "expired", "% of sessions with expired vaccines (lower is better)",
  "Are there any expired vaccines?",
  "vvm", "% of sessions with vaccines with VVM in 3rd/4th stage (lower is better)",
  "Are there any vaccines with VVM in third and fourth stages?",
  "safety_box", "% of EHWs using a safety box",
  "Does the EPI Health Worker (EHW) use safety box?",
  "technique", "% of EHWs following proper administration technique",
  "Does the EHW follow proper administration technique?",
  "open_vial", "% of EHWs understanding and properly using the open-vial policy",
  "Does the EHW understands and properly use the open vial policy",
  "tally_sheet", "% of EHWs filling the tally sheet correctly",
  "Does the EHW fill tally sheet correctly?",
  "msg_importance", "% of EHWs delivering the message: importance of vaccines",
  "Importance of vaccines",
  "msg_reactions", "% of EHWs delivering the message: vaccine probable reactions",
  "Vaccine probable  reactions",
  "imm_cards", "% of EHWs filling immunization cards correctly",
  "Does the EHW fill immunization cards correctly?",
  "session_register", "% of EHWs filling registration books correctly",
  "Does the EHW fill registration book correctly?",
  "aefi_aware", "% of EHWs aware of AEFI",
  "Is EHW aware of the AEFI?",
  "next_contact", "% of EHWs communicating the date of next contact",
  "Date of next contact",
  "card_retention", "% of EHWs explaining the importance of card retention",
  "Importance of card retention",
  "caregiver_feedback", "% of sessions where EHWs obtain caregiver feedback on messages",
  "Does the EHW get feed-back from the mother on the messages?"
)

# Community coverage markers (numerators); denominator = children 12-23 months checked.
epi_markers <- data.frame(
  label = c("Vaccination card", "BCG", "OPV-0", "OPV-1", "Penta-1", "PCV-1", "OPV-3", "Penta-3",
            "PCV-3", "Rota-1", "Rota-2", "Measles-1", "Measles-2"),
  column = c("Vaccination card available", "BCG", "OPV-0", "OPV-1", "Penta-1", "PCV-1", "OPV-3",
             "Penta-3", "PCV-3", "Rota-1", "Rota-2", "Measles-1", "Measles-2"),
  stringsAsFactors = FALSE)
epi_markers$key <- paste0("m_", gsub("[^a-z0-9]", "", tolower(epi_markers$label)))
# The seven markers shown in Power BI, in its order
epi_default_markers <- c("Vaccination card", "BCG", "PCV-3", "OPV-3", "Penta-3", "Rota-1", "Measles-1")
epi_children_col <- "Number of Children checked in Community aged 12-23 Monhts"

.epi_reason_prefix <- "Why the children have not been vaccinated: (select multiple)/"
epi_reasons <- data.frame(
  label = c("Lack of awareness of parents/caregivers", "Parents/caregivers do not believe in immunization",
            "Refusal after side effects of a previous dose",
            "Family/community reasons (purdah, mother busy, child sick)", "Immunization session is too far",
            "Timing inconvenient / long wait", "No female vaccinator available", "Security issues"),
  column = paste0(.epi_reason_prefix, c(
    "Lack of awareness of parents/caregivers", "Parents/caregivers do not believe in immunization.",
    "Refusal following side effects to the previous dose.",
    "Family/community reasons (purdah, mother was busy, child was sick)", "Immunization session is too far.",
    "Timing inconvenient/long wait", "A female vaccinator is not available.", "Security issues")),
  stringsAsFactors = FALSE)
epi_reasons$key <- paste0("r_", seq_len(nrow(epi_reasons)))
epi_reasons$short <- c("awareness", "don't believe", "side effects", "family reasons", "too far", "timing / wait",
                       "no female vaccinator", "security")

# Short labels for table headers
epi_health_indicators$label <- c("Catchment map", "Current target", "Micro-plan", "Outreach <5 km", "Coverage chart",
  "Trained 12 m", "Community links", "Referral", "Cold-chain equipment", "Temperature +2 to +8 C", "Fridge-tag",
  "Tally sheet", "Immunization cards", "Registration book", "Monthly report", "Penta-3 calculation",
  "Dropout calculation", "Defaulter follow-up", "PEMT outreach supervision", "NGO outreach supervision",
  "PEMT mobile supervision", "NGO mobile supervision")
epi_session_indicators$label <- c("Referred to session", "Suitable place", "Carrier + icepacks", "Recently trained",
  "Community contact", "Session >=3 hours", "Vaccines/supplies", "Expired vaccines", "VVM stage 3/4", "Safety box",
  "Admin technique", "Open-vial policy", "Tally sheet", "Msg: importance", "Msg: reactions", "Immunization cards",
  "Registration book", "AEFI awareness", "Next-contact date", "Card retention", "Caregiver feedback")

epi_pages <- c(health = "Health center", session = "Session", community = "Community")

# ---- Helpers ----------------------------------------------------------------
.epi_col <- function(raw, name) {
  if (is.null(raw) || !name %in% names(raw)) return(rep(NA_character_, if (is.null(raw)) 0L else nrow(raw)))
  raw[[name]]
}

epi_yes_no <- function(x) {
  x <- tolower(trimws(x))
  ifelse(x == "yes", "Yes", ifelse(x == "no", "No", NA_character_))
}

epi_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", trimws(x), fixed = TRUE)))

# Kobo CSV gives 2025-03-01; Excel read as text gives the serial number 45717.
epi_parse_date <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))
  num <- !is.na(x) & grepl("^[0-9]+([.][0-9]+)?$", x)
  out[num] <- as.Date(floor(as.numeric(x[num])), origin = "1899-12-30")
  oth <- !is.na(x) & !num
  out[oth] <- suppressWarnings(as.Date(substr(x[oth], 1, 10), format = "%Y-%m-%d"))
  out
}

.epi_clean_text <- function(x) {
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}

# Province names come from a Kobo list, but casing can vary: use the spelling
# of the boundary file when the name matches it (ignoring case).
.epi_province <- function(x) {
  x <- .epi_clean_text(x)
  std <- tryCatch(unique(readRDS(epi_paths()$geography)$districts$province), error = function(e) character())
  hit <- match(tolower(x), tolower(std))
  ifelse(is.na(hit), x, std[hit])
}

# ---- Preparation --------------------------------------------------------------
.epi_base <- function(raw) {
  vd <- epi_parse_date(.epi_col(raw, "Date of Visit"))
  data.frame(
    checklist = .epi_clean_text(.epi_col(raw, "Select Checklist is to be filled:")),
    Region = toupper(.epi_clean_text(.epi_col(raw, "Region"))),
    Province = .epi_province(.epi_col(raw, "Province")),
    District = .epi_clean_text(.epi_col(raw, "District")),
    DCode = suppressWarnings(as.integer(epi_num(.epi_col(raw, "District Code")))),
    NGO = .epi_clean_text(.epi_col(raw, "Name of BPHS NGO")),
    Facility = .epi_clean_text(.epi_col(raw, "Name of Health Facility/ Nearest Facility Name")),
    FacilityType = .epi_clean_text(.epi_col(raw, "Type of Health Facility")),
    SessionType = .epi_clean_text(.epi_col(raw, "Session Type")),
    Supervisor = .epi_clean_text(.epi_col(raw, "Name & Title of Supervisor")),
    Monitor = .epi_clean_text(.epi_col(raw, "Name of Monitor")),
    VisitDate = vd,
    Year = as.integer(format(vd, "%Y")),
    Month = factor(month.name[as.integer(format(vd, "%m"))], levels = month.name),
    kobo_id = .epi_col(raw, "_id"),
    Source = { s <- .epi_col(raw, ".source"); ifelse(is.na(s), "Kobo", s) },   # "Historic (<file>)" = data/Historic
    stringsAsFactors = FALSE, check.names = FALSE)
}

epi_prepare <- function(raw) {
  base <- .epi_base(raw)
  pick <- function(type) which(base$checklist %in% type)

  yn_table <- function(rows, defs) {
    d <- base[rows, , drop = FALSE]
    for (i in seq_len(nrow(defs))) d[[defs$key[i]]] <- epi_yes_no(.epi_col(raw, defs$column[i])[rows])
    rownames(d) <- NULL
    d
  }
  health <- yn_table(pick("Health Center"), epi_health_indicators)
  session <- yn_table(pick("Session"), epi_session_indicators)
  hr <- pick("Health Center")
  health$outreach_planned <- epi_num(.epi_col(raw, "How many outreach sessions were planned last month?")[hr])
  health$outreach_conducted <- epi_num(.epi_col(raw, "How many outreach sessions were conducted last month?")[hr])
  health$mobile_planned <- epi_num(.epi_col(raw, "How many mobile sessions were planned last month?")[hr])
  health$mobile_conducted <- epi_num(.epi_col(raw, "How many mobile sessions were conducted last month?")[hr])
  sr <- pick("Session")
  supply_cols <- paste0("Are vaccines/ supplies available for the current session?/",
                        c("BCG", "Penta", "HepB", "IPV", "OPV", "Measles", "PCV", "Rota",
                          "AD syringes", "Reconstitution Syringe"))
  supply <- do.call(cbind, lapply(supply_cols, function(nm) epi_num(.epi_col(raw, nm)[sr])))
  answered <- rowSums(!is.na(supply))
  session$supplies <- ifelse(answered == 0, NA_character_, ifelse(rowSums(supply == 1, na.rm = TRUE) == ncol(supply), "Yes", "No"))

  rc <- pick("Community")
  community <- base[rc, , drop = FALSE]
  community$children <- epi_num(.epi_col(raw, epi_children_col)[rc])
  for (i in seq_len(nrow(epi_markers))) community[[epi_markers$key[i]]] <- epi_num(.epi_col(raw, epi_markers$column[i])[rc])
  for (i in seq_len(nrow(epi_reasons))) community[[epi_reasons$key[i]]] <- epi_num(.epi_col(raw, epi_reasons$column[i])[rc])
  rownames(community) <- NULL

  list(schema = epi_schema_version, health = health, session = session, community = community)
}

# ---- Filters ----------------------------------------------------------------
# f = list(regions, provinces, years, months); empty = all. Visits without a
# date or before the start date are dropped (as the Power BI queries did).
epi_filter <- function(d, f = list(), start = epi_start_date()) {
  keep <- !is.na(d$VisitDate) & d$VisitDate >= start
  if (length(f$regions))   keep <- keep & d$Region %in% f$regions
  if (length(f$provinces)) keep <- keep & d$Province %in% f$provinces
  if (length(f$years))     keep <- keep & d$Year %in% as.integer(f$years)
  if (length(f$months))    keep <- keep & as.character(d$Month) %in% f$months
  d[keep, , drop = FALSE]
}

epi_filter_label <- function(f) {
  part <- function(lbl, v) if (length(v)) paste0(lbl, ": ", paste(v, collapse = ", ")) else NULL
  x <- c(part("Region", f$regions), part("Province", f$provinces), part("Year", f$years), part("Month", f$months))
  if (length(x)) paste(x, collapse = " | ") else "All regions, all periods"
}

# Bars group by region, or by province when exactly one region is selected
# (Power BI GeoAxis behaviour). "Region"/"Province" force a level.
epi_geo_level <- function(choice = "Auto", regions = NULL) {
  if (identical(choice, "Region") || identical(choice, "Province")) return(choice)
  if (length(regions) == 1) "Province" else "Region"
}

# ---- Summaries ----------------------------------------------------------------
epi_yn_by <- function(d, key, by = "Region") {
  v <- d[[key]]; g <- d[[by]]
  ok <- !is.na(v) & !is.na(g)
  if (!any(ok)) return(data.frame(group = character(), Yes = integer(), No = integer(), Total = integer(), pct = numeric()))
  t <- table(factor(g[ok]), factor(v[ok], levels = c("Yes", "No")))
  out <- data.frame(group = rownames(t), Yes = as.integer(t[, "Yes"]), No = as.integer(t[, "No"]), stringsAsFactors = FALSE)
  out$Total <- out$Yes + out$No
  out$pct <- out$Yes / out$Total
  out[order(out$group), , drop = FALSE]
}

epi_yn_overall <- function(d, key) {
  v <- d[[key]]
  y <- sum(v == "Yes", na.rm = TRUE); n <- sum(v == "No", na.rm = TRUE)
  list(Yes = y, No = n, Total = y + n, pct = if (y + n > 0) y / (y + n) else NA_real_)
}

epi_yn_summary <- function(d, defs) {
  do.call(rbind, lapply(seq_len(nrow(defs)), function(i) {
    o <- epi_yn_overall(d, defs$key[i])
    data.frame(Indicator = defs$title[i], Yes = o$Yes, No = o$No, Answered = o$Total,
               `% Yes` = o$pct, check.names = FALSE, stringsAsFactors = FALSE)
  }))
}

epi_visits_by <- function(d, by = "Region") {
  g <- d[[by]]; g <- g[!is.na(g)]
  if (!length(g)) return(data.frame(group = character(), Visits = integer()))
  t <- table(g)
  out <- data.frame(group = names(t), Visits = as.integer(t), stringsAsFactors = FALSE)
  out[order(out$group), , drop = FALSE]
}

epi_visits_by_district <- function(d) {
  g <- d$DCode[!is.na(d$DCode)]
  if (!length(g)) return(data.frame(DCode = integer(), Visits = integer()))
  t <- table(g)
  data.frame(DCode = as.integer(names(t)), Visits = as.integer(t))
}

# Coverage % = sum(vaccinated) / sum(children checked), rows with a valid
# children count only. by = NULL gives one overall row.
epi_coverage <- function(d, markers = epi_default_markers, by = NULL) {
  m <- epi_markers[match(markers, epi_markers$label), , drop = FALSE]
  m <- m[!is.na(m$key), , drop = FALSE]
  d <- d[!is.na(d$children) & d$children > 0, , drop = FALSE]
  grp <- if (is.null(by)) rep("All", nrow(d)) else as.character(d[[by]])
  ok <- !is.na(grp)
  d <- d[ok, , drop = FALSE]; grp <- grp[ok]
  if (!nrow(d) || !nrow(m)) return(data.frame(group = character(), marker = character(), children = numeric(),
                                              vaccinated = numeric(), pct = numeric()))
  out <- do.call(rbind, lapply(split(seq_len(nrow(d)), grp), function(idx) {
    ch <- sum(d$children[idx])
    do.call(rbind, lapply(seq_len(nrow(m)), function(j) {
      vac <- sum(d[[m$key[j]]][idx], na.rm = TRUE)
      data.frame(group = grp[idx[1]], marker = m$label[j], children = ch, vaccinated = vac,
                 pct = if (ch > 0) min(vac / ch, 1.5) else NA_real_, stringsAsFactors = FALSE)
    }))
  }))
  out$marker <- factor(out$marker, levels = m$label)
  out[order(out$group, out$marker), , drop = FALSE]
}

epi_reason_summary <- function(d) {
  n_visits <- nrow(d)
  out <- data.frame(Reason = epi_reasons$label,
                    Visits = vapply(epi_reasons$key, function(k) sum(d[[k]] > 0, na.rm = TRUE), numeric(1)),
                    stringsAsFactors = FALSE)
  out$pct <- if (n_visits > 0) out$Visits / n_visits else NA_real_
  out[order(-out$Visits), , drop = FALSE]
}

# Colours follow the marker (its position in the full marker list), not its
# rank in the current selection; the palette has 8 slots, so at most 8 markers.
epi_marker_colors <- function(markers) {
  order_all <- c(epi_default_markers, setdiff(epi_markers$label, epi_default_markers))
  m <- markers[markers %in% order_all]
  # defaults keep slots 1-7; any extra marker takes the next free slot
  slots <- match(m, epi_default_markers)
  free <- setdiff(seq_along(epi_colors$series), slots[!is.na(slots)])
  slots[is.na(slots)] <- free[seq_len(sum(is.na(slots)))]
  stats::setNames(epi_colors$series[slots], m)
}

# ---- Export tables ------------------------------------------------------------
# Prepared table with readable column names (Kobo question text / marker and
# reason labels) for the Data page and the Excel download.
# short = TRUE gives short headers for the on-screen table (long Kobo question
# texts make very tall table headers); the Excel download keeps the full text.
epi_export_table <- function(d, page, short = FALSE) {
  d$Month <- as.character(d$Month)
  map <- switch(page,
    health = stats::setNames(if (short) epi_health_indicators$label else epi_health_indicators$column,
                             epi_health_indicators$key),
    session = stats::setNames(if (short) epi_session_indicators$label else epi_session_indicators$column,
                              epi_session_indicators$key),
    community = c(children = if (short) "Children checked" else "Children 12-23 months checked",
                  stats::setNames(if (short) epi_markers$label else paste(epi_markers$label, "(vaccinated)"), epi_markers$key),
                  stats::setNames(paste("Reason:", if (short) epi_reasons$short else epi_reasons$label), epi_reasons$key)))
  hit <- names(d) %in% names(map)
  names(d)[hit] <- unname(map[names(d)[hit]])
  d$checklist <- NULL
  d
}

# ---- Combined summaries (Home page + management review deck) -------------------
epi_month_label <- function(key) format(as.Date(paste0(key, "-01")), "%b %Y")

# One row per checklist: page, checklist label, Region, Province, month key (YYYY-MM)
epi_responses <- function(data) {
  out <- do.call(rbind, lapply(names(epi_pages), function(p) {
    d <- data[[p]]
    if (is.null(d) || !nrow(d)) return(NULL)
    data.frame(page = p, checklist = epi_pages[[p]], Region = d$Region, Province = d$Province,
               DCode = d$DCode, month = format(d$VisitDate, "%Y-%m"), stringsAsFactors = FALSE)
  }))
  if (is.null(out)) out <- data.frame(page = character(), checklist = character(), Region = character(),
                                      Province = character(), DCode = integer(), month = character())
  out
}

# Responses by month and checklist type (long): month, checklist, n
epi_responses_by_month <- function(data) {
  r <- epi_responses(data)
  if (!nrow(r)) return(data.frame(month = character(), checklist = character(), n = integer()))
  t <- as.data.frame(table(month = r$month, checklist = factor(r$checklist, levels = unname(epi_pages))),
                     stringsAsFactors = FALSE)
  names(t)[3] <- "n"
  t[order(t$month, t$checklist), ]
}

# Region x month table of responses (wide) with totals. pages = which checklists.
epi_region_month_table <- function(data, pages = names(epi_pages)) {
  r <- epi_responses(data[pages])
  r <- r[!is.na(r$Region) & !is.na(r$month), , drop = FALSE]
  if (!nrow(r)) return(data.frame(Region = character()))
  months <- sort(unique(r$month))
  t <- table(factor(r$Region), factor(r$month, levels = months))
  wide <- as.data.frame.matrix(t)
  names(wide) <- epi_month_label(months)
  wide$Total <- rowSums(wide)
  wide <- cbind(Region = rownames(wide), wide, stringsAsFactors = FALSE)
  tot <- as.data.frame(as.list(c(Region = "Total", colSums(wide[, -1, drop = FALSE]))), check.names = FALSE,
                       stringsAsFactors = FALSE)
  for (j in names(tot)[-1]) tot[[j]] <- as.numeric(tot[[j]])
  rownames(wide) <- NULL
  rbind(wide, tot)
}

# Yes share of every indicator by group (long): key, title, group, pct, Total
epi_yn_matrix <- function(d, defs, by = "Region") {
  do.call(rbind, lapply(seq_len(nrow(defs)), function(i) {
    t <- epi_yn_by(d, defs$key[i], by)
    if (!nrow(t)) return(NULL)
    data.frame(key = defs$key[i], title = defs$title[i], group = t$group, pct = t$pct, Total = t$Total,
               stringsAsFactors = FALSE)
  }))
}

# Yes share by month for one indicator
epi_yn_trend <- function(d, key) {
  d$month <- format(d$VisitDate, "%Y-%m")
  t <- epi_yn_by(d, key, "month")
  names(t)[1] <- "month"
  t
}

# Coverage by month for one marker
epi_coverage_trend <- function(d, marker) {
  d$month <- format(d$VisitDate, "%Y-%m")
  cv <- epi_coverage(d, marker, by = "month")
  names(cv)[1] <- "month"
  cv
}

# Penta-1 to Penta-3 drop-out = (Penta-1 - Penta-3) / Penta-1
epi_dropout <- function(d, by = NULL) {
  cv <- epi_coverage(d, c("Penta-1", "Penta-3"), by = by)
  if (!nrow(cv)) return(data.frame(group = character(), dropout = numeric()))
  w <- tidyr::pivot_wider(cv[, c("group", "marker", "vaccinated")], names_from = "marker", values_from = "vaccinated")
  data.frame(group = w$group, dropout = ifelse(w$`Penta-1` > 0, (w$`Penta-1` - w$`Penta-3`) / w$`Penta-1`, NA_real_),
             stringsAsFactors = FALSE)
}

# ---- Report month, year to date, compliance ------------------------------------
.epi_month_key <- function(d) format(d$VisitDate, "%Y-%m")
.epi_add_months <- function(key, n) format(seq(as.Date(paste0(key, "-01")), by = paste(n, "month"), length.out = 2)[2], "%Y-%m")

# Last complete calendar month (the month before today); if it has no data,
# the latest month that has data.
epi_ref_month <- function(data, today = Sys.Date()) {
  last <- .epi_add_months(format(today, "%Y-%m"), -1)
  have <- unique(unlist(lapply(data, .epi_month_key)))
  have <- have[!is.na(have)]
  if (!length(have) || last %in% have) return(last)
  max(have[have <= last], max(have))
}

epi_subset_months <- function(data, months) {
  lapply(data, function(d) d[!is.na(d$VisitDate) & .epi_month_key(d) %in% months, , drop = FALSE])
}

# Months of the report month's year, January .. report month
epi_ytd_months <- function(ref) {
  y <- substr(ref, 1, 4)
  m <- sprintf("%s-%02d", y, seq_len(as.integer(substr(ref, 6, 7))))
  m
}

# Provinces expected to report: all provinces of the boundary file in the
# selected regions (or the selected provinces).
epi_province_scope <- function(regions = NULL, provinces = NULL) {
  g <- sf::st_drop_geometry(epi_geo()$districts)
  p <- unique(g[, c("region", "province")])
  names(p) <- c("Region", "Province")
  if (length(regions)) p <- p[p$Region %in% regions, ]
  if (length(provinces)) p <- p[p$Province %in% provinces, ]
  p[order(p$Region, p$Province), ]
}

# Province x month checklist counts (zeros kept) for the chosen checklists.
epi_compliance_grid <- function(data, months, pages = names(epi_pages), regions = NULL, provinces = NULL) {
  scope <- epi_province_scope(regions, provinces)
  r <- epi_responses(data[pages])
  r <- r[r$month %in% months, , drop = FALSE]
  t <- table(factor(r$Province, levels = scope$Province), factor(r$month, levels = months))
  out <- cbind(scope, as.data.frame.matrix(t), stringsAsFactors = FALSE)
  names(out) <- c("Region", "Province", epi_month_label(months))
  rownames(out) <- NULL
  mc <- epi_month_label(months)
  out$`Months reported` <- rowSums(out[, mc, drop = FALSE] > 0)
  out$Total <- rowSums(out[, mc, drop = FALSE])
  out
}

# Share of provinces in scope with at least one checklist, by month and type
epi_compliance_by_month <- function(data, months, regions = NULL, provinces = NULL) {
  scope <- epi_province_scope(regions, provinces)
  r <- epi_responses(data)
  do.call(rbind, lapply(names(epi_pages), function(p) {
    do.call(rbind, lapply(months, function(m) {
      got <- unique(r$Province[r$page == p & r$month == m])
      k <- sum(scope$Province %in% got)
      data.frame(month = m, checklist = epi_pages[[p]], reporting = k, expected = nrow(scope),
                 pct = if (nrow(scope)) k / nrow(scope) else NA_real_, stringsAsFactors = FALSE)
    }))
  }))
}

# Region summary of reporting compliance for one set of months.
# Denominator = provinces of the region in the boundary file ("Provinces");
# numerator = provinces with >= 1 checklist of that type ("<type> n").
# The "<type>" columns hold the share (numerator / denominator).
epi_compliance_by_region <- function(data, months, regions = NULL, provinces = NULL) {
  scope <- epi_province_scope(regions, provinces)
  r <- epi_responses(data)
  r <- r[r$month %in% months, , drop = FALSE]
  out <- do.call(rbind, lapply(split(scope, scope$Region), function(s) {
    rep_by <- function(p) sum(s$Province %in% r$Province[r$page == p])
    none <- s$Province[!s$Province %in% r$Province]
    data.frame(Region = s$Region[1], Provinces = nrow(s),
               `Health center n` = rep_by("health"), `Session n` = rep_by("session"),
               `Community n` = rep_by("community"),
               Checklists = sum(r$Province %in% s$Province),
               `No checklist at all` = if (length(none)) paste(none, collapse = ", ") else "-",
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  if (is.null(out)) return(out)
  tot <- data.frame(Region = "Total", Provinces = sum(out$Provinces), `Health center n` = sum(out$`Health center n`),
                    `Session n` = sum(out$`Session n`), `Community n` = sum(out$`Community n`),
                    Checklists = sum(out$Checklists), `No checklist at all` = "", check.names = FALSE)
  out <- rbind(out, tot)
  for (p in unname(epi_pages)) out[[p]] <- out[[paste(p, "n")]] / out$Provinces
  rownames(out) <- NULL
  out
}

# "6 of 8 (75%)" text for a compliance cell
epi_compliance_label <- function(n, d) ifelse(d > 0, sprintf("%s of %s (%s)", n, d, .epi_pct(n / d)), "-")

# ---- District reporting (Naveed Saeed EPI deck) ---------------------------------
# Districts expected to report: all districts of the boundary file in the
# selected regions / provinces (joined to the checklists on District Code).
epi_district_scope <- function(regions = NULL, provinces = NULL) {
  g <- sf::st_drop_geometry(epi_geo()$districts)
  d <- data.frame(Region = g$region, Province = g$province, District = g$district, DCode = g$district_code,
                  stringsAsFactors = FALSE)
  if (length(regions)) d <- d[d$Region %in% regions, ]
  if (length(provinces)) d <- d[d$Province %in% provinces, ]
  d <- d[order(d$Region, d$Province, d$District), ]
  rownames(d) <- NULL
  d
}

# Region x month: districts with >= 1 checklist of any type. "Districts" = in
# scope; "Any month" = districts with a checklist in at least one of the months.
epi_district_region_month <- function(data, months, regions = NULL, provinces = NULL) {
  scope <- epi_district_scope(regions, provinces)
  if (!nrow(scope)) return(NULL)
  r <- epi_responses(data)
  r <- r[r$month %in% months & !is.na(r$DCode), , drop = FALSE]
  row_for <- function(s, lbl) {
    out <- data.frame(Region = lbl, Districts = nrow(s), check.names = FALSE, stringsAsFactors = FALSE)
    for (m in months) out[[epi_month_label(m)]] <- sum(s$DCode %in% r$DCode[r$month == m])
    out$`Any month` <- sum(s$DCode %in% r$DCode)
    out
  }
  out <- do.call(rbind, lapply(split(scope, scope$Region), function(s) row_for(s, s$Region[1])))
  rbind(out, row_for(scope, "Total"))
}

# Region summary of district reporting for one set of months (usually the report
# month): districts with >= 1 checklist of each type, and of any type.
# Denominator = districts of the region in the boundary file ("Districts").
# "No checklist at all" = provinces of the region with no checklist of any type.
epi_district_compliance_by_region <- function(data, months, regions = NULL, provinces = NULL) {
  scope <- epi_district_scope(regions, provinces)
  if (!nrow(scope)) return(NULL)
  r <- epi_responses(data)
  r <- r[r$month %in% months, , drop = FALSE]
  row_for <- function(s, lbl) {
    out <- data.frame(Region = lbl, Districts = nrow(s), check.names = FALSE, stringsAsFactors = FALSE)
    for (p in names(epi_pages)) out[[epi_pages[[p]]]] <- sum(s$DCode %in% r$DCode[r$page == p & !is.na(r$DCode)])
    out$`Any checklist` <- sum(s$DCode %in% r$DCode[!is.na(r$DCode)])
    out$Checklists <- sum(r$Province %in% s$Province)
    none <- setdiff(unique(s$Province), r$Province)
    out$`No checklist at all` <- if (lbl == "Total") "" else if (length(none)) paste(sort(none), collapse = ", ") else "-"
    out
  }
  out <- do.call(rbind, lapply(split(scope, scope$Region), function(s) row_for(s, s$Region[1])))
  out <- rbind(out, row_for(scope, "Total"))
  rownames(out) <- NULL
  out
}

# Per province: districts with no checklist (any type) in the report month
# ("*" = none in any of `months` either). all = FALSE leaves out provinces where
# every district reported.
epi_silent_districts <- function(data, ref, months, regions = NULL, provinces = NULL, all = FALSE) {
  scope <- epi_district_scope(regions, provinces)
  r <- epi_responses(data)
  in_ref <- scope$DCode %in% r$DCode[r$month %in% ref]
  in_any <- scope$DCode %in% r$DCode[r$month %in% months]
  out <- do.call(rbind, lapply(split(seq_len(nrow(scope)), paste(scope$Region, scope$Province, sep = "\r")), function(i) {
    miss <- i[!in_ref[i]]
    data.frame(Region = scope$Region[i[1]], Province = scope$Province[i[1]], Districts = length(i),
               Reporting = sum(in_ref[i]), `Not reporting` = length(miss), `None all period` = sum(!in_any[i]),
               Names = paste(paste0(scope$District[miss], ifelse(in_any[miss], "", "*")), collapse = ", "),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  if (is.null(out)) return(NULL)
  if (!all) out <- out[out$`Not reporting` > 0, , drop = FALSE]
  out <- out[order(out$Region, -out$`Not reporting` / out$Districts, out$Province), , drop = FALSE]
  rownames(out) <- NULL
  out
}


# Indicator x month matrix of % Yes (wide): Indicator + one column per month
epi_yn_month_matrix <- function(d, defs, months, flip = rep(FALSE, nrow(defs))) {
  d$month <- .epi_month_key(d)
  out <- data.frame(Indicator = defs$title, stringsAsFactors = FALSE)
  for (m in months) {
    s <- d[d$month %in% m, , drop = FALSE]
    v <- vapply(defs$key, function(k) epi_yn_overall(s, k)$pct, numeric(1))
    out[[epi_month_label(m)]] <- ifelse(flip, 1 - v, v)
  }
  out
}

# Marker x month coverage matrix (wide) + drop-out row
epi_coverage_month_matrix <- function(d, markers, months) {
  d$month <- .epi_month_key(d)
  out <- data.frame(Marker = c(markers, "Drop-out Penta-1 to 3"), stringsAsFactors = FALSE)
  for (m in months) {
    s <- d[d$month %in% m, , drop = FALSE]
    cv <- epi_coverage(s, markers)
    dr <- epi_dropout(s)
    out[[epi_month_label(m)]] <- c(cv$pct[match(markers, as.character(cv$marker))],
                                   if (nrow(dr)) dr$dropout[1] else NA_real_)
  }
  out
}
