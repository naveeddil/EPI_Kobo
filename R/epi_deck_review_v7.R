# epi_deck_review_v7.R ----------------------------------------------------
# Deck B v7 (EPI supportive-supervision deck, one deck): a manifest of named
# slide modules (main slides, then a short technical annex of tables). Each
# module adds exactly one slide. Slide numbers and every "see slide / annex"
# reference come from the manifest, so reordering or dropping a module
# renumbers the deck automatically.
#
# Principle: finding -> geographical evidence -> operational implication ->
# corrective action. The v6 tables (health-center, supervision, session and
# community by area) are replaced by grids of provincial maps, one map per
# category / indicator / antigen, with the national figure in the map title.
# Tables stay where exact values, denominators or owners matter (trends,
# tracker, annex). Calculation rules are the validated v6 ones (shared helpers
# below are used by every slide that shows the same figure).

# ---- Manifest ------------------------------------------------------------------
# id: stable module name. deck: main / annex. kicker / title: {ref} = report
# month, {ytd} = Jan - report month, {geo} = region / province. period: data
# the module gets (month = report month, ytd = Jan..report month).
# map_type: what the slide draws. v6: source slides in the v6 deck (29 slides).
epi_review_manifest <- function() {
  r <- function(id, part, deck, kicker, title, module, period, map_type, indicators, v6)
    data.frame(id = id, part = part, deck = deck, kicker = kicker, title = title, module = module, period = period,
               map_type = map_type, indicators = indicators, v6_source = v6, stringsAsFactors = FALSE)
  ov <- "Overview"; hc <- "Health-center checklist"; ss <- "Session checklist"; cm <- "Community checklist"
  yt <- "Year to date and actions"; an <- "Technical annex"
  m <- rbind(
    r("cover", ov, "main", "", "Routine EPI Supportive Supervision", ".rv_m_cover", "month", "none",
      "Period, scope, data date, contents", "1"),
    r("exec_findings", ov, "main", "Executive findings | {ref}", "Five operational findings and the decisions needed",
      ".rv_m_exec", "month", "none (KPI tiles + findings)",
      "Checklists, reporting, completion, mobile supervision, safety exceptions, Penta-3", "2-3"),
    r("reporting_presence", ov, "main", "All checklists | {ref}", "Reporting presence and district gaps", ".rv_m_reporting",
      "month", "district status (1 map)", "Districts with / without a checklist; districts by checklist type", "4-5"),
    r("div_health", hc, "main", "", "Health-center checklist", ".rv_m_divider", "month", "none (section divider)",
      "Assessments, districts and provinces reached; slides in the part", ""),
    r("implementation", hc, "main", "Health-center checklist | {ref}", "Outreach and mobile session completion",
      ".rv_m_implementation", "month", "province rate (2 maps)", "Planned, conducted, completion (outreach, mobile)", "6-7"),
    r("hc_thematic", hc, "main", "Health-center checklist | {ref}", "Health-center performance by province - {ref}",
      ".rv_m_hc_thematic", "month", "province score (7-map grid)",
      "Readiness, defaulter follow-up, vaccinator training, planning, cold chain, recording, proximity", "8, 25"),
    r("supervision", hc, "main", "Health-center checklist | {ref}", "Supervision indicators by province - {ref}",
      ".rv_m_supervision", "month", "province rate (4 maps + 4 tiles)",
      "PEMT/NGO outreach (quarterly), PEMT mobile (monthly), NGO mobile (quarterly)", "9-10, 26"),
    r("div_session", ss, "main", "", "Session checklist", ".rv_m_divider", "month", "none (section divider)",
      "Sessions observed, districts and provinces reached; slides in the part", ""),
    r("session_thematic", ss, "main", "Session checklist | {ref}", "Session quality by province - {ref}",
      ".rv_m_session_thematic", "month", "province category score (5-map grid)",
      "5 session categories; administration/safety and readiness questions", "11, 27"),
    # v0.8.0: the combined "priority safety exceptions" map (v6 12-13) is merged here: the 7 item
    # maps already show where each problem was; the 8th cell lists the sessions to follow up.
    r("safety_practice", ss, "main", "Session checklist | {ref}", "Vaccine-safety practices by province - {ref}",
      ".rv_m_safety_practice", "month", "province rate (7-map grid) + follow-up panel",
      "7 safety items (expired / VVM reverse-coded); sessions with a problem in 6 priority items; provinces to verify", "12-13, 28"),
    r("div_community", cm, "main", "", "Community checklist", ".rv_m_divider", "month", "none (section divider)",
      "Assessments, children checked, districts reached; slides in the part", ""),
    r("community_status", cm, "main", "Community checklist | {ref}", "Vaccination status of assessed children by province",
      ".rv_m_community", "month", "province vaccination status (8-map grid)",
      "8 antigens; P1-P3 difference; Measles-2 caveat", "14-15, 29"),
    r("community_reasons", cm, "main", "Community checklist | {ytd}", "Reasons reported during community assessments, {ytd}",
      ".rv_reasons", "ytd", "none (bar chart)", "8 reasons, assessment-level multi-select", "16"),
    r("div_ytd", yt, "main", "", "Year to date and actions", ".rv_m_divider", "ytd", "none (section divider)",
      "Checklists in the year; slides in the part", ""),
    r("ytd_activity", yt, "main", "All checklists | {ytd}", "Year-to-date overview and monthly checklist activity",
      ".rv_m_ytd_activity", "ytd", "none (stacked bars + KPI tiles)", "Checklists by month and type; districts reached", "17-18"),
    r("district_region_month", yt, "main", "All checklists | {ytd}", "District reporting presence by region and month",
      ".rv_district_region", "ytd", "none (heat table)", "Districts with >= 1 checklist / districts in region", "19"),
    r("hc_trends", yt, "main", "Health-center checklist | {ytd}", "Health-center thematic trends by month", ".rv_m_hc_trends", "ytd",
      "none (heat table)", "8 health-center categories by month", "20"),
    r("session_trends", yt, "main", "Session checklist | {ytd}", "Session thematic trends by month", ".rv_m_session_trends", "ytd",
      "none (heat table)", "5 session categories by month", "21"),
    r("community_trends", yt, "main", "Community checklist | {ytd}", "Vaccination-status trends by month", ".rv_cov_month", "ytd",
      "none (heat table)", "9 antigens + P1-P3 difference by month", "22"),
    r("action_tracker", yt, "main", "Accountability | {ref}", "Management decisions and action tracker", ".rv_m_tracker",
      "month", "none (table)", "Five priorities: geography, owner, task, target, closure evidence", "3, 23"),
    r("definitions", yt, "main", "Notes", "Definitions, sources, scoring and methods", ".rv_m_notes", "ytd", "none",
      "Definitions, thresholds, map classes", "24"),
    r("annex_hc_questions", an, "annex", "Health-center checklist", "Health-center questions by {geo} (exact values)",
      ".rv_m_annex_hc_questions", "month", "none (question x {geo} table)",
      "22 health-center questions with n/N", "8"),
    r("annex_safety_register", an, "annex", "Session checklist", "Safety exception register (every flagged session)",
      ".rv_m_annex_register", "month", "none (table)", "Province, district, facility, date, unfavourable items", "12-13"))
  m$order <- seq_len(nrow(m))
  m$annex_label <- ifelse(m$deck == "annex", paste0("A", cumsum(m$deck == "annex")), "")
  rownames(m) <- NULL
  m
}

# Before -> after slide mapping (v6 -> v7) and the duplication-removal log
epi_review_v6_mapping <- function(man = epi_review_manifest()) {
  v6 <- c("Cover", "At a glance", "Action points", "Districts reached by checklist (3 maps)", "Districts with a checklist (map)",
          "Implementation table", "Completion maps (outreach, mobile)", "Health-center thematic table", "Supervision table",
          "Mobile supervision maps", "Session thematic table", "Safety KPI table", "Safety exception map",
          "Vaccination status by region (table)", "Penta-3 map", "Reasons", "YTD key numbers", "Checklists per month",
          "Districts reporting by region and month", "Health-center trends", "Session trends", "Vaccination-status trends",
          "Action tracker", "Definitions", "Annex: 8 health-center thematic maps", "Annex: 4 supervision maps",
          "Annex: 5 session thematic maps", "Annex: 7 safety indicator maps", "Annex: 8 antigen maps")
  to <- lapply(seq_along(v6), function(i) {
    hit <- man$order[vapply(man$v6_source, function(s) {
      parts <- trimws(strsplit(s, ",")[[1]])
      any(vapply(parts, function(p) { b <- as.integer(strsplit(p, "-")[[1]]); i >= b[1] && i <= b[length(b)] }, logical(1)))
    }, logical(1))]
    if (length(hit)) paste(hit, collapse = ", ") else "removed"
  })
  data.frame(`v6 slide` = seq_along(v6), `v6 content` = v6, `v7 slide(s)` = unlist(to), check.names = FALSE)
}

# ---- Shared computations (one definition per figure) ----------------------------
.RV_SAFETY_KEYS <- c("expired", "vvm", "icepacks", "safety_box", "technique", "open_vial")
.RV_CRITICAL <- c("expired", "vvm")
.RV_CLS <- c("90%+", "75-89%", "Below 75%", "Low sample", "Not applicable", "No observations")
.RV_CLS_COL <- stats::setNames(c("#CDEBD3", "#FCE8B2", "#F8C9C4", "#AEB6C2", "#D4D8DE", "#EEF0F3"), .RV_CLS)
.RV_EXC <- c("No exception", "1-9%", "10%+", "Low sample", "No observations")
.RV_EXC_COL <- stats::setNames(c("#67b97a", "#f2bd4b", "#e8635c", "#AEB6C2", "#EEF0F3"), .RV_EXC)
.RV_MIN_N <- 3L; .RV_MIN_CHILD <- 10L
.RV_HC_MAPPED <- c("Staff and community readiness", "Defaulter follow-up")
.RV_HC_SUPERVISION <- c("Outreach supervision", "Mobile supervision")
.RV_SESSION_PRIORITY <- c("Administration and safety", "Session readiness")

.rv_log <- function(ctx, module, decision) {
  if (is.environment(ctx$log)) ctx$log$rows[[length(ctx$log$rows) + 1]] <-
    data.frame(module = module, decision = decision, stringsAsFactors = FALSE)
  invisible(NULL)
}

# "slide 6" / "annex A2, slide 19"; NULL when the module is not in the manifest
.rv_ref <- function(ctx, id) {
  man <- ctx$manifest; k <- match(id, man$id)
  if (is.null(man) || is.na(k)) return(NULL)
  if (man$deck[k] == "annex") sprintf("annex %s, slide %d", man$annex_label[k], man$order[k]) else sprintf("slide %d", man$order[k])
}
.rv_note <- function(...) { x <- unlist(list(...)); x <- x[!is.na(x) & nzchar(x)]; if (length(x)) paste(x, collapse = " | ") else NULL }

.rv_fav <- function(key, o) if (key %in% .RV_CRITICAL) o$No else o$Yes
.rv_nN <- function(n, N) sprintf("%s/%s", .epi_n(n), .epi_n(N))

# Facility-month completion = conducted / planned among records with planned > 0.
# One authoritative rule at every level (v0.7.0): a facility-month (District
# Code + facility + year + month) counts once - the first submitted record is
# kept - BEFORE splitting by region / province, so provinces and regions add up
# to the national total. Dropped duplicates are listed by .rv_completion_dups().
.rv_fm_key <- function(h) paste(h$DCode, h$Facility, h$Year, h$Month, sep = "|")
.rv_dedup_fm <- function(h) if (nrow(h)) h[!duplicated(.rv_fm_key(h)), , drop = FALSE] else h
.rv_completion_dups <- function(h) {
  k <- .rv_fm_key(h); dup <- k %in% k[duplicated(k)]
  x <- h[dup, intersect(c("Region", "Province", "District", "DCode", "Facility", "VisitDate", "kobo_id", "outreach_planned",
                          "outreach_conducted", "mobile_planned", "mobile_conducted"), names(h)), drop = FALSE]
  x$kept <- !duplicated(k[dup])
  x[order(k[dup], !x$kept), , drop = FALSE]
}
.rv_completion <- function(h, type, dedup = TRUE) {
  if (dedup) h <- .rv_dedup_fm(h)
  p <- h[[paste0(type, "_planned")]]; c <- h[[paste0(type, "_conducted")]]
  if (is.null(p)) p <- c <- numeric(0)
  ok <- !is.na(p) & !is.na(c) & p > 0
  list(records = nrow(h), applicable = sum(ok), planned = sum(p[ok]), conducted = sum(c[ok]),
       rate = if (sum(p[ok]) > 0) sum(c[ok]) / sum(p[ok]) else NA_real_,
       zero = sum(!is.na(p) & p == 0), over = sum(ok & c > p))
}
.rv_completion_by <- function(h, type, by = "Province") {
  h <- .rv_dedup_fm(h)
  h <- h[!is.na(h[[by]]), , drop = FALSE]
  if (!nrow(h)) return(data.frame(group = character(), records = integer(), applicable = integer(), planned = numeric(),
                                  conducted = numeric(), rate = numeric(), over = integer()))
  do.call(rbind, lapply(split(h, h[[by]]), function(x) {
    o <- .rv_completion(x, type)
    data.frame(group = x[[by]][1], records = o$records, applicable = o$applicable, planned = o$planned,
               conducted = o$conducted, rate = o$rate, over = o$over, stringsAsFactors = FALSE)
  }))
}

# Priority safety exceptions: a session counts once when any of the six items is
# unfavourable (expired / VVM = Yes, the others = No); valid = >= 1 item answered.
.rv_safety <- function(s) {
  if (!nrow(s)) return(list(bad = matrix(NA, 0, length(.RV_SAFETY_KEYS), dimnames = list(NULL, .RV_SAFETY_KEYS)),
                            valid = logical(), exception = logical(), N = 0L, n = 0L))
  bad <- do.call(cbind, lapply(.RV_SAFETY_KEYS, function(k) if (k %in% .RV_CRITICAL) s[[k]] == "Yes" else s[[k]] == "No"))
  colnames(bad) <- .RV_SAFETY_KEYS
  valid <- rowSums(!is.na(bad)) > 0
  exception <- rowSums(bad, na.rm = TRUE) > 0 & valid
  list(bad = bad, valid = valid, exception = exception, N = sum(valid), n = sum(exception))
}
.rv_safety_province <- function(s) {
  f <- .rv_safety(s)
  ok <- !is.na(s$Province)
  if (!any(ok)) return(data.frame(Province = character(), n = integer(), N = integer(), Rate = numeric()))
  z <- do.call(rbind, lapply(split(which(ok), s$Province[ok]), function(ix)
    data.frame(Province = s$Province[ix[1]], n = sum(f$exception[ix]), N = sum(f$valid[ix]), stringsAsFactors = FALSE)))
  z$Rate <- ifelse(z$N > 0, z$n / z$N, NA_real_)
  rownames(z) <- NULL
  z
}
# Eligible priorities: >= 3 assessed sessions and >= 1 exception, most exceptions first
.rv_safety_priority <- function(s) {
  z <- .rv_safety_province(s)
  list(eligible = { e <- z[z$N >= .RV_MIN_N & z$n > 0, ]; e[order(-e$n, -e$Rate), ] },
       low = { l <- z[z$N < .RV_MIN_N & z$n > 0, ]; l[order(-l$n, l$Province), ] })
}

# Province frames for maps: province (all of the boundary file), observed (any
# record of that checklist), n (denominator used for the sample rule), value.
.rv_provs <- function() epi_geo()$provinces$province
.rv_pv_indicator <- function(d, key) {
  prov <- .rv_provs(); app <- .rv_applicable(d, key)
  z <- epi_yn_by(app, key, "Province"); i <- match(prov, z$group)
  flip <- key %in% .RV_CRITICAL
  data.frame(province = prov, observed = prov %in% d$Province, n = ifelse(is.na(i), 0, z$Total[i]),
             fav = if (flip) z$No[i] else z$Yes[i], value = if (flip) 1 - z$pct[i] else z$pct[i], stringsAsFactors = FALSE)
}
.rv_pv_category <- function(d, keys) {
  prov <- .rv_provs(); n <- table(d$Province)
  v <- vapply(prov, function(p) {
    x <- d[!is.na(d$Province) & d$Province == p, , drop = FALSE]
    if (!nrow(x)) return(NA_real_)
    q <- vapply(keys, function(k) .rv_value(.rv_overall(x, k)$pct, k %in% .RV_CRITICAL), numeric(1))
    if (all(is.na(q))) NA_real_ else mean(q, na.rm = TRUE)
  }, numeric(1))
  nn <- as.numeric(n[prov]); nn[is.na(nn)] <- 0
  data.frame(province = prov, observed = prov %in% d$Province, n = nn, value = unname(v), stringsAsFactors = FALSE)
}
.rv_pv_completion <- function(h, type) {
  prov <- .rv_provs(); z <- .rv_completion_by(h, type, "Province"); i <- match(prov, z$group)
  data.frame(province = prov, observed = prov %in% h$Province, n = ifelse(is.na(i), 0, z$applicable[i]),
             value = z$rate[i], over = ifelse(is.na(i), 0, z$over[i]), stringsAsFactors = FALSE)
}
.rv_pv_coverage <- function(cm, marker) {
  prov <- .rv_provs(); cv <- epi_coverage(cm, marker, by = "Province"); i <- match(prov, cv$group)
  data.frame(province = prov, observed = prov %in% cm$Province, n = ifelse(is.na(i), 0, cv$children[i]),
             fav = cv$vaccinated[i], value = cv$pct[i], stringsAsFactors = FALSE)
}

# Class of each province: valid data (RAG band), low sample, not applicable
# (records but no applicable answer), no observations (no record).
.rv_classify <- function(pv, min_n = .RV_MIN_N) {
  ifelse(!pv$observed, "No observations",
         ifelse(is.na(pv$n) | pv$n == 0 | is.na(pv$value), "Not applicable",
                ifelse(pv$n < min_n, "Low sample",
                       ifelse(pv$value >= .RV_GOOD, "90%+", ifelse(pv$value >= .RV_WARN, "75-89%", "Below 75%")))))
}
.rv_valid <- function(pv, min_n = .RV_MIN_N) .rv_classify(pv, min_n) %in% .RV_CLS[1:3]

# Lowest valid provinces below 90%, "Kandahar 46%, Uruzgan 51%"
.rv_lowest <- function(pv, min_n = .RV_MIN_N, k = 4, nN = FALSE) {
  x <- pv[.rv_valid(pv, min_n) & pv$value < .RV_GOOD, , drop = FALSE]
  if (!nrow(x)) return(sprintf("none below 90%% (provinces with >= %d)", min_n))
  x <- x[order(x$value), , drop = FALSE]
  lab <- if (nN) sprintf("%s %s (%s)", x$province, .rv_nN(x$fav, x$n), .epi_pct(x$value)) else sprintf("%s %s", x$province, .epi_pct(x$value))
  .rv_list(lab, k)
}
.rv_list <- function(x, max = 6) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) > max) paste0(paste(x[seq_len(max)], collapse = ", "), sprintf(" + %s more", length(x) - max))
  else paste(x, collapse = ", ")
}

.rv_prov_map <- function(ctx, pv, min_n = .RV_MIN_N, label = c("all", "flag", "red", "none"), mark = NULL, text_size = 1.9,
                         base_size = 9) {
  label <- match.arg(label)
  if (length(ctx$regions) == 1 && label %in% c("flag", "red")) label <- "all"   # zoomed to one region: room for every label
  st <- .rv_classify(pv, min_n); valid <- st %in% .RV_CLS[1:3]
  show <- switch(label, all = valid, flag = valid & pv$value < .RV_GOOD, red = valid & pv$value < .RV_WARN,
                 none = rep(FALSE, nrow(pv)))
  nm <- ifelse(pv$province %in% mark, paste0(pv$province, "*"), pv$province)
  text <- ifelse(show, paste0(nm, "\n", .epi_pct(pv$value)), ifelse(pv$province %in% mark, nm, ""))
  epi_map_status(st, .RV_CLS_COL, "province", text = text, focus_regions = ctx$regions, base_size = base_size,
                 legend = FALSE, text_size = text_size)
}
.rv_put_map <- function(pres, plot, l, t, w, h, dpi = 250) {
  png <- tempfile(fileext = ".png")
  epi_save_map(plot, png, w, h, dpi = dpi)
  .epi_at(pres, officer::external_img(png, width = w, height = h), l, t, w, h)
}
.rv_cls_legend <- function(pres, l, t, low = "<3", w = 1.3, size = 8) {
  labs <- c("90%+", "75-89%", "Below 75%", sprintf("Low n (%s)", low), "Not applicable", "No obs")
  for (i in seq_along(labs))
    pres <- .rv_text(pres, labs[i], l + (i - 1) * (w + 0.05), t, w, 0.26, size = size, align = "center", bg = .RV_CLS_COL[[i]])
  pres
}
.rv_accent <- function(v) if (is.na(v)) .RV$blue else if (v >= .RV_GOOD) "#67b97a" else if (v >= .RV_WARN) "#f2bd4b" else "#e8635c"

# Compact KPI tile: value, bold label, optional muted sub-line
.rv_ktile <- function(pres, l, t, w, h, value, label, sub = NULL, accent = .RV$blue, vsize = 20) {
  pres <- .rv_text(pres, "", l, t, w, h, bg = .RV$tile)
  pres <- .rv_text(pres, "", l, t, 0.06, h, bg = accent)
  vh <- vsize / 72 + 0.12
  pres <- .rv_text(pres, value, l + 0.14, t + 0.02, w - 0.2, vh, size = vsize, bold = TRUE, color = .RV$navy)
  pres <- .rv_text(pres, label, l + 0.14, t + vh, w - 0.2, 0.26, size = 10, bold = TRUE, color = .RV$ink)
  if (!is.null(sub)) pres <- .rv_text(pres, sub, l + 0.14, t + vh + 0.23, w - 0.2, 0.26, size = 8.5, color = .RV$muted)
  pres
}
# Small KPI tile for a strip: value on the left, label and sub-line beside it
.rv_mtile <- function(pres, l, t, w, h, value, label, sub = NULL, accent = .RV$blue) {
  pres <- .rv_text(pres, "", l, t, w, h, bg = .RV$tile)
  pres <- .rv_text(pres, "", l, t, 0.06, h, bg = accent)
  pres <- .rv_text(pres, value, l + 0.1, t + 0.1, 0.92, 0.42, size = 16, bold = TRUE, color = .RV$navy)
  pres <- .rv_text(pres, label, l + 1.02, t + 0.03, w - 1.07, 0.28, size = 9.5, bold = TRUE, color = .RV$ink)
  if (!is.null(sub)) pres <- .rv_text(pres, sub, l + 1.02, t + 0.29, w - 1.07, h - 0.3, size = 8, color = .RV$muted)
  pres
}
# One paragraph of runs: list(list("text", bold = TRUE, color = ...), ...)
.rv_runs <- function(pres, runs, l, t, w, h, size = 11, align = "left") {
  ft <- lapply(runs, function(r) officer::ftext(r[[1]], epi_fp(size, r$color %||% .RV$ink, isTRUE(r$bold), isTRUE(r$italic))))
  par <- do.call(officer::fpar, c(ft, list(fp_p = officer::fp_par(text.align = align, padding = 2, line_spacing = 1.05))))
  officer::ph_with(pres, officer::block_list(par), location = officer::ph_location(left = l, top = t, width = w, height = h))
}
# Header over a map: "Name  86%" + muted sub-line
.rv_map_head <- function(pres, l, t, w, name, value, sub = NULL, size = 13) {
  pres <- .rv_runs(pres, list(list(name, bold = TRUE, color = .RV$navy),
                              list(if (nzchar(value)) paste0("   ", value) else "", bold = TRUE, color = .RV$blue)),
                   l, t, w, 0.32, size = size)
  if (!is.null(sub)) pres <- .rv_text(pres, sub, l, t + 0.3, w, 0.27, size = 9, color = .RV$muted)
  pres
}

# Reporting status of the districts in scope (report month, Jan..report month)
.rv_reporting <- function(ctx) {
  scope <- epi_district_scope(ctx$regions, ctx$provinces)
  r <- epi_responses(ctx$ytd$data)
  dc <- scope$DCode
  in_ref <- dc %in% r$DCode[r$month %in% ctx$ref]; in_any <- dc %in% r$DCode
  by_type <- data.frame(Checklist = unname(epi_pages), stringsAsFactors = FALSE)
  by_type$Districts <- vapply(names(epi_pages), function(p) sum(dc %in% ctx$data[[p]]$DCode), numeric(1))
  by_type$Checklists <- vapply(names(epi_pages), function(p) nrow(ctx$data[[p]]), numeric(1))
  pscope <- epi_province_scope(ctx$regions, ctx$provinces)
  prov_none <- pscope$Province[!pscope$Province %in% epi_responses(ctx$data)$Province]
  silent <- epi_silent_districts(ctx$ytd$data, ctx$ref, ctx$ytd$months, ctx$regions, ctx$provinces)
  if (!is.null(silent) && nrow(silent)) silent <- silent[order(-silent$`Not reporting`, -silent$`None all period`), ]
  list(scope = scope, status = ifelse(in_ref, 1L, ifelse(in_any, 2L, 3L)), nd = length(dc),
       n = c(sum(in_ref), sum(!in_ref & in_any), sum(!in_any)), by_type = by_type,
       prov_none = prov_none, nprov = nrow(pscope), silent = silent)
}

# ---- Main-deck modules --------------------------------------------------------------
.rv_m_cover <- function(pres, ctx) .rv_cover(pres, ctx)

# ---- Part dividers: one per checklist (and the year-to-date part) --------------------
.RV_PART_COL <- c(`Overview` = "#9FB3C8", `Health-center checklist` = "#2a78d6", `Session checklist` = "#eb6834",
                  `Community checklist` = "#1baf7a", `Year to date and actions` = "#00A6D6", `Technical annex` = "#6B7785")
.RV_PART_TEXT <- c(
  `Health-center checklist` = "Facility visit: planning, staff and community readiness, cold chain, recording, defaulter follow-up, implementation of planned outreach / mobile sessions and their supervision.",
  `Session checklist` = "Observed immunization session (fixed, outreach, mobile): readiness, vaccine management, administration and safety, recording and caregiver communication.",
  `Community checklist` = "Children 12-23 months checked in the community around the facility: recorded vaccination status and reasons for non-vaccination.",
  `Year to date and actions` = "January to the report month across all three checklists: reporting, monthly trends, then the action tracker and definitions.")
# "5-7" for the main slides of a part
.rv_part_range <- function(man, part) { o <- man$order[man$part == part]; if (length(o) > 1) sprintf("%s-%s", min(o), max(o)) else as.character(o) }

.rv_m_divider <- function(pres, ctx) {
  ctx$page_no$n <- ctx$page_no$n + 1
  man <- ctx$manifest; part <- ctx$mod$part; col <- .RV_PART_COL[[part]]
  pres <- officer::add_slide(pres, layout = "Blank", master = "Office Theme")
  pres <- .rv_text(pres, "", 0, 0, 4.6, 7.5, bg = .RV$navy)
  pres <- .rv_text(pres, "", 0, 0, 0.18, 7.5, bg = col)
  k <- match(part, unique(man$part[man$deck == "main"]))
  pres <- .rv_text(pres, sprintf("PART %s", k), 0.6, 2.5, 3.6, 0.45, size = 16, bold = TRUE, color = col)
  pres <- .rv_text(pres, if (part == "Year to date and actions") ctx$ytd_label else ctx$ref_label, 0.6, 2.95, 3.8, 0.5,
                   size = 18, color = "#D6E4F0")
  page <- c(`Health-center checklist` = "health", `Session checklist` = "session", `Community checklist` = "community")[part]
  stats <- if (!is.na(page)) {
    d <- ctx$data[[page]]; scope <- epi_district_scope(ctx$regions, ctx$provinces)
    x <- c(sprintf("%s %s", .epi_n(nrow(d)), c(health = "health-center assessments", session = "sessions observed",
                                                 community = "community assessments")[[page]]),
           sprintf("%s of %s districts", sum(scope$DCode %in% d$DCode), nrow(scope)),
           sprintf("%s provinces", length(unique(d$Province[!is.na(d$Province)]))))
    if (page == "community") x[3] <- sprintf("%s children 12-23 m checked", .epi_n(sum(d$children[d$children > 0], na.rm = TRUE)))
    x
  } else sprintf("%s checklists", .epi_n(sum(vapply(ctx$data, nrow, integer(1)))))
  for (i in seq_along(stats)) pres <- .rv_text(pres, stats[i], 0.6, 3.75 + (i - 1) * 0.42, 3.8, 0.4, size = 13, color = "#FFFFFF")
  pres <- .rv_text(pres, part, 5.2, 1.2, 7.9, 0.9, size = 36, bold = TRUE, color = .RV$navy)
  pres <- .rv_text(pres, "", 5.3, 2.12, 0.9, 0.07, bg = col)
  pres <- .rv_text(pres, .RV_PART_TEXT[[part]], 5.2, 2.3, 7.6, 0.85, size = 13, color = .RV$muted)
  sl <- man[man$part == part & man$module != ".rv_m_divider", ]
  pres <- .rv_text(pres, "In this part", 5.2, 3.35, 7.6, 0.35, size = 12, bold = TRUE, color = col)
  for (i in seq_len(nrow(sl))) {
    t <- 3.75 + (i - 1) * 0.42
    pres <- .rv_text(pres, as.character(sl$order[i]), 5.2, t, 0.6, 0.38, size = 14, bold = TRUE, color = col)
    pres <- .rv_text(pres, sl$title[i], 5.8, t, 7.1, 0.38, size = 14, color = .RV$ink)
  }
  .rv_text(pres, as.character(ctx$page_no$n), 12.33, 7.08, 0.5, 0.28, size = 9, color = .RV$muted, align = "right")
}


.rv_m_exec <- function(pres, ctx) {
  pres <- .rv_slide(pres, ctx, "", "", note = "Findings condensed from the evidence slides; detailed owners and closure evidence on the action tracker")
  dat <- ctx$data; prev <- ctx$data_prev
  n <- vapply(dat, nrow, integer(1)); np <- vapply(prev, nrow, integer(1))
  if (!sum(n)) return(epi_no_data(pres))
  rp <- .rv_reporting(ctx)
  tiles <- list(
    list(.epi_n(sum(n)), "Checklists", .rv_delta(sum(n), sum(np))),
    list(.epi_n(n[["health"]]), "Health-center visits", .rv_delta(n[["health"]], np[["health"]])),
    list(.epi_n(n[["session"]]), "Session visits", .rv_delta(n[["session"]], np[["session"]])),
    list(.epi_n(n[["community"]]), "Community assessments", .rv_delta(n[["community"]], np[["community"]])),
    list(sprintf("%s / %s", rp$nprov - length(rp$prov_none), rp$nprov), "Provinces reporting", "at least one checklist"))
  for (i in seq_along(tiles))
    pres <- .rv_ktile(pres, 0.5 + (i - 1) * 2.49, 1.4, 2.35, 1.0, tiles[[i]][[1]], tiles[[i]][[2]], tiles[[i]][[3]],
                      accent = if (i == 5 && length(rp$prov_none)) "#e34948" else .RV$blue, vsize = 22)
  f <- .rv_exec_findings(ctx, rp)
  pres <- .rv_text(pres, "Finding", 2.2, 2.6, 6.6, 0.3, size = 10, bold = TRUE, color = .RV$muted)
  pres <- .rv_text(pres, "Decision needed", 8.95, 2.6, 3.0, 0.3, size = 10, bold = TRUE, color = .RV$muted)
  pres <- .rv_text(pres, "Evidence", 11.95, 2.6, 0.9, 0.3, size = 10, bold = TRUE, color = .RV$muted)
  cols <- c(Reporting = "#e34948", Implementation = .RV$blue, Supervision = "#6250d6", `Vaccine safety` = "#eb6834",
            Community = "#1baf7a")
  for (i in seq_len(nrow(f))) {
    t <- 2.92 + (i - 1) * 0.8
    pres <- .rv_text(pres, "", 0.5, t, 12.35, 0.74, bg = if (i %% 2) "#F6F8FA" else "#FFFFFF")
    pres <- .rv_text(pres, f$area[i], 0.5, t, 1.6, 0.74, size = 11, bold = TRUE, color = "#FFFFFF", bg = cols[[f$area[i]]])
    pres <- .rv_text(pres, f$text[i], 2.2, t + 0.02, 6.6, 0.72, size = 10.5, color = .RV$ink)
    pres <- .rv_text(pres, f$decision[i], 8.95, t + 0.02, 2.95, 0.72, size = 10, color = .RV$navy, bold = TRUE)
    pres <- .rv_text(pres, f$ref[i], 11.95, t + 0.02, 0.9, 0.72, size = 10, color = .RV$blue)
  }
  pres
}

.rv_exec_findings <- function(ctx, rp = .rv_reporting(ctx)) {
  h <- ctx$data$health; s <- ctx$data$session; cm <- ctx$data$community
  ml <- month.abb[as.integer(substr(ctx$ref, 6, 7))]
  ref <- function(id) { x <- .rv_ref(ctx, id); if (is.null(x)) "" else sub("^slide", "Slide", x) }
  out <- data.frame(area = character(), text = character(), decision = character(), ref = character(), stringsAsFactors = FALSE)
  add <- function(a, t, dcs, id) out[nrow(out) + 1, ] <<- c(a, t, dcs, ref(id))
  add("Reporting", sprintf("%s of %s districts (%s) sent a checklist in %s; %s had none this month and %s none since January.%s",
                           rp$n[1], rp$nd, .epi_pct(rp$n[1] / max(rp$nd, 1)), ml, rp$n[2], rp$n[3],
                           if (length(rp$prov_none)) sprintf(" No checklist from %s province(s): %s.", length(rp$prov_none),
                                                             .rv_list(rp$prov_none, 4)) else ""),
      "Reconcile submissions with visit plans", "reporting_presence")
  if (nrow(h)) {
    o <- .rv_completion(h, "outreach", dedup = TRUE); m <- .rv_completion(h, "mobile", dedup = TRUE)
    add("Implementation", sprintf("Outreach completion %s (%s of %s planned sessions), mobile %s (%s of %s). Lowest provinces (outreach): %s.",
                                  .epi_pct(o$rate), .epi_n(o$conducted), .epi_n(o$planned), .epi_pct(m$rate), .epi_n(m$conducted),
                                  .epi_n(m$planned), .rv_lowest(.rv_pv_completion(h, "outreach"), k = 2)),
        "Verify plans, access and over-plan records", "implementation")
    pm <- .rv_overall(h, "pemt_mobile_supervision"); ng <- .rv_overall(h, "ngo_mobile_supervision")
    po <- .rv_overall(h, "pemt_supervision"); no <- .rv_overall(h, "ngo_supervision")
    add("Supervision", sprintf("Mobile supervision where mobile sessions were planned: PEMT %s (%s, monthly), NGO %s (%s, quarterly). Outreach supervision: PEMT %s, NGO %s.",
                               .rv_nN(pm$Yes, pm$Total), .epi_pct(pm$pct), .rv_nN(ng$Yes, ng$Total), .epi_pct(ng$pct),
                               .epi_pct(po$pct), .epi_pct(no$pct)),
        "Check mobile plans and supervision logbooks", "supervision")
  }
  if (nrow(s)) {
    sf <- .rv_safety(s); pr <- .rv_safety_priority(s)
    add("Vaccine safety", sprintf("%s of %s assessed sessions (%s) had a priority safety exception: %s expired-vaccine and %s VVM stage 3/4 observation(s). Priority provinces: %s.",
                                  sf$n, sf$N, .epi_pct(if (sf$N) sf$n / sf$N else NA), sum(s$expired == "Yes", na.rm = TRUE),
                                  sum(s$vvm == "Yes", na.rm = TRUE),
                                  if (nrow(pr$eligible)) .rv_list(sprintf("%s %s", pr$eligible$Province, .rv_nN(pr$eligible$n, pr$eligible$N)), 3)
                                  else "none with 3+ sessions"),
        "Trace flagged sessions; confirm vaccine disposition", "safety_practice")
  }
  if (nrow(cm)) {
    cv <- epi_coverage(cm, c("Penta-3", "Measles-1")); pv <- .rv_pv_coverage(cm, "Penta-3")
    ok <- .rv_valid(pv, .RV_MIN_CHILD); low <- pv[ok, ][which.min(pv$value[ok]), ]
    add("Community", sprintf("Penta-3 recorded for %s of %s assessed children (%s), Measles-1 %s; not population coverage. Lowest province (10+ children): %s.",
                             .epi_n(cv$vaccinated[1]), .epi_n(cv$children[1]), .epi_pct(cv$pct[1]), .epi_pct(cv$pct[2]),
                             if (nrow(low)) sprintf("%s %s (%s)", low$province, .rv_nN(low$fav, low$n), .epi_pct(low$value)) else "none"),
        "Validate child lists and due-dose follow-up", "community_status")
  }
  out
}

.rv_m_reporting <- function(pres, ctx) {
  rp <- .rv_reporting(ctx)
  pres <- .rv_slide(pres, ctx, "", "", note = "Any checklist type, joined on District Code; presence is not compliance with a supervision plan")
  mon <- month.abb[as.integer(substr(ctx$ref, 6, 7))]
  lv <- c(sprintf("Checklist in %s", mon), "None this month (some earlier this year)", "None since January")
  g <- epi_geo(); st <- rep(NA_character_, nrow(g$districts))
  st[match(rp$scope$DCode, g$districts$district_code)] <- lv[rp$status]
  st[is.na(st)] <- lv[3]
  cols <- stats::setNames(c("#86b6ef", "#f9d77e", "#e8635c"), lv)
  pres <- .rv_put_map(pres, epi_map_status(st, cols, "district", focus_regions = ctx$regions, base_size = 10),
                      0.35, 1.35, 8.0, 5.55, dpi = 300)
  acc <- c("#2a78d6", "#eda100", "#e34948")
  for (i in 1:3) pres <- .rv_ktile(pres, 8.6, 1.42 + (i - 1) * 0.78, 4.25, 0.72,
                                   sprintf("%s  (%s)", rp$n[i], .epi_pct(rp$n[i] / max(rp$nd, 1))), lv[i], accent = acc[i], vsize = 18)
  pres <- .rv_text(pres, "Districts reached by checklist type", 8.6, 3.85, 4.25, 0.32, size = 12, bold = TRUE, color = .RV$navy)
  bt <- rp$by_type
  bt$`% of districts` <- bt$Districts / max(rp$nd, 1)
  ft <- .rv_heat_ft(bt[, c("Checklist", "Districts", "% of districts", "Checklists")], character(), font_size = 10)
  ft <- flextable::set_formatter(ft, `% of districts` = function(x) .epi_pct(x))
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::width(ft, j = 1:4, width = c(1.35, 0.9, 1.1, 0.9))
  ft <- flextable::height_all(ft, height = 0.27, part = "all")
  pres <- .epi_at(pres, ft, 8.6, 4.2, 4.25, 1.2)
  pres <- .rv_text(pres, "Largest gaps this month", 8.6, 5.42, 4.25, 0.3, size = 12, bold = TRUE, color = .RV$navy)
  gaps <- if (!is.null(rp$silent) && nrow(rp$silent)) {
    s <- head(rp$silent, 3)
    sprintf("%s: %s of %s districts%s", s$Province, s$`Not reporting`, s$Districts,
            ifelse(s$`None all period` > 0, sprintf(" (%s none since Jan)", s$`None all period`), ""))
  } else "Every district in scope reported"
  if (length(rp$prov_none)) gaps <- c(gaps, sprintf("No checklist at all: %s", .rv_list(rp$prov_none, 5)))
  .rv_bullets(pres, gaps, 8.6, 5.72, 4.25, 1.3, size = 10)
}

.rv_m_implementation <- function(pres, ctx) {
  h <- ctx$data$health
  pres <- .rv_slide(pres, ctx, "", "", note = "Completion = conducted / planned, facility-month records with planned > 0; a facility-month counts once at every level")
  if (!nrow(h)) return(epi_no_data(pres))
  for (i in 1:2) {
    type <- c("outreach", "mobile")[i]; l <- c(0.45, 6.88)[i]
    o <- .rv_completion(h, type, dedup = TRUE)
    pres <- .rv_ktile(pres, l, 1.38, 6.0, 0.98, sprintf("%s  completion", .epi_pct(o$rate)),
                      sprintf("%s: %s conducted of %s planned sessions", tools::toTitleCase(type), .epi_n(o$conducted), .epi_n(o$planned)),
                      sprintf("%s facility-months with planned sessions | %s with none planned | %s conducted > planned (verify)",
                              .epi_n(o$applicable), .epi_n(o$zero), .epi_n(o$over)), accent = .rv_accent(o$rate), vsize = 20)
    pv <- .rv_pv_completion(h, type)
    pres <- .rv_put_map(pres, .rv_prov_map(ctx, pv, label = "all", text_size = 2.0), l, 2.45, 6.0, 3.75)
    lowtxt <- sprintf("Below 90%% (3+ records): %s. Low sample: %s province(s).", .rv_lowest(pv, k = 5),
                      sum(.rv_classify(pv) == "Low sample"))
    pres <- .rv_text(pres, lowtxt, l, 6.22, 6.0, 0.42, size = 9, color = .RV$ink)
    .rv_log(ctx, "implementation", sprintf("%s map: %s valid provinces; %s", type, sum(.rv_valid(pv)), lowtxt))
  }
  pres <- .rv_text(pres, "Duplicate facility-months counted once at every level.", 0.5, 6.72, 4.2, 0.26,
                   size = 8, italic = TRUE, color = .RV$muted)
  .rv_cls_legend(pres, 4.75, 6.72)
}

# ---- Map grids (replace the v6 tables: one small provincial map per category / indicator) ----
# Cell k (1-based, row-wise) of an ncol x nrow grid inside [left, left + width] x [top, bottom]
.rv_cell <- function(k, ncol, nrow, top = 1.36, bottom = 6.62, left = 0.42, width = 12.5, gap = 0.12) {
  cw <- (width - gap * (ncol - 1)) / ncol; ch <- (bottom - top) / nrow
  list(l = left + ((k - 1) %% ncol) * (cw + gap), t = top + ((k - 1) %/% ncol) * ch, w = cw, h = ch)
}
# panels: list of list(name, value, sub, pv, min_n, mark, text). Title + muted sub-line over each
# map; provinces below 75% are labelled unless `text` gives the labels.
.rv_grid <- function(pres, ctx, panels, ncol, nrow = ceiling(length(panels) / ncol), top = 1.36, bottom = 6.62,
                     text_size = 1.6) {
  for (i in seq_along(panels)) {
    p <- panels[[i]]; c <- .rv_cell(i, ncol, nrow, top, bottom)
    pres <- .rv_runs(pres, list(list(p$name, bold = TRUE, color = .RV$navy),
                                list(if (length(p$value) && nzchar(p$value)) paste0("  ", p$value) else "", bold = TRUE, color = .RV$blue)),
                     c$l, c$t, c$w, 0.3, size = 10.5, align = "center")
    if (!is.null(p$sub)) pres <- .rv_text(pres, p$sub, c$l, c$t + 0.27, c$w, 0.24, size = 8, color = .RV$muted, align = "center")
    mn <- p$min_n %||% .RV_MIN_N
    plot <- if (!is.null(p$text)) epi_map_status(.rv_classify(p$pv, mn), .RV_CLS_COL, "province", text = p$text,
                                                 focus_regions = ctx$regions, base_size = 6, legend = FALSE, text_size = text_size)
            else .rv_prov_map(ctx, p$pv, min_n = mn, label = p$label %||% "red", mark = p$mark, text_size = text_size, base_size = 6)
    pres <- .rv_put_map(pres, plot, c$l, c$t + 0.52, c$w, c$h - 0.56, dpi = 220)
  }
  pres
}
.rv_grid_legend <- function(pres, low = "<3", note = NULL) {
  if (!is.null(note)) pres <- .rv_text(pres, note, 0.5, 6.7, 6.1, 0.32, size = 8.5, italic = TRUE, color = .RV$muted)
  .rv_cls_legend(pres, 6.75, 6.72, low = low, w = 0.97, size = 7.5)
}

.rv_m_hc_thematic <- function(pres, ctx) {
  d <- ctx$data$health
  ann <- .rv_ref(ctx, "annex_hc_questions")
  pres <- .rv_slide(pres, ctx, "", "", note = .rv_note("Category = equal-weight mean of applicable question rates; provinces need 3+ assessments",
                                                       if (!is.null(ann)) paste("Question values:", ann)))
  if (!nrow(d)) return(epi_no_data(pres))
  # Supervision categories are left out here: slide "supervision" maps the four indicators.
  cats <- c("Staff and community readiness", "Defaulter follow-up", "Planning and microplanning", "Cold-chain management",
            "Recording and reporting", "Outreach proximity (context)")
  tab <- .rv_category_table(d, .rv_hc_categories, ctx$geo_level); gl <- tolower(ctx$geo_level)
  cat_panel <- function(nm) {
    tr <- tab[tab$Category == nm, ]
    list(name = nm, value = .epi_pct(tr$National), sub = sprintf("national | lowest %s: %s %s", gl, tr$`Lowest area`, .epi_pct(tr$`Lowest score`)),
         pv = .rv_pv_category(d, .rv_hc_categories[[nm]]))
  }
  tr_o <- .rv_overall(d, "trained"); tr_g <- epi_yn_by(d, "trained", ctx$geo_level); tr_g <- tr_g[order(tr_g$pct), ][1, ]
  trained <- list(name = "Vaccinator trained (12 m)", value = .epi_pct(tr_o$pct), pv = .rv_pv_indicator(d, "trained"),
                  sub = sprintf("%s | lowest %s: %s %s", .rv_nN(tr_o$Yes, tr_o$Total), gl, tr_g$group, .epi_pct(tr_g$pct)))
  panels <- c(lapply(cats[1:2], cat_panel), list(trained), lapply(cats[3:6], cat_panel))
  pres <- .rv_grid(pres, ctx, panels, 4)
  c8 <- .rv_cell(8, 4, 2)
  pres <- .rv_text(pres, "Reading this slide", c8$l, c8$t, c8$w, 0.3, size = 11, bold = TRUE, color = .RV$navy)
  pres <- .rv_bullets(pres, c("Top row = priorities: readiness, defaulter follow-up, vaccinator training.",
                              sprintf("Training = assessed facilities whose vaccinator was trained in the last 12 months (%s valid Yes/No answers; blanks excluded). Not a staff census.",
                                      .rv_nN(tr_o$Yes, tr_o$Total)),
                              paste0("Supervision is mapped by indicator on ", .rv_ref(ctx, "supervision") %||% "the supervision slide", ".")),
                      c8$l, c8$t + 0.32, c8$w, c8$h - 0.35, size = 8.5)
  .rv_log(ctx, "hc_thematic", sprintf("Vaccinator trained %s (%s); supervision categories not mapped here (indicator maps on the supervision slide).",
                                      .rv_nN(tr_o$Yes, tr_o$Total), .epi_pct(tr_o$pct)))
  .rv_grid_legend(pres, note = "Labels: provinces below 75%. Provinces need 3+ assessments to be coloured.")
}

.rv_m_supervision <- function(pres, ctx) {
  h <- ctx$data$health
  pres <- .rv_slide(pres, ctx, "", "", note = "Mobile rates: facilities with mobile sessions planned only; reference periods are not combined")
  if (!nrow(h)) return(epi_no_data(pres))
  keys <- c("pemt_supervision", "ngo_supervision", "pemt_mobile_supervision", "ngo_mobile_supervision")
  labs <- c("PEMT outreach", "NGO outreach", "PEMT mobile", "NGO mobile"); per <- c("quarterly", "quarterly", "monthly", "quarterly")
  reg <- .rv_matrix(h, epi_health_indicators[epi_health_indicators$key %in% keys, ], "Region")
  if (is.null(reg)) reg <- data.frame(key = character(), group = character(), pct = numeric(), Total = numeric())
  for (i in 1:4) {
    o <- .rv_overall(h, keys[i]); c <- .rv_cell(i, 4, 1, top = 1.38, bottom = 6.6)
    pres <- .rv_ktile(pres, c$l, 1.38, c$w, 0.95, .epi_pct(o$pct), sprintf("%s (%s)", labs[i], per[i]),
                      sprintf("%s %s", .rv_nN(o$Yes, o$Total), if (i > 2) "with mobile sessions planned" else "facilities answering"),
                      accent = .rv_accent(o$pct), vsize = 20)
    pv <- .rv_pv_indicator(h, keys[i])
    pres <- .rv_put_map(pres, .rv_prov_map(ctx, pv, label = "red", text_size = 1.7, base_size = 6), c$l, 2.5, c$w, 2.9, dpi = 220)
    r <- reg[reg$key == keys[i] & reg$Total > 0, ]; r <- r[order(r$pct), ][1, ]
    pres <- .rv_text(pres, if (!is.na(r$pct)) sprintf("Lowest region: %s %s (%s)", r$group, .rv_nN(round(r$pct * r$Total), r$Total), .epi_pct(r$pct)) else "",
                     c$l, 5.5, c$w, 0.26, size = 9, bold = TRUE, color = .RV$navy, align = "center")
    pres <- .rv_text(pres, sprintf("Provinces below 75%%: %s", sum(.rv_valid(pv) & pv$value < .RV_WARN)), c$l, 5.78, c$w, 0.26,
                     size = 8.5, color = .RV$muted, align = "center")
  }
  .rv_grid_legend(pres, note = "Labels: provinces below 75%. Not applicable (mobile) = no facility with mobile sessions planned.")
}

.rv_m_session_thematic <- function(pres, ctx) {
  s <- ctx$data$session
  pres <- .rv_slide(pres, ctx, "", "", note = .rv_note("Category = equal-weight mean of applicable question rates; expired / VVM reverse-coded",
                                                       if (!is.null(.rv_ref(ctx, "safety_practice"))) paste("* critical exception, see", .rv_ref(ctx, "safety_practice"))))
  if (!nrow(s)) return(epi_no_data(pres))
  cats <- .rv_session_categories; tab <- .rv_category_table(s, cats, ctx$geo_level); gl <- tolower(ctx$geo_level)
  crit <- unique(s$Province[!is.na(s$Province) & (s$expired %in% "Yes" | s$vvm %in% "Yes")])
  panels <- lapply(c("Administration and safety", "Session readiness", "Caregiver communication", "Vaccine management", "Recording"), function(nm) {
    tr <- tab[tab$Category == nm, ]
    list(name = nm, value = .epi_pct(tr$National), sub = sprintf("national | lowest %s: %s %s", gl, tr$`Lowest area`, .epi_pct(tr$`Lowest score`)),
         pv = .rv_pv_category(s, cats[[nm]]), mark = crit, label = "flag")
  })
  pres <- .rv_grid(pres, ctx, panels, 3, nrow = 2, text_size = 1.8)
  c <- .rv_cell(6, 3, 2)
  pres <- .rv_text(pres, "Administration, safety and readiness", c$l, c$t, c$w, 0.3, size = 11, bold = TRUE, color = .RV$navy)
  defs <- .rv_defs(epi_session_indicators)
  qk <- c(cats[["Administration and safety"]], cats[["Session readiness"]])
  qv <- vapply(qk, function(k) .rv_overall(s, k)$pct, numeric(1)); qk <- qk[order(qv)][1:min(5, length(qk))]
  obs <- vapply(qk, function(k) { o <- .rv_overall(s, k)
    sprintf("%s: %s (%s)", defs$label[defs$key == k], .rv_nN(.rv_fav(k, o), o$Total), .epi_pct(.rv_value(o$pct, k %in% .RV_CRITICAL))) },
    character(1))
  pres <- .rv_bullets(pres, obs, c$l, c$t + 0.32, c$w, c$h - 0.35, size = 9.5)
  .rv_grid_legend(pres, note = "Labels: provinces below 90%; * = expired vaccine or VVM 3/4 observed (a high score does not clear it).")
}

# v0.8.0: one safety slide. Seven item maps (where each problem was) + a follow-up
# panel in the 8th cell (the old combined exception map, v6 12-13, is no longer drawn:
# it repeated the item maps). Same figures as before: .rv_safety / .rv_safety_priority.
.rv_m_safety_practice <- function(pres, ctx) {
  s <- ctx$data$session
  reg <- .rv_ref(ctx, "annex_safety_register")
  pres <- .rv_slide(pres, ctx, "", "", note = .rv_note("Favourable among valid answers; expired vaccine and VVM 3/4 reverse-coded (green = not observed)",
                                                       if (!is.null(reg)) paste("Every flagged session:", reg)))
  if (!nrow(s)) return(epi_no_data(pres))
  keys <- c("icepacks", "expired", "vvm", "safety_box", "technique", "open_vial", "aefi_aware")
  nm <- c(icepacks = "Carrier + 4 icepacks", expired = "No expired vaccine", vvm = "No VVM stage 3/4", safety_box = "Safety box",
          technique = "Admin technique", open_vial = "Open-vial policy", aefi_aware = "AEFI awareness")
  panels <- lapply(keys, function(k) {
    o <- .rv_overall(s, k); pv <- .rv_pv_indicator(s, k); bad <- pv$n - pv$fav
    list(name = nm[[k]], value = .epi_pct(.rv_value(o$pct, k %in% .RV_CRITICAL)),
         sub = sprintf("%s unfavourable of %s sessions", .epi_n(o$Total - .rv_fav(k, o)), .epi_n(o$Total)), pv = pv,
         text = ifelse(!is.na(bad) & bad > 0, sprintf("%s\n%s/%s", pv$province, bad, pv$n), ""))
  })
  pres <- .rv_grid(pres, ctx, panels, 4, text_size = 1.6)
  # 8th cell: sessions to follow up (a session counts once, whatever number of items failed)
  sf <- .rv_safety(s); pr <- .rv_safety_priority(s)
  ex <- sum(s$expired == "Yes", na.rm = TRUE); vv <- sum(s$vvm == "Yes", na.rm = TRUE)
  where <- function(k) { p <- unique(s$Province[s[[k]] %in% "Yes"]); if (length(p)) sprintf(" (%s)", .rv_list(p, 2)) else "" }
  lst <- function(z, k) .rv_list(sprintf("%s %s", z$Province, .rv_nN(z$n, z$N)), k)
  c <- .rv_cell(8, 4, 2)
  pres <- .rv_text(pres, "", c$l, c$t + 0.02, c$w, c$h - 0.06, bg = .RV$tile)
  pres <- .rv_text(pres, "", c$l, c$t + 0.02, 0.06, c$h - 0.06, bg = if (sf$n) "#e8635c" else "#67b97a")
  pres <- .rv_text(pres, "Sessions to follow up", c$l + 0.14, c$t + 0.06, c$w - 0.2, 0.3, size = 11, bold = TRUE, color = .RV$navy)
  pres <- .rv_runs(pres, list(list(sprintf("%s of %s", sf$n, .epi_n(sf$N)), bold = TRUE, color = .RV$navy),
                              list(sprintf(" sessions (%s) had a problem in at least one of the first six items (all but AEFI).",
                                           .epi_pct(if (sf$N) sf$n / sf$N else NA)))),
                   c$l + 0.14, c$t + 0.36, c$w - 0.2, 0.5, size = 9)
  lines <- c(sprintf("Expired vaccine: %s%s. VVM stage 3/4: %s%s.", ex, where("expired"), vv, where("vvm")),
             sprintf("Verify first (3+ sessions): %s.", if (nrow(pr$eligible)) lst(pr$eligible, 3) else "none"),
             if (nrow(pr$low)) sprintf("Check each, not ranked (<3 sessions): %s.", lst(pr$low, 4)),
             if (!is.null(reg)) sprintf("Facility and date of every session: %s.", reg))
  pres <- .rv_bullets(pres, lines, c$l + 0.1, c$t + 0.88, c$w - 0.16, c$h - 0.92, size = 8.5)
  .rv_log(ctx, "safety_practice", sprintf("%s/%s sessions with a priority exception; verify first %s; low-sample (not ranked) %s", sf$n, sf$N,
                                          lst(pr$eligible, 8), lst(pr$low, 8)))
  .rv_grid_legend(pres, note = "Map labels: provinces with an unfavourable session (unfavourable / assessed); provinces need 3+ sessions.")
}

.rv_m_community <- function(pres, ctx) {
  cm <- ctx$data$community
  pres <- .rv_slide(pres, ctx, "", "", note = "Recorded status among assessed children 12-23 m, not population coverage; provinces need 10+ children")
  cv <- epi_coverage(cm, c("Vaccination card", "BCG", "Penta-1", "Penta-3", "PCV-3", "OPV-3", "Rota-1", "Measles-1", "Measles-2"))
  if (!nrow(cv)) return(epi_no_data(pres, "No community assessments for this selection"))
  mk <- c("Penta-3", "Measles-1", "BCG", "Vaccination card", "Penta-1", "PCV-3", "OPV-3", "Rota-1")   # priority row first
  panels <- lapply(mk, function(m) {
    o <- cv[cv$marker == m, ]; pv <- .rv_pv_coverage(cm, m); ok <- .rv_valid(pv, .RV_MIN_CHILD)
    low <- pv[ok, , drop = FALSE]; low <- low[order(low$value), , drop = FALSE][1, ]
    list(name = m, value = .epi_pct(o$pct), min_n = .RV_MIN_CHILD, pv = pv,
         sub = sprintf("%s | lowest: %s", .rv_nN(o$vaccinated, o$children),
                       if (!is.na(low$value)) sprintf("%s %s", low$province, .rv_nN(low$fav, low$n)) else "-"))
  })
  pres <- .rv_grid(pres, ctx, panels, 4, bottom = 5.95)
  dr <- epi_dropout(cm); drp <- epi_dropout(cm, "Province")
  ch <- epi_coverage(cm, "Penta-3", by = "Province"); keep <- ch$group[ch$children >= .RV_MIN_CHILD]
  neg <- drp[!is.na(drp$dropout) & drp$dropout < 0 & drp$group %in% keep, ]
  m2 <- cv[cv$marker == "Measles-2", ]
  pres <- .rv_mtile(pres, 0.45, 5.97, 3.0, 0.68, .epi_n(cv$children[1]), "Children 12-23 m",
                    sprintf("assessed in %s community assessments", .epi_n(nrow(cm))), "#1baf7a")
  pres <- .rv_mtile(pres, 3.56, 5.97, 3.0, 0.68, .epi_pct(dr$dropout[1], 1), "Penta-1 to Penta-3",
                    "cross-sectional difference, not cohort dropout", "#1baf7a")
  pres <- .rv_mtile(pres, 6.67, 5.97, 3.0, 0.68, .epi_pct(m2$pct), "Measles-2 recorded",
                    "age eligibility not applied; not mapped", "#AEB6C2")
  pres <- .rv_text(pres, if (nrow(neg)) sprintf("Validate: more Penta-3 than Penta-1 recorded in %s (difference kept, not set to zero).",
                                                .rv_list(sprintf("%s (by %s)", neg$group, .epi_pct(-neg$dropout, 1)), 3))
                   else "No province (10+ children) records more Penta-3 than Penta-1.", 9.8, 5.97, 3.05, 0.68, size = 8.5, color = .RV$ink)
  .rv_grid_legend(pres, low = "<10", note = "Labels: provinces below 75%.")
}

.rv_m_ytd_activity <- function(pres, ctx) {
  pres <- .rv_slide(pres, ctx, "", "", note = "Month of the visit date; the latest month may still receive late submissions")
  m <- epi_responses_by_month(ctx$data)
  if (!nrow(m) || !sum(m$n)) return(epi_no_data(pres))
  m$x <- factor(epi_month_label(m$month), levels = epi_month_label(sort(unique(m$month))))
  cols <- stats::setNames(epi_colors$series[1:3], unname(epi_pages))
  m$checklist <- factor(m$checklist, levels = names(cols))
  ch <- mschart::ms_barchart(m, x = "x", y = "n", group = "checklist")
  ch <- mschart::as_bar_stack(ch, gap_width = 50)
  ch <- mschart::chart_data_fill(ch, values = cols)
  ch <- mschart::chart_data_stroke(ch, values = stats::setNames(rep("#FFFFFF", 3), names(cols)))
  ch <- mschart::chart_labels(ch, title = NULL, xlab = NULL, ylab = NULL)
  ch <- mschart::chart_ax_y(ch, num_fmt = "#,##0", limit_min = 0)
  ch <- .epi_ms_theme(ch, legend = "t", axis_size = 9)
  pres <- .epi_at(pres, ch, 0.4, 1.4, 8.6, 5.55)
  dat <- ctx$data; n <- vapply(dat, nrow, integer(1))
  scope <- epi_district_scope(ctx$regions, ctx$provinces)
  reached <- sum(scope$DCode %in% epi_responses(dat)$DCode)
  nm <- length(unique(m$month))
  tl <- list(list(.epi_n(sum(n)), "Checklists", sprintf("%s per month on average", .epi_n(round(sum(n) / nm))), .RV$blue),
             list(.epi_n(n[["health"]]), "Health-center visits", NULL, cols[[1]]),
             list(.epi_n(n[["session"]]), "Session visits", NULL, cols[[2]]),
             list(.epi_n(n[["community"]]), "Community assessments",
                  sprintf("%s children checked", .epi_n(sum(dat$community$children[dat$community$children > 0], na.rm = TRUE))), cols[[3]]),
             list(sprintf("%s / %s", reached, nrow(scope)), "Districts reached", "1+ checklist this year", .RV$blue))
  for (i in seq_along(tl)) pres <- .rv_ktile(pres, 9.3, 1.42 + (i - 1) * 1.07, 3.55, 0.98, tl[[i]][[1]], tl[[i]][[2]],
                                             tl[[i]][[3]], accent = tl[[i]][[4]], vsize = 20)
  pres
}

.rv_m_hc_trends <- function(pres, ctx) .rv_category_trends(pres, ctx, "health", .rv_hc_categories, "")
.rv_m_session_trends <- function(pres, ctx) .rv_category_trends(pres, ctx, "session", .rv_session_categories, "")

.rv_m_tracker <- function(pres, ctx) {
  pres <- .rv_slide(pres, ctx, "", "", note = "Targets are verification deadlines, not performance targets; geography uses the eligibility rules of the evidence slides")
  h <- ctx$data$health; s <- ctx$data$session; cm <- ctx$data$community
  if (!nrow(h) || !nrow(s) || !nrow(cm)) return(epi_no_data(pres, "Insufficient data to generate the action tracker"))
  rp <- .rv_reporting(ctx); ml <- month.abb[as.integer(substr(ctx$ref, 6, 7))]
  gaps <- if (!is.null(rp$silent) && nrow(rp$silent)) .rv_list(sprintf("%s %s/%s", rp$silent$Province, rp$silent$`Not reporting`,
                                                                         rp$silent$Districts), 3) else "none"
  comp <- .rv_completion_by(h, "outreach", "Region"); comp <- comp[!is.na(comp$rate), ]
  comp_geo <- if (nrow(comp)) { cw <- comp[which.min(comp$rate), ]
    sprintf("%s: %s of %s planned (%s)", cw$group, .epi_n(cw$conducted), .epi_n(cw$planned), .epi_pct(cw$rate)) } else "No planned outreach recorded"
  keys <- c("pemt_mobile_supervision", "ngo_mobile_supervision")
  sup <- .rv_matrix(h, epi_health_indicators[epi_health_indicators$key %in% keys, ], "Region")
  sup_geo <- if (!is.null(sup) && nrow(sup)) {
    sw <- sup[order(sup$pct), ][1, ]; both <- sup[sup$group == sw$group, ]
    sprintf("%s: %s", sw$group, paste(sprintf("%s %s (%s)", c(pemt_mobile_supervision = "PEMT", ngo_mobile_supervision = "NGO")[both$key],
                                              .rv_nN(round(both$pct * both$Total), both$Total), .epi_pct(both$pct)), collapse = ", "))
  } else "No facility with mobile sessions planned"
  pr <- .rv_safety_priority(s)
  safety_where <- if (nrow(pr$eligible)) .rv_list(sprintf("%s %s", pr$eligible$Province, .rv_nN(pr$eligible$n, pr$eligible$N)), 3) else "No eligible province flagged"
  pv <- .rv_pv_coverage(cm, "Penta-3"); ok <- .rv_valid(pv, .RV_MIN_CHILD); lp <- pv[ok, ][which.min(pv$value[ok]), ]
  ev <- function(id) { x <- .rv_ref(ctx, id); if (is.null(x)) "-" else sub("^slide ", "", x) }
  rows <- data.frame(
    Priority = 1:5,
    Issue = c("District reporting gaps", "Outreach implementation gap", "Applicable mobile supervision gap",
              "Priority session-safety exceptions", "Low Penta-3 status among assessed children"),
    Geography = c(sprintf("%s districts without a checklist in %s (%s none since Jan); largest: %s", rp$n[2] + rp$n[3], ml, rp$n[3], gaps),
                  comp_geo, sup_geo, safety_where,
                  if (nrow(lp)) sprintf("%s: %s (%s)", lp$province, .rv_nN(lp$fav, lp$n), .epi_pct(lp$value)) else "-"),
    `Accountable function` = c("REMT/PEMT focal points", "PEMT + implementing NGO", "PEMT + NGO supervisors",
                               "PEMT/REMT EPI teams", "PEMT/NGO follow-up teams"),
    `Verification task` = c("Reconcile Kobo submissions with visit records", "Verify plans, access constraints and over-plan records",
                            "Check applicable mobile plans and supervision logbooks", "Trace checklist, facility, date and corrective response",
                            "Validate child records, eligibility and due-dose follow-up"),
    Target = rep("Next review", 5),
    `Closure evidence` = c("Matched submission or documented reason", "Corrected plan and verified completion",
                           "Signed supervision log", "Facility verification and corrective action",
                           "Validated child list and follow-up record"),
    Evidence = c(ev("reporting_presence"), ev("implementation"), ev("supervision"), ev("safety_practice"), ev("community_status")),
    check.names = FALSE)
  names(rows)[names(rows) == "Evidence"] <- "Evidence slide"
  ft <- flextable::flextable(rows)
  ft <- flextable::border_remove(ft); ft <- flextable::font(ft, fontname = epi_font, part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all"); ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = .RV$navy, part = "header")
  ft <- flextable::bg(ft, j = "Priority", bg = function(x) c(.RV$bad, .RV$bad, .RV$warn, .RV$bad, .RV$warn)[x], part = "body")
  ft <- flextable::bg(ft, i = c(2, 4), j = 2:ncol(rows), bg = "#F6F8FA", part = "body")
  ft <- flextable::bold(ft, j = "Geography", part = "body")
  ft <- flextable::vline(ft, j = 1:(ncol(rows) - 1), border = officer::fp_border(color = "#D9E2EC", width = 0.6), part = "all")
  ft <- flextable::align(ft, j = c("Priority", "Target", "Evidence slide"), align = "center", part = "all")
  ft <- flextable::valign(ft, valign = "center", part = "body")
  ft <- flextable::padding(ft, padding = 4, part = "all")
  ft <- flextable::width(ft, j = seq_len(ncol(rows)), width = c(0.6, 1.55, 2.6, 1.35, 2.1, 0.75, 1.95, 0.75))
  .epi_at(pres, .rv_fit(ft, 12.3, 5.4, max_row = 0.95), 0.5, 1.45, 12.3, 5.4)
}

.rv_m_notes <- function(pres, ctx) {
  pres <- .rv_slide(pres, ctx, "", "")
  man <- ctx$manifest
  parts <- unique(man$part)
  struct <- sprintf("Structure: %s. Every indicator stays in the dashboard app (Health center, Session, Community pages).",
                    paste(sprintf("%s = slides %s", parts, vapply(parts, function(p) .rv_part_range(man, p), character(1))), collapse = "; "))
  lines <- c(
    sprintf("Source: KoboToolbox Routine EPI checklist (export EPI_BACKUP), unchanged form. Report month = %s; year to date = %s; month = month of the visit date.",
            ctx$ref_label, ctx$ytd_label),
    struct,
    "Reporting presence: districts with at least one checklist / districts of the selected regions in the boundary file (District Code). Presence is not compliance with a supervision plan.",
    "Yes/No indicators: favourable answers / valid Yes + No answers; blanks excluded, so denominators (n/N) differ by question. Vaccines/supplies multi-select is excluded from scores.",
    "Thematic scores: equal-weight mean of applicable question rates (descriptive, not all-criteria compliance). Expired vaccine and VVM 3/4 are reverse-coded (higher = better).",
    "Mobile supervision: only facilities with planned mobile sessions > 0; PEMT monthly and NGO quarterly reference periods are reported separately, never combined.",
    "Implementation: completion = conducted / planned, facility-month records with planned > 0; a duplicate facility-month (same district, facility, year, month) counts once - first record kept - nationally, by region and by province, so the levels add up.",
    "Safety exception: a session counts once when any of six priority items is unfavourable; provinces need 3+ sessions to be ranked, smaller ones are listed separately, not ranked.",
    "Community: recorded vaccinated / assessed children 12-23 m, not population coverage. Measles-2 age eligibility not applied. Penta-1 to Penta-3 = cross-sectional difference. Reasons use the assessment as denominator (multi-select).",
    sprintf("Map classes: green >= %d%%, amber %d-%d%%, red < %d%% (descriptive display bands, not targets); low sample = fewer than 3 assessments / sessions (10 children); not applicable = records but no applicable answer; no observations = no record.",
            .RV_GOOD * 100, .RV_WARN * 100, .RV_GOOD * 100 - 1, .RV_WARN * 100),
    "Map grids replace the v6 tables: one small provincial map per category, indicator or antigen, national value in the map title, lowest area underneath; provinces below 75% are labelled. Exact question values: annex.")
  .rv_bullets(pres, lines, 0.5, 1.42, 12.3, 5.55, size = 10.5)
}

# ---- Technical annex modules ------------------------------------------------------
.rv_m_annex_hc_questions <- function(pres, ctx) {
  d <- ctx$data$health
  pres <- .rv_slide(pres, ctx, "", "", note = .rv_note("Favourable answers among valid Yes/No answers; mobile supervision: facilities with mobile sessions planned",
                                                       if (!is.null(.rv_ref(ctx, "hc_thematic"))) paste("Category maps:", .rv_ref(ctx, "hc_thematic"))))
  if (!nrow(d)) return(epi_no_data(pres))
  cats <- names(.rv_hc_categories)
  keys <- unlist(.rv_hc_categories[cats], use.names = FALSE)
  defs <- .rv_defs(epi_health_indicators[match(keys, epi_health_indicators$key), ])
  cat_of <- rep(sub(" \\(context\\)", "", sub(" and microplanning| management| and reporting| and community readiness| follow-up", "", cats)),
                lengths(.rv_hc_categories[cats]))
  mat <- .rv_matrix(d, defs, ctx$geo_level)
  groups <- sort(unique(mat$group))
  tab <- data.frame(Category = cat_of, Question = defs$label, stringsAsFactors = FALSE)
  for (g in groups) { x <- mat[mat$group == g, ]; tab[[g]] <- x$pct[match(defs$key, x$key)] }
  ov <- lapply(defs$key, function(k) .rv_overall(d, k))
  tab$National <- vapply(ov, function(o) o$pct, numeric(1))
  tab$`n / N` <- vapply(ov, function(o) .rv_nN(o$Yes, o$Total), character(1))
  pc <- c(groups, "National")
  ft <- .rv_heat_ft(tab, pc, font_size = 8)
  ft <- flextable::merge_v(ft, j = "Category")
  ft <- flextable::valign(ft, j = "Category", valign = "top", part = "body")
  ft <- flextable::bold(ft, j = c("Category", "National"), part = "all")
  ft <- flextable::align(ft, j = 1:2, align = "left", part = "all")
  ft <- flextable::padding(ft, padding.top = 0.5, padding.bottom = 0.5, part = "all")
  ft <- flextable::fit_to_width(ft, max_width = 12.3)
  ft <- flextable::width(ft, j = seq_len(ncol(tab)), width = ft$body$colwidths * 12.3 / sum(ft$body$colwidths))
  ft <- flextable::height_all(ft, height = 0.215, part = "all")
  pres <- .epi_at(pres, ft, 0.5, 1.36, 12.3, 5.3)
  .rv_log(ctx, "annex_hc_questions", sprintf("%s questions by %s (exact values behind the category maps).", length(keys), tolower(ctx$geo_level)))
  .rv_rag_legend(pres, 9.65, 6.72)
}

.rv_m_annex_register <- function(pres, ctx) {
  s <- ctx$data$session
  pres <- .rv_slide(pres, ctx, "", "", note = .rv_note("Sessions with any of six priority items unfavourable; critical items (expired / VVM) shaded",
                                                       if (!is.null(.rv_ref(ctx, "safety_practice"))) paste("Item maps:", .rv_ref(ctx, "safety_practice"))))
  if (!nrow(s)) return(epi_no_data(pres))
  sf <- .rv_safety(s); defs <- .rv_defs(epi_session_indicators)
  lab <- defs$label[match(.RV_SAFETY_KEYS, defs$key)]; names(lab) <- .RV_SAFETY_KEYS
  fl <- which(sf$exception)
  if (!length(fl)) return(epi_no_data(pres, "No session with a priority safety exception"))
  items <- vapply(fl, function(r) paste(lab[.RV_SAFETY_KEYS[which(sf$bad[r, ] %in% TRUE)]], collapse = ", "), character(1))
  crit <- vapply(fl, function(r) any(sf$bad[r, .RV_CRITICAL] %in% TRUE), logical(1))
  reg <- data.frame(Region = s$Region[fl], Province = s$Province[fl], District = s$District[fl], Facility = s$Facility[fl],
                    Date = format(s$VisitDate[fl], "%d %b %Y"), Session = s$SessionType[fl], `Unfavourable items` = items,
                    check.names = FALSE, stringsAsFactors = FALSE)
  ord <- order(!crit, reg$Region, reg$Province, reg$District); reg <- reg[ord, ]; crit <- crit[ord]
  shown <- head(reg, 24)
  cnt <- colSums(sf$bad, na.rm = TRUE)
  pres <- .rv_text(pres, sprintf("%s of %s assessed sessions flagged%s. By item: %s.", nrow(reg), sf$N,
                                 if (nrow(reg) > nrow(shown)) sprintf("; first %s shown, all in the app (Session page)", nrow(shown)) else "",
                                 paste(sprintf("%s %s", lab, cnt), collapse = ", ")),
                   0.5, 1.38, 12.3, 0.32, size = 10.5, bold = TRUE, color = .RV$navy)
  ft <- flextable::flextable(shown)
  ft <- flextable::border_remove(ft); ft <- flextable::font(ft, fontname = epi_font, part = "all")
  ft <- flextable::fontsize(ft, size = if (nrow(shown) > 16) 8.5 else 10, part = "all"); ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = .RV$navy, part = "header")
  ft <- flextable::hline(ft, border = officer::fp_border(color = "#E6EBF0", width = 0.5), part = "body")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = .RV$navy, width = 1), part = "header")
  ci <- which(crit[seq_len(nrow(shown))])
  if (length(ci)) ft <- flextable::bg(ft, i = ci, bg = .RV$bad, part = "body")
  ft <- flextable::padding(ft, padding.top = 1.5, padding.bottom = 1.5, padding.left = 3, padding.right = 3, part = "all")
  ft <- flextable::width(ft, j = 1:7, width = c(1.2, 1.3, 1.5, 2.2, 1.1, 0.9, 4.1))
  ft <- .rv_fit(ft, 12.3, 5.1, max_row = 0.3)
  pres <- .epi_at(pres, ft, 0.5, 1.8, 12.3, 5.1)
  .rv_log(ctx, "annex_safety_register", sprintf("%s flagged sessions listed (%s critical).", nrow(reg), sum(crit)))
  pres
}

# ---- Build -------------------------------------------------------------------------
# data: the three tables filtered by region / province only (the period comes
# from ref_month: the report month and its year to date). ref_month = "YYYY-MM";
# NULL = last complete month (epi_ref_month()). One deck: every slide of the
# manifest. Returns the file path with attributes "manifest" (with slide
# numbers) and "log" (notes written by the modules).
epi_build_review_deck <- function(data, file, filters = list(), geo_choice = "Auto", as_of = NA, paths = epi_paths(),
                                  ref_month = NULL, progress = NULL, ...) {
  step <- function(msg) if (is.function(progress)) progress(msg)
  ref <- if (length(ref_month) && nzchar(ref_month)) ref_month else epi_ref_month(data)
  months <- epi_ytd_months(ref)
  prev <- .epi_add_months(ref, -1)
  base_filters <- filters[c("regions", "provinces")]
  ref_label <- format(as.Date(paste0(ref, "-01")), "%B %Y")
  ytd_label <- if (length(months) > 1) paste(month.abb[1], "-", epi_month_label(ref)) else epi_month_label(ref)
  man <- epi_review_manifest()
  geo_level <- epi_geo_level(geo_choice, filters$regions)
  fill <- function(x) {
    x <- gsub("{ref}", ref_label, x, fixed = TRUE); x <- gsub("{ytd}", ytd_label, x, fixed = TRUE)
    x <- gsub("{geo}", tolower(geo_level), x, fixed = TRUE)
    x <- gsub("{community_status}", as.character(man$order[match("community_status", man$id)]), x, fixed = TRUE)
    x
  }
  man$title <- fill(man$title); man$kicker <- fill(man$kicker)
  man$kicker[man$deck == "annex"] <- paste0("Technical annex ", man$annex_label[man$deck == "annex"], " | ",
                                            man$kicker[man$deck == "annex"], " | ", ref_label)
  parts <- unique(man$part)
  contents <- data.frame(part = parts, range = vapply(parts, function(p) .rv_part_range(man, p), character(1)),
                         color = unname(.RV_PART_COL[parts]), stringsAsFactors = FALSE)

  ctx <- list(regions = filters$regions, provinces = filters$provinces, geo_level = geo_level, as_of = as_of,
              filter_label = if (length(unlist(base_filters))) epi_filter_label(base_filters) else "All regions",
              ref = ref, ref_label = ref_label, ytd_label = ytd_label, months = months, page_no = new.env(),
              integrated = TRUE, manifest = man, log = new.env(), contents = contents,
              source_note = epi_source_note(epi_subset_months(data, months), "Source: KoboToolbox (EPI_BACKUP)"))
  ctx$page_no$n <- 1; ctx$log$rows <- list()
  ytd <- ctx; ytd$data <- epi_subset_months(data, months); ytd$period <- ytd_label
  last <- ctx; last$data <- epi_subset_months(data, ref); last$data_prev <- epi_subset_months(data, prev)
  last$period <- ref_label; last$ytd <- ytd
  ytd$ytd <- ytd

  pres <- officer::read_pptx(.epi_review_template())
  for (i in seq_len(nrow(man))) {
    m <- man[i, ]
    cx <- if (m$period == "ytd") ytd else last
    cx$mod <- if (m$id == "cover") NULL else list(kicker = m$kicker, title = m$title, part = m$part)
    step(m$title)
    pres <- get(m$module, mode = "function")(pres, cx)
    if (length(pres) != i) stop(sprintf("Deck module '%s' made %s slides; each module must add exactly one", m$id, length(pres) - i + 1))
  }
  step("Writing file")
  print(pres, target = file)
  log <- if (length(ctx$log$rows)) do.call(rbind, ctx$log$rows) else data.frame(module = character(), decision = character())
  invisible(structure(file, manifest = man, log = log))
}
