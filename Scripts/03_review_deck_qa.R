# Deck B v7: build the EPI supervision deck (one deck), then check it.
#   Rscript Scripts/03_review_deck_qa.R [report month YYYY-MM] [output folder]
# Writes to the output folder (default outputs/presentations):
#   Deck_B_EPI_Integrated_v7_<Mon><YYYY>.pptx, Deck_B_v7_manifest.csv, Deck_B_v7_slide_mapping.csv,
#   Deck_B_v7_build_log.csv, Deck_B_v7_qa_results.csv, Deck_B_EPI_Integrated_v7_<Mon><YYYY>_Validation.md
# Checks: slide count against the manifest; figures moved / merged from v6
# against an independent recomputation with the v6 code; every provincial map
# against the v6 map logic; every v6 map present; single-region and no-data
# builds; navigation references. Exit status 1 when a check fails.
source(file.path(Sys.getenv("EPI_PROJECT_ROOT", getwd()), "R", "epi_load.R"))
epi_source_all()
args <- commandArgs(TRUE)
ref <- if (length(args) >= 1 && nzchar(args[1])) args[1] else "2026-10"
out_dir <- if (length(args) >= 2 && nzchar(args[2])) args[2] else epi_paths()$decks
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tag <- format(as.Date(paste0(ref, "-01")), "%b%Y")

d <- epi_load_data(epi_paths())
data <- lapply(d[c("health", "session", "community")], epi_filter, f = list())
last <- epi_subset_months(data, ref)
h <- last$health; s <- last$session; cm <- last$community
tmp <- tempfile("v7qa_"); dir.create(tmp)
res <- data.frame(area = character(), check = character(), expected = character(), observed = character(),
                  status = character(), stringsAsFactors = FALSE)
chk <- function(area, check, expected, observed, ok = identical(as.character(expected), as.character(observed)))
  res[nrow(res) + 1, ] <<- c(area, check, as.character(expected), as.character(observed), if (isTRUE(ok)) "PASS" else "FAIL")

# ---- 1. Builds -------------------------------------------------------------------
f_deck <- file.path(tmp, "deck.pptx")
b <- epi_build_review_deck(data, f_deck, as_of = d$meta$fetched_at, ref_month = ref)
man <- attr(b, "manifest"); lg <- attr(b, "log")
n_deck <- length(officer::read_pptx(f_deck))
chk("Structure", "Slides = manifest", nrow(man), n_deck)
n_div <- sum(man$module == ".rv_m_divider")
chk("Structure", "Content slides (without dividers) 19-21", "19-21", n_deck - n_div, (n_deck - n_div) >= 19 && (n_deck - n_div) <= 21)
chk("Structure", "One divider per checklist + year to date", 4, n_div)
chk("Structure", "Technical annex has 2-4 slides", "2-4", sum(man$deck == "annex"), sum(man$deck == "annex") %in% 2:4)
f_south <- file.path(tmp, "south.pptx")
chk("Structure", "Single region (SOUTH) builds, slides = manifest", nrow(man),
    tryCatch({ epi_build_review_deck(lapply(data, epi_filter, f = list(regions = "SOUTH")), f_south, filters = list(regions = "SOUTH"),
                                     ref_month = ref); length(officer::read_pptx(f_south)) }, error = function(e) conditionMessage(e)))
f_empty <- file.path(tmp, "empty.pptx")
chk("Structure", "No-data build, slides = manifest", nrow(man),
    tryCatch({ epi_build_review_deck(lapply(data, function(x) x[0, ]), f_empty, ref_month = ref); length(officer::read_pptx(f_empty)) },
             error = function(e) conditionMessage(e)))
chk("Structure", "No speaker notes", FALSE, any(grepl("notesSlides/notesSlide", utils::unzip(f_deck, list = TRUE)$Name)))

# slide text (in slide order) for figure checks
slide_text <- function(f) {
  p <- officer::read_pptx(f)
  vapply(seq_len(length(p)), function(i) {
    x <- officer::slide_summary(p, index = i)
    paste(x$text[!is.na(x$text)], collapse = " ")
  }, character(1))
}
tx <- slide_text(f_deck)
on_slide <- function(id) tx[man$order[match(id, man$id)]]
has <- function(id, pattern) grepl(pattern, on_slide(id), fixed = TRUE)
v6_file <- file.path(epi_paths()$decks, "Deck_B_EPI_Integrated_v6_Oct2026.pptx")
tx6 <- if (file.exists(v6_file) && ref == "2026-10") slide_text(v6_file) else NULL
has6 <- function(i, pattern) if (is.null(tx6)) NA else grepl(pattern, gsub("\\s+", " ", tx6[i]), fixed = TRUE)

# ---- 2. Figures moved / merged from v6 (independent recomputation) -----------------
pct <- function(x) .epi_pct(x)
yn <- function(k, app = FALSE) { x <- if (app) h[!is.na(h$mobile_planned) & h$mobile_planned > 0, ] else h; epi_yn_overall(x, k) }
fig <- function(id, label, value, v6_slide = NA, v6_pattern = NULL) {
  ok7 <- has(id, value)
  ok6 <- if (!is.null(v6_pattern) && !is.na(v6_slide)) has6(v6_slide, v6_pattern) else NA
  chk(paste("Figure |", id), label, value, if (ok7) value else "not found on slide", ok7 && (is.na(ok6) || ok6))
  if (!is.na(ok6) && !ok6) res$check[nrow(res)] <<- paste(label, "(v6 slide", v6_slide, "does not show it)")
}
fig("exec_findings", "Checklists in month", .epi_n(sum(vapply(last, nrow, 1L))), 2, .epi_n(sum(vapply(last, nrow, 1L))))
for (p in names(epi_pages)) fig("exec_findings", paste(epi_pages[[p]], "count"), .epi_n(nrow(last[[p]])), 2, .epi_n(nrow(last[[p]])))
r <- epi_responses(epi_subset_months(data, epi_ytd_months(ref)))
dc <- epi_geo()$districts$district_code
n_ref <- sum(dc %in% r$DCode[r$month == ref]); n_any <- sum(dc %in% r$DCode)
fig("reporting_presence", "Districts with a checklist", sprintf("%s  (%s)", n_ref, pct(n_ref / length(dc))), 5, sprintf("%s (%s)", n_ref, pct(n_ref / length(dc))))
fig("reporting_presence", "None since January", sprintf("%s  (%s)", length(dc) - n_any, pct((length(dc) - n_any) / length(dc))), 5,
    sprintf("%s (%s)", length(dc) - n_any, pct((length(dc) - n_any) / length(dc))))
for (p in names(epi_pages)) {
  k <- length(unique(last[[p]]$DCode[!is.na(last[[p]]$DCode) & last[[p]]$DCode %in% dc]))
  fig("reporting_presence", paste("Districts reached,", epi_pages[[p]]), as.character(k), 4, sprintf("%s districts", k))
}
dd <- h[!duplicated(paste(h$DCode, h$Facility, h$Year, h$Month, sep = "|")), ]
for (type in c("outreach", "mobile")) {
  p <- dd[[paste0(type, "_planned")]]; c <- dd[[paste0(type, "_conducted")]]; ok <- !is.na(p) & !is.na(c) & p > 0
  fig("implementation", paste(type, "completion (national, v6 table logic)"), sprintf("%s  completion", pct(sum(c[ok]) / sum(p[ok]))), 6,
      pct(sum(c[ok]) / sum(p[ok])))
  fig("implementation", paste(type, "planned / conducted"), sprintf("%s conducted of %s planned", .epi_n(sum(c[ok])), .epi_n(sum(p[ok]))), 6,
      .epi_n(sum(p[ok])))
}
# health-center thematic table (v6 slide 8) -> map titles: national score and lowest area
# (supervision categories are mapped by indicator on the supervision slide)
tab <- .rv_category_table(h, .rv_hc_categories, "Region")
tab <- tab[!tab$Category %in% c("Outreach supervision", "Mobile supervision"), ]
o <- epi_yn_overall(h, "trained"); tg <- epi_yn_by(h, "trained", "Region"); tg <- tg[which.min(tg$pct), ]
fig("hc_thematic", "Vaccinator trained n/N", sprintf("%s/%s | lowest region: %s %s", o$Yes, o$Total, tg$group, pct(tg$pct)))
for (i in seq_len(nrow(tab)))
  fig("hc_thematic", paste("HC category,", tab$Category[i]), sprintf("lowest region: %s %s", tab$`Lowest area`[i], pct(tab$`Lowest score`[i])), 8,
      tab$`Lowest area`[i])
for (k in c("pemt_supervision", "ngo_supervision")) { o <- yn(k)
  fig("supervision", k, sprintf("%s/%s", o$Yes, o$Total), 9, sprintf("%s / %s", o$Yes, o$Total)) }
for (k in c("pemt_mobile_supervision", "ngo_mobile_supervision")) { o <- yn(k, TRUE)
  fig("supervision", paste(k, "(applicable)"), sprintf("%s/%s", o$Yes, o$Total), 9, sprintf("%s / %s", o$Yes, o$Total)) }
stab <- .rv_category_table(s, .rv_session_categories, "Region")
for (i in seq_len(nrow(stab)))
  fig("session_thematic", paste("Session category,", stab$Category[i]), sprintf("lowest region: %s %s", stab$`Lowest area`[i], pct(stab$`Lowest score`[i])), 11,
      stab$`Lowest area`[i])
keys <- c("expired", "vvm", "icepacks", "safety_box", "technique", "open_vial")
bad <- do.call(cbind, lapply(keys, function(k) if (k %in% c("expired", "vvm")) s[[k]] == "Yes" else s[[k]] == "No"))
valid <- rowSums(!is.na(bad)) > 0; nexc <- sum(rowSums(bad, na.rm = TRUE) > 0 & valid)
# v0.8.0: the combined exception slide (v6 12-13) is merged into the safety-practice slide (follow-up panel)
fig("safety_practice", "Sessions with an exception / assessed", sprintf("%s of %s sessions (%s)", nexc, sum(valid), pct(nexc / sum(valid))), 13,
    sprintf("%s/%s", nexc, sum(valid)))
fig("safety_practice", "Expired-vaccine observations", sprintf("Expired vaccine: %s", sum(s$expired == "Yes", na.rm = TRUE)), 12,
    as.character(sum(s$expired == "Yes", na.rm = TRUE)))
fig("safety_practice", "VVM stage 3/4 observations", sprintf("VVM stage 3/4: %s", sum(s$vvm == "Yes", na.rm = TRUE)), 12,
    as.character(sum(s$vvm == "Yes", na.rm = TRUE)))
chk("Duplication", "No separate combined safety-exception map slide", FALSE, "session_safety" %in% man$id)
for (k in c(keys, "aefi_aware")) { o <- epi_yn_overall(s, k); u <- if (k %in% c("expired", "vvm")) o$Yes else o$No
  fig("safety_practice", paste("Unfavourable,", k), sprintf("%s unfavourable of %s sessions", u, o$Total)) }
mk <- c("Penta-3", "Measles-1", "Penta-1", "PCV-3", "OPV-3", "Rota-1", "BCG", "Vaccination card")
cv <- epi_coverage(cm, mk)
for (m in mk) { o <- cv[cv$marker == m, ]
  fig("community_status", paste(m, "n/N"), sprintf("%s  %s %s/%s", m, pct(o$pct), .epi_n(o$vaccinated), .epi_n(o$children))) }
fig("community_status", "Zabul negative P1-P3 difference kept", "Zabul (by 6.7%)", 15, "-6.7%*")
if (ref == "2026-10") {
  fig("community_status", "Lowest eligible Penta-3 (map title)", "lowest: Kandahar 12/26", 15, "Kandahar")
  fig("action_tracker", "Northeast mobile supervision", "NORTHEAST: PEMT 1/9 (11%), NGO 1/9 (11%)", 23, "NORTHEAST (11%)")
  fig("action_tracker", "Eligible safety priorities", "Ghor 3/19, Hilmand 2/11, Badakhshan 1/6", 23, "Ghor 3/19, Hilmand 2/11, Badakhshan 1/6")
  fig("action_tracker", "Lowest eligible Penta-3", "Kandahar: 12/26 (46%)", 23, "Kandahar: 12/26 (46%)")
  fig("action_tracker", "Outreach implementation gap", "BADAKHSHAN: 80 of 105 planned (76%)", 23, "BADAKHSHAN (76%)")
  fig("safety_practice", "Verify first (3+ sessions)", "Ghor 3/19, Hilmand 2/11, Badakhshan 1/6", 13, "Ghor")
  fig("safety_practice", "Low-sample provinces not ranked", "Baghlan 1/2, Faryab 1/1, Kunduz 1/2, Takhar 1/2", NA)
  fig("annex_safety_register", "Expired-vaccine session traceable", "Kajaki CHC")
}

# ---- 3. Provincial maps vs v6 map logic -----------------------------------------
prov <- epi_geo()$provinces$province
cmp <- function(map, v7, v6v) {
  diff <- suppressWarnings(max(abs(v7 - v6v), na.rm = TRUE))
  same_na <- identical(is.na(v7), is.na(v6v))
  chk("Map values", map, "max |v7 - v6| < 1e-9, same provinces", sprintf("%s provinces with data, max diff %s",
      sum(!is.na(v7)), format(if (is.finite(diff)) diff else 0, digits = 3)), same_na && (!is.finite(diff) || diff < 1e-9))
}
# Completion: one dedup rule at every level (v0.7.0). Recompute independently (dedup first, then split)
# and report the provinces whose value moved from the v6 map (v6 maps did not dedup).
v6_completion <- function(type, hh = h) {
  z <- do.call(rbind, lapply(split(hh, hh$Province), function(x) {
    p <- x[[paste0(type, "_planned")]]; c <- x[[paste0(type, "_conducted")]]; ok <- !is.na(p) & !is.na(c) & p > 0
    data.frame(Province = x$Province[1], Rate = if (sum(p[ok]) > 0) sum(c[ok]) / sum(p[ok]) else NA_real_)
  }))
  z$Rate[match(prov, z$Province)]
}
hd <- h[!duplicated(paste(h$DCode, h$Facility, h$Year, h$Month, sep = "|")), ]
moved <- character()
for (type in c("outreach", "mobile")) {
  v7 <- .rv_pv_completion(h, type)$value
  cmp(paste("Completion (dedup, independent),", type), v7, v6_completion(type, hd))
  old <- v6_completion(type); ch <- which(abs(v7 - old) > 1e-9 | xor(is.na(v7), is.na(old)))
  moved <- c(moved, sprintf("%s %s: v6 %s -> v7 %s", type, prov[ch], .epi_pct(old[ch], 1), .epi_pct(v7[ch], 1)))
  nat <- .rv_completion(h, type); pr <- .rv_completion_by(h, type, "Province"); rg <- .rv_completion_by(h, type, "Region")
  chk("Reconciliation", paste(type, "planned: provinces = regions = national"), nat$planned,
      sprintf("%s / %s", sum(pr$planned), sum(rg$planned)), sum(pr$planned) == nat$planned && sum(rg$planned) == nat$planned)
  chk("Reconciliation", paste(type, "conducted: provinces = regions = national"), nat$conducted,
      sprintf("%s / %s", sum(pr$conducted), sum(rg$conducted)), sum(pr$conducted) == nat$conducted && sum(rg$conducted) == nat$conducted)
}
chk("Reconciliation", "Provincial completion values changed by the dedup rule (vs v6 maps)", "listed", if (length(moved)) paste(moved, collapse = "; ") else "none", ok = TRUE)
dups <- .rv_completion_dups(h)
for (k in c("pemt_supervision", "ngo_supervision", "pemt_mobile_supervision", "ngo_mobile_supervision")) {
  z <- epi_yn_by(.rv_applicable(h, k), k, "Province"); cmp(paste("Supervision,", k), .rv_pv_indicator(h, k)$value, z$pct[match(prov, z$group)])
}
v6_cat <- function(dd, keys) vapply(prov, function(p) {
  x <- dd[dd$Province == p, , drop = FALSE]; if (!nrow(x)) return(NA_real_)
  q <- vapply(keys, function(k) .rv_value(.rv_overall(x, k)$pct, k %in% c("expired", "vvm")), numeric(1))
  if (all(is.na(q))) NA_real_ else mean(q, na.rm = TRUE) }, numeric(1))
for (nm in names(.rv_hc_categories)) cmp(paste("HC category,", nm), .rv_pv_category(h, .rv_hc_categories[[nm]])$value,
                                         unname(v6_cat(h, .rv_hc_categories[[nm]])))
for (nm in names(.rv_session_categories)) cmp(paste("Session category,", nm), .rv_pv_category(s, .rv_session_categories[[nm]])$value,
                                              unname(v6_cat(s, .rv_session_categories[[nm]])))
for (k in c(keys, "aefi_aware")) {
  z <- epi_yn_by(s, k, "Province"); v6v <- z$pct[match(prov, z$group)]; if (k %in% c("expired", "vvm")) v6v <- 1 - v6v
  cmp(paste("Safety item,", k), .rv_pv_indicator(s, k)$value, v6v)
}
d6 <- s; d6$exception <- ifelse(rowSums(!is.na(bad)) == 0, NA, rowSums(bad, na.rm = TRUE) > 0)
z6 <- do.call(rbind, lapply(split(d6, d6$Province), function(x) data.frame(Province = x$Province[1], Sessions = sum(!is.na(x$exception)),
  Rate = if (sum(!is.na(x$exception))) sum(x$exception, na.rm = TRUE) / sum(!is.na(x$exception)) else NA_real_)))
z7 <- .rv_safety_province(s)
cmp("Safety exception rate", z7$Rate[match(prov, z7$Province)], z6$Rate[match(prov, z6$Province)])
for (m in mk) { z <- epi_coverage(cm, m, by = "Province"); cmp(paste("Vaccination status,", m), .rv_pv_coverage(cm, m)$value, z$pct[match(prov, z$group)]) }
cls <- .rv_classify(.rv_pv_completion(h, "mobile"))
chk("Map classes", "Mobile completion uses the grey classes separately", "Low sample, Not applicable",
    paste(intersect(c("Low sample", "Not applicable", "No observations"), unique(cls)), collapse = ", "),
    all(c("Low sample", "Not applicable") %in% cls))
chk("Map classes", "Grey class colours are distinct", 3, length(unique(.RV_CLS_COL[c("Low sample", "Not applicable", "No observations")])))

# ---- 4. Every v6 map is on a v7 slide; annex holds tables only ----------------------
v6_maps <- list(hc_thematic = c(setdiff(names(.rv_hc_categories), c("Outreach supervision", "Mobile supervision")),
                                "Vaccinator trained (12 m)"), supervision = c("PEMT outreach", "NGO outreach", "PEMT mobile", "NGO mobile"),
                session_thematic = names(.rv_session_categories), safety_practice = c("Carrier + 4 icepacks", "No expired vaccine", "No VVM stage 3/4",
                "Safety box", "Admin technique", "Open-vial policy", "AEFI awareness"), community_status = mk)
for (id in names(v6_maps)) {
  miss <- v6_maps[[id]][!vapply(v6_maps[[id]], function(x) has(id, x), logical(1))]
  chk("Maps kept", sprintf("%s: %s maps", id, length(v6_maps[[id]])), "all present", if (length(miss)) paste("missing:", paste(miss, collapse = ", ")) else "all present")
}
chk("Duplication", "Supervision averages not mapped twice (health-center slide omits them)", FALSE,
    has("hc_thematic", "Outreach supervision") || has("hc_thematic", "Mobile supervision  "))
for (p in unique(man$part)) {
  rng <- .rv_part_range(man, p)
  chk("Navigation", paste("Cover contents lists", p), rng, if (grepl(rng, tx[1], fixed = TRUE) && grepl(p, tx[1], fixed = TRUE)) rng else "missing",
      grepl(rng, tx[1], fixed = TRUE) && grepl(p, tx[1], fixed = TRUE))
}
for (i in which(man$module == ".rv_m_divider")) {
  sl <- man[man$part == man$part[i] & man$module != ".rv_m_divider", ]
  ok <- all(vapply(seq_len(nrow(sl)), function(j) grepl(sl$title[j], tx[i], fixed = TRUE), logical(1)))
  chk("Navigation", paste("Divider lists its slides:", man$part[i]), nrow(sl), if (ok) nrow(sl) else "missing", ok)
}
# Community age range: the form records an aggregate count "children aged 12-23 months" per assessment
cm_ok <- cm[!is.na(cm$children) & cm$children > 0, ]
mcols <- grep("^m_", names(cm_ok), value = TRUE)
over <- cm_ok[apply(as.matrix(cm_ok[, mcols]), 1, function(x) any(x > 0 & !is.na(x))) &
                apply(as.matrix(cm_ok[, mcols]) > cm_ok$children, 1, any, na.rm = TRUE), ]
chk("Data checks", "Child-level age available to find 24-month-olds", "not available (aggregate count only)",
    if (any(grepl("age", names(cm), ignore.case = TRUE))) "age column present" else "not available (aggregate count only)")
chk("Data checks", "Assessments where an antigen count exceeds children checked (kept, not changed)", "listed",
    sprintf("%s of %s assessments", nrow(over), nrow(cm_ok)), ok = TRUE)
chk("Duplication", "Annex slides contain no maps (tables only)", TRUE, all(grepl("^none", man$map_type[man$deck == "annex"])))
chk("Duplication", "No v6 table repeats a map grid (category tables replaced)", FALSE,
    any(grepl("thematic summary|Supervision summary", tx, ignore.case = TRUE)))

# ---- 5. Navigation references point at the right slides ----------------------------
for (id in c("supervision", "safety_practice", "community_status", "implementation", "reporting_presence"))
  chk("Navigation", paste("Executive findings cites", id), paste("Slide", man$order[man$id == id]), paste("Slide", man$order[man$id == id]),
      has("exec_findings", paste("Slide", man$order[man$id == id])))
for (id in man$id[man$deck == "annex"]) {
  lab <- sprintf("annex %s, slide %d", man$annex_label[man$id == id], man$order[man$id == id])
  chk("Navigation", paste("Reference to", id), lab, if (any(grepl(lab, tx, fixed = TRUE))) lab else "missing", any(grepl(lab, tx, fixed = TRUE)))
}

# ---- 6. Outputs ----------------------------------------------------------------------
final <- file.path(out_dir, sprintf("Deck_B_EPI_Integrated_v7_%s.pptx", tag))
file.copy(f_deck, final, overwrite = TRUE)
mp <- epi_review_v6_mapping(man)
utils::write.csv(dups, file.path(out_dir, "Deck_B_v7_duplicate_facility_months.csv"), row.names = FALSE)
utils::write.csv(man[, c("order", "part", "id", "deck", "annex_label", "kicker", "title", "module", "period", "map_type", "indicators", "v6_source")],
                 file.path(out_dir, "Deck_B_v7_manifest.csv"), row.names = FALSE)
utils::write.csv(mp, file.path(out_dir, "Deck_B_v7_slide_mapping.csv"), row.names = FALSE)
utils::write.csv(lg, file.path(out_dir, "Deck_B_v7_build_log.csv"), row.names = FALSE)
utils::write.csv(res, file.path(out_dir, "Deck_B_v7_qa_results.csv"), row.names = FALSE)

md_table <- function(df) {
  df[] <- lapply(df, function(x) gsub("\\|", "/", as.character(x)))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"), paste0("|", strrep("---|", ncol(df))),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
dup_log <- c(
  "v6 8 (health-center thematic table) + v6 25 (8 thematic maps): the table is replaced by the 8-map grid; each map title carries the national score and the lowest region (the table's values). Exact question values: annex A1.",
  "v6 9 (supervision table) + v6 10 (2 mobile maps) + v6 26 (4 supervision maps): one slide with 4 tiles (rate, n/N, period) above the 4 provincial maps; the separate mobile-map slide is no longer needed.",
  "v6 11 (session thematic table) + v6 27 (5 thematic maps): the table is replaced by the 5-map grid with national score and lowest region in each title; the sixth cell lists the weakest administration / safety / readiness questions.",
  "v6 12 (safety KPI table) + v6 13 (exception map) + v6 28 (7 safety-item maps): one slide - the 7 item maps show where each problem was; the 8th cell lists the sessions to follow up (sessions with a problem in the 6 priority items, expired / VVM counts, provinces to verify first, low-sample provinces not ranked). The combined exception map is not drawn (it repeated the item maps); every flagged session is listed in annex A2.",
  "v6 14 (vaccination status by region table) + v6 15 (Penta-3 map) + v6 29 (8 antigen maps): one 8-map grid (national %, n/N, lowest province in each title) with the Measles-2 caveat, P1-P3 difference and negative-difference note.",
  "v6 4 (3 checklist-reach maps) + v6 5 (district-status map): one district-status map with a districts-by-checklist-type table.",
  "v6 6 (implementation table) + v6 7 (2 completion maps): national planned / conducted / completion in tiles above the 2 maps.",
  "v6 17 (year-to-date key numbers) + v6 18 (checklists per month): one chart + KPI slide.",
  "v6 2 (at a glance) + v6 3 (action points table): executive findings slide (tiles, five findings, decision, evidence slide); detailed tracker after the evidence.",
  "Single deck: no separate management-only or map-annex export.",
  "v0.7.0: the health-center slide no longer maps the two supervision averages (the supervision slide maps the four indicators); vaccinator training (12 months) is mapped instead.",
  "v0.7.0: one divider slide per checklist (health center, session, community) and for the year-to-date part; the cover lists every part with its slide range.")
md <- c("# Deck B v7 (EPI supportive-supervision deck) - validation and change log", "",
        sprintf("Report month %s; year to date %s. Generated %s by Scripts/03_review_deck_qa.R from the cached Kobo data (data as of %s).",
                format(as.Date(paste0(ref, "-01")), "%B %Y"), paste(month.abb[1], "-", epi_month_label(ref)), format(Sys.time(), "%d %b %Y %H:%M"),
                format(as.Date(d$meta$fetched_at), "%d %b %Y")), "",
        "## Final count", "",
        sprintf("- v6: 29 slides (24 management + 5 map annex). v7: **%s slides** in one deck = %s content slides + %s part dividers + %s technical annex (tables).",
                n_deck, sum(man$deck == "main") - n_div, n_div, sum(man$deck == "annex")),
        "- Parts: ", paste0("  - ", unique(man$part), ": slides ", vapply(unique(man$part), function(p) .rv_part_range(man, p), character(1))),
        "- v6 tables are replaced by provincial map grids; the two health-center supervision averages are not mapped twice (the supervision slide maps the four indicators).",
        sprintf("- QA checks: %s passed, %s failed (details below).", sum(res$status == "PASS"), sum(res$status == "FAIL")), "",
        "## v7 slide manifest", "",
        md_table(data.frame(Slide = man$order, Part = ifelse(man$deck == "annex", paste("Annex", man$annex_label), "Main"),
                            Title = man$title, Module = man$id, Visual = man$map_type, `v6 source` = man$v6_source, check.names = FALSE)), "",
        "## Before -> after slide mapping", "", md_table(mp), "",
        "## Duplication-removal log", "", paste0("- ", dup_log), "",
        "## Notes written by the modules for this month", "", md_table(lg), "",
        "## Reconciliation and structural checks", "",
        "Figures are recomputed independently (v6 formulas) and must appear on the v7 slide; where the v6 deck is present the same figure must also appear on its v6 slide. Map values are compared province by province with the v6 map logic.", "",
        md_table(res), "",
        "## Duplicate facility-months (session completion)", "",
        "One rule at every level: a facility-month (District Code + facility + year + month) counts once, the first submitted record is kept, before splitting by region / province. National totals are unchanged from v6; provincial / regional values changed where v6 maps counted duplicates (listed in the checks above). Records dropped by the rule are kept here for verification (kept = FALSE); pairs whose values differ should be checked at the facility:", "",
        if (nrow(dups)) md_table(dups) else "None.", "",
        "## Community age range (12-23 months)", "",
        "The community checklist records one cumulative count per assessment (\"Number of Children checked in Community aged 12-23 Monhts\") and the antigen counts; neither the export nor the prepared data has a child-level age or date of birth. Submissions of 24-month-old children therefore cannot be identified or excluded from the data; the 12-23 month range is as labelled by the form. No value was changed. Internal consistency check (antigen count above the children checked) is reported in the checks above.", "",
        "## Unchanged rules", "",
        "- XLSForm, historical records, source totals and indicator definitions unchanged. Completion: one facility-month dedup rule at every level (national values identical to v6).",
        "- Expired vaccine / VVM reverse-coded in scores, absolute exception counts always shown; mobile supervision restricted to facilities with mobile sessions planned; community status uses assessed children (not population coverage); reasons use the assessment denominator; Measles-2 eligibility caveat and the negative Penta-1/Penta-3 difference kept.",
        "- Approved action geography: Northeast for mobile supervision; eligible safety priorities ranked by exceptions (3+ sessions); lowest eligible Penta-3 province (10+ children).")
writeLines(md, file.path(out_dir, sprintf("Deck_B_EPI_Integrated_v7_%s_Validation.md", tag)), useBytes = TRUE)

print(res[, c("area", "check", "status")], right = FALSE)
cat(sprintf("\n%d passed, %d failed | %d slides\nWrote %s\n", sum(res$status == "PASS"), sum(res$status == "FAIL"), n_deck, final))
unlink(tmp, recursive = TRUE)
if (any(res$status == "FAIL")) quit(status = 1)
