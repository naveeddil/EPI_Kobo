# epi_deck_review.R -------------------------------------------------------
# "Naveed Saeed EPI deck" (deck B): shared look (plain white 16:9, navy accent,
# KPI tiles, RAG tables), indicator helpers and the slide functions that v7
# reuses (thematic trends, reasons, district x month, vaccination status by
# month, cover). The slide order, the map-grid modules and the builder are in
# epi_deck_review_v7.R (manifest-driven, one deck). Charts are native
# PowerPoint charts; maps are PNG. No speaker notes. The v6 version of this
# file is in archive/code_v6/.

.RV <- list(navy = "#0B2545", blue = "#0072BC", sky = "#EAF4FB", ink = "#1F2933", muted = "#6B7785",
            line = "#D9E2EC", tile = "#F3F7FB",
            good = "#CDEBD3", warn = "#FCE8B2", bad = "#F8C9C4", na = "#F2F2F2")
.RV_GOOD <- 0.90; .RV_WARN <- 0.75

# 16:9 version of officer's plain template, built once per session
.epi_review_template <- local({
  cache <- NULL
  function() {
    if (!is.null(cache) && file.exists(cache)) return(cache)
    src <- system.file("template", "template.pptx", package = "officer")
    tmp <- tempfile("rvtpl_"); dir.create(tmp)
    utils::unzip(src, exdir = tmp)
    f <- file.path(tmp, "ppt", "presentation.xml")
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    x <- sub('<p:sldSz cx="[0-9]+" cy="[0-9]+"[^/]*/>', '<p:sldSz cx="12192000" cy="6858000"/>', x)
    writeLines(x, f, useBytes = TRUE)
    out <- tempfile(fileext = ".pptx")
    old <- setwd(tmp); on.exit(setwd(old))
    zip::zip(out, files = list.files(".", recursive = TRUE, all.files = TRUE))
    cache <<- out
    out
  }
})

.rv_rag <- function(x) ifelse(is.na(x), .RV$na, ifelse(x >= .RV_GOOD, .RV$good, ifelse(x >= .RV_WARN, .RV$warn, .RV$bad)))

# ---- Layout helpers ----------------------------------------------------------
.rv_text <- function(pres, text, l, t, w, h, size = 12, color = .RV$ink, bold = FALSE, italic = FALSE,
                     align = "left", bg = NULL) {
  # Text-box fills do not show on this plain template, so a fill is drawn as a
  # stretched solid-colour picture under the text.
  if (!is.null(bg)) pres <- .rv_rect(pres, l, t, w, h, bg)
  if (!nzchar(text)) return(pres)
  epi_text(pres, text, l, t, w, h, size = size, color = color, bold = bold, italic = italic, align = align)
}

.rv_rect_cache <- new.env()
.rv_rect <- function(pres, l, t, w, h, color) {
  f <- .rv_rect_cache[[color]]
  if (is.null(f) || !file.exists(f)) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, width = 32, height = 32)   # tiny solid swatch, stretched in PowerPoint
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
    graphics::rect(-1, -1, 2, 2, col = color, border = NA)
    grDevices::dev.off()
    .rv_rect_cache[[color]] <- f
  }
  officer::ph_with(pres, officer::external_img(f, width = w, height = h),
                   location = officer::ph_location(left = l, top = t, width = w, height = h))
}

.rv_bullets <- function(pres, lines, l, t, w, h, size = 14) {
  pars <- lapply(lines, function(s) officer::fpar(
    officer::ftext("-  ", epi_fp(size - 2, .RV$blue, bold = TRUE)), officer::ftext(s, epi_fp(size, .RV$ink)),
    fp_p = officer::fp_par(padding.bottom = 9, line_spacing = 1.1)))
  officer::ph_with(pres, do.call(officer::block_list, pars), location = officer::ph_location(left = l, top = t, width = w, height = h))
}

.rv_slide <- function(pres, ctx, kicker, title, note = NULL) {
  ctx$page_no$n <- ctx$page_no$n + 1
  # v7: a slide built from the manifest takes its kicker and title from it
  if (!is.null(ctx$mod)) { kicker <- ctx$mod$kicker; title <- ctx$mod$title }
  if (isTRUE(ctx$annex)) kicker <- paste("Technical map annex |", kicker)
  pres <- officer::add_slide(pres, layout = "Blank", master = "Office Theme")
  pres <- .rv_text(pres, "", 0, 0, 0.14, 7.5, bg = .RV$navy)
  pres <- .rv_text(pres, toupper(kicker), 0.5, 0.28, 9, 0.32, size = 11, bold = TRUE, color = .RV$blue)
  pres <- .rv_text(pres, title, 0.5, 0.58, 12.3, 0.62, size = 24, bold = TRUE, color = .RV$navy)
  pres <- .rv_text(pres, "", 0.5, 1.22, 12.33, 0.02, bg = .RV$line)
  # "All regions" is left out of the footer; a region / province selection is kept
  flt <- if (identical(ctx$filter_label, "All regions")) NULL else ctx$filter_label
  foot <- paste(c("Routine EPI Supportive Supervision | NS EPI deck", ctx$period, flt, note), collapse = "  |  ")
  pres <- .rv_text(pres, foot, 0.5, 7.08, 11.6, 0.28, size = 8, color = .RV$muted)
  .rv_text(pres, as.character(ctx$page_no$n), 12.33, 7.08, 0.5, 0.28, size = 9, color = .RV$muted, align = "right")
}

.rv_tile <- function(pres, l, t, w, h, value, label, sub = NULL, accent = .RV$blue) {
  pres <- .rv_text(pres, "", l, t, w, h, bg = .RV$tile)
  pres <- .rv_text(pres, "", l, t, 0.07, h, bg = accent)
  pres <- .rv_text(pres, value, l + 0.2, t + 0.12, w - 0.3, h * 0.48, size = 26, bold = TRUE, color = .RV$navy)
  pres <- .rv_text(pres, label, l + 0.2, t + h * 0.55, w - 0.3, 0.32, size = 11, bold = TRUE, color = .RV$ink)
  if (!is.null(sub)) pres <- .rv_text(pres, sub, l + 0.2, t + h * 0.55 + 0.3, w - 0.3, 0.3, size = 9, color = .RV$muted)
  pres
}

.rv_rag_legend <- function(pres, l, t) {
  lab <- c(sprintf("%d%%+", .RV_GOOD * 100), sprintf("%d-%d%%", .RV_WARN * 100, .RV_GOOD * 100 - 1),
           sprintf("<%d%%", .RV_WARN * 100))
  cols <- c(.RV$good, .RV$warn, .RV$bad)
  for (i in 1:3) pres <- .rv_text(pres, lab[i], l + (i - 1) * 1.05, t, 1.0, 0.3, size = 9, align = "center", bg = cols[i])
  pres
}

.rv_heat_ft <- function(df, pct_cols, font_size = 10, first_width = NULL) {
  ft <- flextable::flextable(df)
  if (length(pct_cols))
    ft <- flextable::set_formatter(ft, values = stats::setNames(rep(list(function(x) .epi_pct(x)), length(pct_cols)), pct_cols))
  num_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], pct_cols)
  if (length(num_cols)) ft <- flextable::colformat_double(ft, j = num_cols, digits = 0, big.mark = ",")
  ft <- flextable::border_remove(ft)
  ft <- flextable::font(ft, fontname = epi_font, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = .RV$navy, part = "header")
  if (length(pct_cols)) ft <- flextable::bg(ft, j = pct_cols, bg = .rv_rag, part = "body")
  ft <- flextable::align(ft, j = setdiff(names(df), names(df)[1]), align = "center", part = "all")
  ft <- flextable::hline(ft, border = officer::fp_border(color = "#FFFFFF", width = 2), part = "body")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = .RV$navy, width = 1), part = "header")
  ft <- flextable::padding(ft, padding.top = 3, padding.bottom = 3, padding.left = 4, padding.right = 4, part = "all")
  ft <- flextable::autofit(ft)
  if (!is.null(first_width)) ft <- flextable::width(ft, j = 1, width = first_width)
  ft
}

# Fit a table to the content area: shrink when too wide, otherwise stretch the
# columns to the full width (12.3 in = 92% of the slide). A table that had to
# be stretched a lot gets a 2 pt larger font; rows are stretched (up to
# max_row in) to use the available height.
.rv_fit <- function(ft, width = 12.3, height = 5.2, max_row = 0.55) {
  ft <- flextable::fit_to_width(ft, max_width = width)
  cw <- ft$body$colwidths
  k <- width / sum(cw)
  if (k > 1.3) {
    fs <- tryCatch(as.numeric(ft$body$styles$text$font.size$data[1, 1]), error = function(e) NA_real_)
    if (!is.na(fs) && fs < 12) ft <- flextable::fontsize(ft, size = min(fs + 2, 12), part = "all")
  }
  ft <- flextable::width(ft, j = seq_along(cw), width = cw * k)
  n <- flextable::nrow_part(ft, "header") + flextable::nrow_part(ft, "body")
  if (n > 0) {
    ft <- flextable::height_all(ft, height = min(max_row, height / n), part = "all")
    ft <- flextable::hrule(ft, rule = "atleast", part = "all")
  }
  ft
}

.rv_hbar <- function(df, x, y, color = .RV$blue, label_size = 10) {
  df <- as.data.frame(df)
  df[[x]] <- factor(df[[x]], levels = rev(df[[x]]))
  df$s <- "v"
  ch <- mschart::ms_barchart(df, x = x, y = y, group = "s")
  ch <- mschart::chart_settings(ch, dir = "horizontal", grouping = "clustered", gap_width = 45)
  ch <- mschart::chart_data_fill(ch, values = c(v = color))
  ch <- mschart::chart_data_stroke(ch, values = c(v = color))
  ch <- mschart::chart_data_labels(ch, show_val = TRUE, position = "outEnd", num_fmt = "0%")
  ch <- mschart::chart_labels_text(ch, values = epi_fp(label_size, .RV$ink))
  ch <- mschart::chart_labels(ch, title = NULL, xlab = NULL, ylab = NULL)
  ch <- mschart::chart_ax_y(ch, num_fmt = "0%", limit_min = 0, limit_max = 1)
  .epi_ms_theme(ch, legend = "n", axis_size = 10)
}

.rv_lines <- function(df, colors, pct = TRUE) {
  df <- as.data.frame(df)
  df$series <- factor(df$series, levels = names(colors))
  df <- df[order(df$series, df$x), ]
  ch <- mschart::ms_linechart(df, x = "x", y = "y", group = "series")
  ch <- mschart::chart_settings(ch, style = "lineMarker")
  ch <- mschart::chart_data_stroke(ch, values = colors)
  ch <- mschart::chart_data_fill(ch, values = colors)
  ch <- mschart::chart_data_line_width(ch, values = stats::setNames(rep(2.25, length(colors)), names(colors)))
  ch <- mschart::chart_data_symbol(ch, values = stats::setNames(rep("circle", length(colors)), names(colors)))
  ch <- mschart::chart_data_size(ch, values = stats::setNames(rep(5, length(colors)), names(colors)))
  ch <- mschart::chart_data_smooth(ch, values = stats::setNames(rep(0, length(colors)), names(colors)))
  ch <- mschart::chart_labels(ch, title = NULL, xlab = NULL, ylab = NULL)
  ch <- if (pct) mschart::chart_ax_y(ch, num_fmt = "0%", limit_min = 0, limit_max = 1)
        else mschart::chart_ax_y(ch, num_fmt = "#,##0", limit_min = 0)
  .epi_ms_theme(ch, legend = "b", axis_size = 9)
}

# Indicator value shown on the scorecards: VVM is "lower is better", so the
# review deck shows the share of sessions WITHOUT the problem.
.rv_defs <- function(defs) {
  defs$flip <- defs$key %in% c("vvm", "expired")
  defs$short <- if ("label" %in% names(defs)) defs$label else sub("^% of ", "", defs$title)
  defs$short[defs$key == "vvm"] <- "sessions with no vaccines in VVM stage 3/4"
  defs$short[defs$key == "expired"] <- "sessions with no expired vaccines"
  defs$short[defs$key == "pemt_supervision"] <- "PEMT outreach supervision (quarterly)"
  defs$short[defs$key == "ngo_supervision"] <- "NGO outreach supervision (quarterly)"
  defs$short[defs$key == "pemt_mobile_supervision"] <- "PEMT mobile supervision (monthly)"
  defs$short[defs$key == "ngo_mobile_supervision"] <- "NGO mobile supervision (quarterly)"
  defs$short <- sub("ngo supervisors", "NGO supervisors", sub("pemt supervisors", "PEMT supervisors", defs$short))
  defs$short <- paste0(toupper(substr(defs$short, 1, 1)), substr(defs$short, 2, nchar(defs$short)))
  defs
}
.rv_value <- function(pct, flip) ifelse(flip, 1 - pct, pct)
.rv_scoring_defs <- function(page) {
  # Multi-select session supplies are shown only as item-level descriptive data
  # after applicability is defined; they are excluded from rankings and scores.
  defs <- if (identical(page, "health")) epi_health_indicators else epi_session_indicators
  defs[!defs$key %in% "supplies", , drop = FALSE]
}
.rv_hc_categories <- list(
  "Planning and microplanning" = c("catchment_map", "target_population", "microplan", "coverage_chart"),
  "Staff and community readiness" = c("trained", "community_link", "referral"),
  "Cold-chain management" = c("cold_chain", "temperature_ok", "fridge_tag"),
  "Recording and reporting" = c("hc_tally", "hc_cards", "hc_register", "hc_monthly_report", "penta3_calc", "dropout_calc"),
  "Defaulter follow-up" = "defaulter_followup",
  "Outreach supervision" = c("pemt_supervision", "ngo_supervision"),
  "Mobile supervision" = c("pemt_mobile_supervision", "ngo_mobile_supervision"),
  "Outreach proximity (context)" = "within5km")
.rv_session_categories <- list(
  "Session readiness" = c("referred", "suitable_place", "session_trained", "contact_person", "duration_3h"),
  "Vaccine management" = c("icepacks", "expired", "vvm", "open_vial"),
  "Administration and safety" = c("safety_box", "technique", "aefi_aware"),
  "Recording" = c("tally_sheet", "imm_cards", "session_register"),
  "Caregiver communication" = c("msg_importance", "msg_reactions", "next_contact", "card_retention", "caregiver_feedback"))
.rv_applicable <- function(d, key) {
  if (key %in% c("pemt_mobile_supervision", "ngo_mobile_supervision") && "mobile_planned" %in% names(d))
    d <- d[!is.na(d$mobile_planned) & d$mobile_planned > 0, , drop = FALSE]
  d
}
.rv_overall <- function(d, key) epi_yn_overall(.rv_applicable(d, key), key)
.rv_matrix <- function(d, defs, by) {
  do.call(rbind, lapply(seq_len(nrow(defs)), function(i) {
    x <- epi_yn_by(.rv_applicable(d, defs$key[i]), defs$key[i], by)
    if (!nrow(x)) return(NULL)
    data.frame(key = defs$key[i], title = defs$title[i], group = x$group, pct = x$pct, Total = x$Total,
               stringsAsFactors = FALSE)
  }))
}
.rv_category_table <- function(d, cats, by = "Region") {
  groups <- sort(unique(d[[by]][!is.na(d[[by]])]))
  do.call(rbind, lapply(names(cats), function(nm) {
    keys <- cats[[nm]]
    vals <- vapply(keys, function(k) {
      o <- .rv_overall(d, k); flip <- k %in% c("expired", "vvm")
      .rv_value(o$pct, flip)
    }, numeric(1))
    gv <- vapply(groups, function(g) {
      x <- d[d[[by]] == g, , drop = FALSE]
      mean(vapply(keys, function(k) .rv_value(.rv_overall(x, k)$pct, k %in% c("expired", "vvm")), numeric(1)), na.rm = TRUE)
    }, numeric(1))
    data.frame(Category = nm, National = mean(vals, na.rm = TRUE), `Lowest area` = groups[which.min(gv)],
               `Lowest score` = min(gv, na.rm = TRUE), Questions = length(keys), check.names = FALSE)
  }))
}

.rv_category_trends <- function(pres, ctx, page, cats, title) {
  d <- ctx$data[[page]]
  trend_note <- if (identical(page, "health"))
    "Equal-weight category scores; assessed facilities may differ by month; mobile supervision requires planned activity > 0" else
    "Equal-weight valid-response averages; assessed sessions and facilities may differ across months"
  pres <- .rv_slide(pres, ctx, paste("Year to date |", ctx$ytd_label), title,
                    note = trend_note)
  if (!nrow(d)) return(epi_no_data(pres))
  tab <- data.frame(Category = names(cats), stringsAsFactors = FALSE)
  for (mon in ctx$months) {
    x <- d[format(d$VisitDate, "%Y-%m") == mon, , drop = FALSE]
    tab[[epi_month_label(mon)]] <- vapply(cats, function(keys) mean(vapply(keys, function(k)
      .rv_value(.rv_overall(x, k)$pct, k %in% c("expired", "vvm")), numeric(1)), na.rm = TRUE), numeric(1))
  }
  tab$YTD <- .rv_category_table(d, cats, ctx$geo_level)$National
  pc <- setdiff(names(tab), "Category")
  ft <- .rv_heat_ft(tab, pc, font_size = 9, first_width = 3.0)
  ft <- flextable::bold(ft, j = "YTD", part = "all")
  pres <- .epi_at(pres, .rv_fit(ft, 12.3, 5.25), 0.5, 1.45, 12.3, 5.25)
  .rv_rag_legend(pres, 9.65, 6.75)
}

# ---- Slides --------------------------------------------------------------------
.rv_cover <- function(pres, ctx) {
  pres <- officer::add_slide(pres, layout = "Blank", master = "Office Theme")
  pres <- .rv_text(pres, "", 0, 0, 13.333, 7.5, bg = .RV$navy)
  pres <- .rv_text(pres, "", 0.8, 2.05, 0.12, 2.3, bg = "#00A6D6")
  pres <- .rv_text(pres, paste("NAVEED SAEED EPI DECK |", toupper(ctx$ref_label)), 1.15, 1.95, 10, 0.45, size = 14,
                   bold = TRUE, color = "#00A6D6")
  pres <- .rv_text(pres, "Routine EPI Supportive Supervision", 1.15, 2.4, 11.5, 0.9, size = 38, bold = TRUE, color = "#FFFFFF")
  pres <- .rv_text(pres, sprintf("Last month (%s) and year to date (%s)", ctx$ref_label, ctx$ytd_label), 1.15, 3.25, 11.5,
                   0.55, size = 20, color = "#D6E4F0")
  pres <- .rv_text(pres, sprintf("Health center, session and community checklists  |  %s", ctx$filter_label),
                   1.15, 3.85, 11.5, 0.45, size = 14, color = "#D6E4F0")
  if (!is.null(ctx$contents)) {     # v7: contents = parts of the deck with slide ranges (two columns)
    cc <- ctx$contents; half <- ceiling(nrow(cc) / 2)
    pres <- .rv_text(pres, "CONTENTS", 1.15, 4.5, 4, 0.32, size = 11, bold = TRUE, color = "#00A6D6")
    for (i in seq_len(nrow(cc))) {
      l <- if (i <= half) 1.15 else 6.4; t <- 4.86 + ((i - 1) %% half) * 0.38
      pres <- .rv_text(pres, "", l, t + 0.07, 0.08, 0.22, bg = cc$color[i])
      pres <- .rv_text(pres, cc$range[i], l + 0.18, t, 0.9, 0.34, size = 12, bold = TRUE, color = "#FFFFFF")
      pres <- .rv_text(pres, cc$part[i], l + 1.1, t, 4.2, 0.34, size = 12, color = "#D6E4F0")
    }
  }
  pres <- .rv_text(pres, paste(epi_as_of_label(ctx$as_of), "|", ctx$source_note), 1.15, 6.6, 11.5, 0.4,
                   size = 11, italic = TRUE, color = "#9FB3C8")
  pres
}

.rv_reasons <- function(pres, ctx) {
  d <- ctx$data$community
  pres <- .rv_slide(pres, ctx, "Community checklist", "Reasons Reported During Community Assessments",
                    note = sprintf("%% of %s community assessments recording each reason; checklist-level multi-select denominator",
                                   ctx$ytd_label))
  rs <- epi_reason_summary(d)
  rs <- rs[rs$Visits > 0, , drop = FALSE]
  if (!nrow(rs)) return(epi_no_data(pres, "No reasons recorded for this selection"))
  r <- data.frame(Reason = rs$Reason, v = round(rs$pct, 3), stringsAsFactors = FALSE)
  ch <- .rv_hbar(r, "Reason", "v", color = "#eb6834")
  ch <- mschart::chart_ax_y(ch, num_fmt = "0%", limit_min = 0, limit_max = min(1, max(r$v) * 1.25 + 0.02))
  pres <- .epi_at(pres, ch, 0.4, 1.4, 8.7, 5.6)
  any_reason <- sum(rowSums(vapply(epi_reasons$key, function(k) !is.na(d[[k]]) & d[[k]] > 0, logical(nrow(d)))) > 0)
  pres <- .rv_tile(pres, 9.35, 1.5, 3.5, 1.25, .epi_n(nrow(d)), "Community assessments", ctx$ytd_label, "#eb6834")
  pres <- .rv_tile(pres, 9.35, 2.9, 3.5, 1.25, .epi_n(any_reason), "Recorded at least one reason",
                   sprintf("%s of assessments", .epi_pct(any_reason / nrow(d))), "#eb6834")
  .rv_bullets(pres, c("Bar = share of community assessments that recorded the reason.",
                      "One assessment can record several reasons, so bars do not add up to 100%.",
                      "Reasons describe assessments, not individual children."),
              9.35, 4.35, 3.5, 2.5, size = 11)
}

.rv_delta <- function(now, before) {
  if (is.na(before) || before == 0) return("no data the month before")
  d <- (now - before) / before
  sprintf("%s%s vs %s (%s)", if (d >= 0) "+" else "", .epi_pct(d), "previous month", .epi_n(before))
}

# Red (0%) -> amber -> white -> light blue (100%), for shares of districts reporting
.rv_share_ramp <- function(x) {
  ramp <- grDevices::colorRampPalette(c(.RV$bad, .RV$warn, "#FFFFFF", "#9ec5f4"))(101)
  ramp[pmin(101, pmax(1, round(100 * x) + 1))]
}

# Region x month: districts with at least one checklist (shaded by share)
.rv_district_region <- function(pres, ctx) {
  pres <- .rv_slide(pres, ctx, paste("Year to date |", ctx$ytd_label), "Districts reporting by region and month",
                    note = "Cell = districts with at least one checklist (any type) / districts in the region")
  t <- epi_district_region_month(ctx$data, ctx$months, ctx$regions, ctx$provinces)
  if (is.null(t) || !nrow(t)) return(epi_no_data(pres))
  mc <- c(epi_month_label(ctx$months), "Any month")
  share <- as.matrix(t[, mc, drop = FALSE]) / t$Districts
  show <- t
  for (j in mc) show[[j]] <- sprintf("%s (%s)", t[[j]], .epi_pct(t[[j]] / t$Districts))
  ft <- flextable::flextable(show)
  for (j in seq_along(mc))
    ft <- flextable::bg(ft, j = mc[j], part = "body", bg = ifelse(is.na(share[, j]), "#FFFFFF", .rv_share_ramp(share[, j])))
  ft <- flextable::bold(ft, i = nrow(t), part = "body"); ft <- flextable::bold(ft, j = "Any month", part = "all")
  ft <- flextable::font(ft, fontname = epi_font, part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::bold(ft, part = "header"); ft <- flextable::color(ft, color = .RV$navy, part = "header")
  ft <- flextable::align(ft, j = setdiff(names(show), "Region"), align = "center", part = "all")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(ft, border = officer::fp_border(color = "#FFFFFF", width = 1.5), part = "body")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = .RV$navy, width = 1), part = "header")
  ft <- flextable::hline(ft, i = nrow(t) - 1, border = officer::fp_border(color = .RV$navy, width = 1), part = "body")
  ft <- flextable::padding(ft, padding = 3, part = "all")
  ft <- .rv_fit(flextable::autofit(ft), 12.3, 4.9)
  pres <- .epi_at(pres, ft, 0.5, 1.45, 12.3, 4.9)
  .rv_text(pres, paste("Red = few districts reporting that month, blue = most districts. A district counts when any",
                       "health-center, session or community checklist has its District Code."),
           0.5, 6.55, 12.3, 0.4, size = 10, italic = TRUE, color = .RV$muted)
}

.rv_cov_month <- function(pres, ctx) {
  d <- ctx$data$community
  pres <- .rv_slide(pres, ctx, paste("Year to date |", ctx$ytd_label), "Vaccination status among assessed children by month",
                    note = "Not population coverage; assessed children may differ by month; final row is a cross-sectional Penta-1 to Penta-3 proxy")
  if (!nrow(d)) return(epi_no_data(pres, "No community assessments for this selection"))
  mk <- c("Vaccination card", "BCG", "Penta-1", "Penta-3", "PCV-3", "OPV-3", "Rota-1", "Measles-1", "Measles-2")
  m <- epi_coverage_month_matrix(d, mk, ctx$months)
  all <- epi_coverage(d, mk); dr <- epi_dropout(d)
  m$YTD <- c(all$pct[match(mk, as.character(all$marker))], if (nrow(dr)) dr$dropout[1] else NA)
  pc <- setdiff(names(m), "Marker")
  ft <- .rv_heat_ft(m, pc, font_size = 9, first_width = 2.4)
  ft <- flextable::bg(ft, i = nrow(m), j = pc, bg = "#FFFFFF", part = "body")
  ft <- flextable::set_formatter(ft, values = stats::setNames(rep(list(function(x) .epi_pct(x, 0)), length(pc)), pc))
  ft <- flextable::italic(ft, i = nrow(m), part = "body")
  ft <- flextable::bold(ft, j = "YTD", part = "all")
  ft <- .rv_fit(ft, 12.3, 5.2)
  pres <- .epi_at(pres, ft, 0.5, 1.45, 12.3, 5.2)
  pres <- .rv_rag_legend(pres, 9.65, 6.7)
  pres
}

