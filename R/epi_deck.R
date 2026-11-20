# epi_deck.R --------------------------------------------------------------
# PowerPoint export of the three dashboard pages for the current filters.
# Template: reference/epi_template.pptx (PEI slide master, 13.33 x 7.5 in).
# Charts are native PowerPoint charts (editable, data inside); maps are PNG
# pictures (300 dpi). Every content slide has a footer and the "Data as of" date.
# No speaker notes (removed in v0.4.0).
#
# Order: title -> for each selected page: overview (map + visits) -> indicator
# slides -> summary table. Community: overview -> coverage overall -> coverage
# by geography -> coverage table -> reasons not vaccinated.

.EPI_SW <- 13.333
.EPI_MASTER <- "1_PEItemplate"

epi_text <- function(pres, text, left, top, width, height, size = 12, color = "#404040",
                     bold = FALSE, italic = FALSE, align = "left", bg = NULL) {
  par <- officer::fpar(officer::ftext(text, epi_fp(size, color, bold, italic)),
                       fp_p = officer::fp_par(text.align = align, padding = 2))
  loc <- if (is.null(bg)) officer::ph_location(left = left, top = top, width = width, height = height)
         else officer::ph_location(left = left, top = top, width = width, height = height, bg = bg)
  officer::ph_with(pres, officer::block_list(par), location = loc)
}

.epi_at <- function(pres, x, left, top, width, height) {
  officer::ph_with(pres, x, location = officer::ph_location(left = left, top = top, width = width, height = height))
}

epi_as_of_label <- function(x) {
  if (is.null(x) || all(is.na(x))) return("")
  paste("Data as of", format(as.Date(x), "%d %b %Y"))
}

epi_footer <- function(pres, ctx, note = NULL) {
  txt <- paste(c(ctx$source_note, ctx$filter_label, note), collapse = "  |  ")
  pres <- epi_text(pres, txt, 0.2, 7.15, 10.7, 0.26, size = 8, color = "#595959")
  epi_text(pres, epi_as_of_label(ctx$as_of), 10.95, 7.12, 2.25, 0.3, size = 10, italic = TRUE,
           color = "#262626", align = "right")
}

epi_content_slide <- function(pres, title, ctx, note = NULL) {
  pres <- officer::add_slide(pres, layout = "Standard with No Body", master = .EPI_MASTER)
  n <- nchar(title)
  sz <- if (n > 80) 21 else if (n > 70) 23 else if (n > 60) 25 else 28
  ttl <- officer::fpar(officer::ftext(title, officer::fp_text_lite(font.size = sz)),
                       fp_p = officer::fp_par_lite(text.align = "center"))
  pres <- officer::ph_with(pres, ttl, location = officer::ph_location_type("title"))
  epi_footer(pres, ctx, note)
}

epi_no_data <- function(pres, msg = "No visits for this selection") {
  epi_text(pres, msg, 0.5, 3.4, 12.3, 0.6, size = 18, color = "#7F7F7F", align = "center")
}

epi_ft <- function(df, pct_cols = character(), font_size = 10, width = NULL) {
  ft <- flextable::flextable(df)
  for (cc in pct_cols) ft <- flextable::set_formatter(ft, values = stats::setNames(list(function(x) .epi_pct(x)), cc))
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, pct_cols)
  if (length(num_cols)) ft <- flextable::colformat_double(ft, j = num_cols, digits = 0, big.mark = ",")
  if (length(num_cols)) ft <- flextable::colformat_int(ft, j = num_cols, big.mark = ",")
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::font(ft, fontname = epi_font, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::bg(ft, bg = "#0072BC", part = "header")
  ft <- flextable::color(ft, color = "#FFFFFF", part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, j = setdiff(names(df), names(df)[1]), align = "center", part = "all")
  ft <- flextable::padding(ft, padding = 2, part = "all")
  ft <- flextable::autofit(ft)
  if (!is.null(width)) ft <- flextable::fit_to_width(ft, max_width = width)
  ft
}

# ---- Slides -------------------------------------------------------------------
epi_slide_title <- function(pres, ctx) {
  pres <- officer::add_slide(pres, layout = "Title Slide", master = .EPI_MASTER)
  pres <- epi_text(pres, "Routine EPI Supportive Supervision", 0.6, 1.9, 12.1, 1.0, size = 40, bold = TRUE,
                   color = "#FFFFFF")
  pres <- epi_text(pres, paste(epi_pages[ctx$pages], collapse = " · "), 0.6, 2.95, 12.1, 0.6, size = 24,
                   color = "#FFFFFF")
  pres <- epi_text(pres, ctx$filter_label, 0.6, 3.55, 12.1, 0.5, size = 18, color = "#FFFFFF")
  pres <- epi_text(pres, epi_as_of_label(ctx$as_of), 0.6, 4.05, 12.1, 0.5, size = 18, italic = TRUE, color = "#FFFFFF")
  pres
}

epi_slide_divider <- function(pres, title, subtitle) {
  pres <- officer::add_slide(pres, layout = "Custom Layout", master = .EPI_MASTER)
  pres <- officer::ph_with(pres, title, location = officer::ph_location_type("title"))
  par <- officer::fpar(officer::ftext(subtitle, epi_fp(22, "#FFFFFF")), fp_p = officer::fp_par(text.align = "center"))
  officer::ph_with(pres, officer::block_list(par),
                   location = officer::ph_location(left = 0.5, top = 3.85, width = 12.33, height = 1.2))
}

.epi_page_title <- function(page) {
  c(health = "Supportive Supervision (Health center)", session = "Supportive Supervision (Session)",
    community = "Coverage Assessments (Community)")[[page]]
}

.epi_map_labels <- function(page) {
  if (page == "health") c("Supervised", "Not supervised") else c("Assessed", "Not assessed")
}

# Map (left) + visits by geography (right)
epi_slide_overview <- function(pres, ctx, page) {
  d <- ctx$data[[page]]
  pres <- epi_content_slide(pres, .epi_page_title(page), ctx)
  if (!nrow(d)) return(epi_no_data(pres))
  geo <- ctx$geo_level
  png <- tempfile(fileext = ".png")
  epi_save_map(epi_map(epi_visits_by_district(d), .epi_map_labels(page), ctx$regions, base_size = 12), png, 7.4, 5.6)
  pres <- .epi_at(pres, officer::external_img(png, width = 7.4, height = 5.6), 0.25, 0.95, 7.4, 5.6)

  n_dist <- length(unique(d$DCode[!is.na(d$DCode)]))
  n_prov <- length(unique(d$Province[!is.na(d$Province)]))
  head_txt <- sprintf("%s visits  ·  %s districts  ·  %s provinces", .epi_n(nrow(d)), n_dist, n_prov)
  pres <- epi_text(pres, head_txt, 7.85, 0.95, 5.25, 0.45, size = 14, bold = TRUE, color = "#17324d")

  if (page == "community") {
    tab <- epi_visits_by(d, geo)
    ch <- tapply(d$children, d[[geo]], sum, na.rm = TRUE)
    tab$`Children checked` <- as.numeric(ch[tab$group])
    cov <- epi_coverage(d, c("Penta-3", "Measles-1"), by = geo)
    for (m in c("Penta-3", "Measles-1")) {
      s <- cov[cov$marker == m, ]
      tab[[m]] <- s$pct[match(tab$group, s$group)]
    }
    names(tab)[1:2] <- c(geo, "Visits")
    ft <- epi_ft(tab, pct_cols = c("Penta-3", "Measles-1"), font_size = if (nrow(tab) > 14) 8 else 10, width = 5.25)
  } else {
    tab <- epi_visits_by(d, geo)
    names(tab) <- c(geo, "Visits")
    tot <- data.frame(x = "Total", Visits = sum(tab$Visits)); names(tot)[1] <- geo
    tab <- rbind(tab, tot)
    ft <- epi_ft(tab, font_size = if (nrow(tab) > 16) 8 else 10, width = 5.25)
    ft <- flextable::bold(ft, i = nrow(tab), part = "body")
  }
  pres <- .epi_at(pres, ft, 7.85, 1.5, 5.25, 5.4)
  pres
}

# Two indicators per slide: stacked Yes/No by geography + overall pie
epi_slide_indicator_pair <- function(pres, ctx, page, defs) {
  d <- ctx$data[[page]]
  pres <- epi_content_slide(pres, .epi_page_title(page), ctx)
  w <- 6.3; lefts <- c(0.25, 0.25 + w + 0.28)
  for (i in seq_len(nrow(defs))) {
    L <- lefts[i]
    tab <- epi_yn_by(d, defs$key[i], ctx$geo_level)
    o <- epi_yn_overall(d, defs$key[i])
    pres <- epi_text(pres, defs$title[i], L, 0.88, w, 0.55, size = 13, bold = TRUE, color = "#17324d", align = "center")
    if (!o$Total) {
      pres <- epi_text(pres, "No answers for this selection", L, 3, w, 0.5, size = 14, color = "#7F7F7F", align = "center")
      next
    }
    pres <- .epi_at(pres, epi_ms_yesno(tab, axis_size = if (nrow(tab) > 8) 7 else 9), L, 1.45, w, 3.95)
    pres <- .epi_at(pres, epi_ms_pie(o), L + 0.6, 5.45, 1.6, 1.6)
    pres <- epi_text(pres, sprintf("Overall: %s Yes", .epi_pct(o$pct)), L + 2.35, 5.75, w - 2.45, 0.45,
                     size = 18, bold = TRUE, color = epi_colors$yes)
    pres <- epi_text(pres, sprintf("%s Yes, %s No (%s answers)", .epi_n(o$Yes), .epi_n(o$No), .epi_n(o$Total)),
                     L + 2.35, 6.2, w - 2.45, 0.4, size = 11, color = "#595959")
  }
  pres
}

epi_slide_summary <- function(pres, ctx, page, defs) {
  d <- ctx$data[[page]]
  pres <- epi_content_slide(pres, paste0(.epi_page_title(page), ": summary"), ctx)
  if (!nrow(d)) return(epi_no_data(pres))
  s <- epi_yn_summary(d, defs)
  ft <- epi_ft(s, pct_cols = "% Yes", font_size = 11, width = 12.3)
  ft <- flextable::width(ft, j = 1, width = 7.5)
  pres <- .epi_at(pres, ft, 0.5, 1.0, 12.3, 5.9)
  pres
}

epi_slide_comm_coverage <- function(pres, ctx) {
  d <- ctx$data$community
  pres <- epi_content_slide(pres, "Overall coverage by marker", ctx,
                            note = "Coverage = vaccinated / children 12-23 months checked")
  cov <- epi_coverage(d, ctx$markers, by = "Year")
  if (!nrow(cov)) return(epi_no_data(pres, "No community assessments for this selection"))
  pres <- .epi_at(pres, epi_ms_coverage_markers(cov, ctx$markers, axis_size = 11), 0.5, 0.95, 12.3, 6.0)
  pres
}

epi_slide_comm_geo <- function(pres, ctx) {
  d <- ctx$data$community
  pres <- epi_content_slide(pres, paste("Coverage by", tolower(ctx$geo_level), "and marker"), ctx,
                            note = "Coverage = vaccinated / children 12-23 months checked")
  cov <- epi_coverage(d, ctx$markers, by = ctx$geo_level)
  if (!nrow(cov)) return(epi_no_data(pres, "No community assessments for this selection"))
  pres <- .epi_at(pres, epi_ms_coverage(cov, epi_marker_colors(ctx$markers), axis_size = if (length(unique(cov$group)) > 10) 8 else 10),
                  0.3, 0.95, 12.7, 6.0)
  pres
}

epi_slide_comm_table <- function(pres, ctx) {
  d <- ctx$data$community
  pres <- epi_content_slide(pres, paste("Coverage by", tolower(ctx$geo_level), "(table)"), ctx)
  cov <- epi_coverage(d, ctx$markers, by = ctx$geo_level)
  if (!nrow(cov)) return(epi_no_data(pres, "No community assessments for this selection"))
  wide <- tidyr::pivot_wider(cov[, c("group", "children", "marker", "pct")], names_from = "marker", values_from = "pct")
  tot <- epi_coverage(d, ctx$markers)
  tot_row <- as.data.frame(c(list(group = "Total", children = tot$children[1]), as.list(stats::setNames(tot$pct, tot$marker))),
                           check.names = FALSE)
  wide <- rbind(as.data.frame(wide, check.names = FALSE), tot_row)
  names(wide)[1:2] <- c(ctx$geo_level, "Children checked")
  ft <- epi_ft(wide, pct_cols = as.character(ctx$markers), font_size = if (nrow(wide) > 14) 8 else 10, width = 12.5)
  ft <- flextable::bold(ft, i = nrow(wide), part = "body")
  pres <- .epi_at(pres, ft, 0.4, 1.0, 12.5, 5.9)
  pres
}

epi_slide_comm_reasons <- function(pres, ctx) {
  d <- ctx$data$community
  pres <- epi_content_slide(pres, "Why children have not been vaccinated", ctx,
                            note = "% of community visits that recorded each reason (several can be ticked)")
  rs <- epi_reason_summary(d)
  if (!nrow(d) || !any(rs$Visits > 0)) return(epi_no_data(pres, "No reasons recorded for this selection"))
  pres <- .epi_at(pres, epi_ms_reasons(rs), 0.5, 0.95, 12.3, 6.0)
  pres
}

# ---- Build ------------------------------------------------------------------------
# data: list(health, session, community) already filtered. ctx gets filled here.
epi_build_deck <- function(data, file, pages = names(epi_pages), filters = list(), geo_choice = "Auto",
                           markers = epi_default_markers, as_of = NA, paths = epi_paths(), progress = NULL) {
  step <- function(msg) if (is.function(progress)) progress(msg)
  ctx <- list(data = data, pages = pages, regions = filters$regions,
              geo_level = epi_geo_level(geo_choice, filters$regions), markers = markers, as_of = as_of,
              filter_label = epi_filter_label(filters),
              source_note = epi_source_note(data))
  pres <- officer::read_pptx(paths$template)
  pres <- epi_slide_title(pres, ctx)
  pair_slides <- function(pres, page, defs) {
    for (s in split(seq_len(nrow(defs)), ceiling(seq_len(nrow(defs)) / 2))) {
      pres <- epi_slide_indicator_pair(pres, ctx, page, defs[s, , drop = FALSE])
    }
    pres
  }
  divider <- function(pres, page) {
    d <- data[[page]]
    epi_slide_divider(pres, .epi_page_title(page),
                      sprintf("%s visits  ·  %s", .epi_n(nrow(d)), ctx$filter_label))
  }
  if ("health" %in% pages) {
    step("Health center slides")
    pres <- divider(pres, "health")
    pres <- epi_slide_overview(pres, ctx, "health")
    if (nrow(data$health)) {
      pres <- pair_slides(pres, "health", epi_health_indicators)
      pres <- epi_slide_summary(pres, ctx, "health", epi_health_indicators)
    }
  }
  if ("session" %in% pages) {
    step("Session slides")
    pres <- divider(pres, "session")
    pres <- epi_slide_overview(pres, ctx, "session")
    if (nrow(data$session)) {
      pres <- pair_slides(pres, "session", epi_session_indicators)
      pres <- epi_slide_summary(pres, ctx, "session", epi_session_indicators)
    }
  }
  if ("community" %in% pages) {
    step("Community slides")
    pres <- divider(pres, "community")
    pres <- epi_slide_overview(pres, ctx, "community")
    if (nrow(data$community)) {
      pres <- epi_slide_comm_coverage(pres, ctx)
      pres <- epi_slide_comm_geo(pres, ctx)
      pres <- epi_slide_comm_table(pres, ctx)
      pres <- epi_slide_comm_reasons(pres, ctx)
    }
  }
  step("Writing file")
  print(pres, target = file)
  invisible(file)
}
