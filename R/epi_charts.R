# epi_charts.R ------------------------------------------------------------
# On-screen charts (plotly, with hover) and native, editable PowerPoint charts
# (mschart). Both are built from the same summary tables in epi_indicators.R.

.epi_pct <- function(x, digits = 0) ifelse(is.na(x), "-", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
.epi_n <- function(x) format(x, big.mark = ",", trim = TRUE)

.epi_plotly_base <- function(p, height = NULL) {
  p |>
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "zoomIn2d",
                                                                  "zoomOut2d", "pan2d", "zoom2d", "hoverCompareCartesian",
                                                                  "hoverClosestCartesian", "toggleSpikelines")) |>
    plotly::layout(font = list(family = "Aptos, Segoe UI, Arial, sans-serif", size = 12, color = epi_colors$ink),
                   paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                   hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#d9e2ec", font = list(color = epi_colors$ink)))
}

epi_plotly_empty <- function(msg = "No answers for this selection") {
  plotly::plot_ly(type = "scatter", mode = "markers", x = numeric(), y = numeric(), hoverinfo = "none") |>
    plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                   annotations = list(list(text = msg, showarrow = FALSE, xref = "paper", yref = "paper",
                                           x = 0.5, y = 0.5, font = list(color = epi_colors$muted, size = 13)))) |>
    .epi_plotly_base()
}

# 100% stacked Yes/No columns by region or province. tab = epi_yn_by().
epi_plotly_yesno <- function(tab, geo_label = "Region") {
  if (!nrow(tab)) return(epi_plotly_empty())
  tab$no_pct <- 1 - tab$pct
  lbl <- function(v) ifelse(v >= 0.07, .epi_pct(v), "")
  hov <- function(ans, v, n) sprintf("<b>%s</b><br>%s: %s (%s of %s)<extra></extra>", tab$group, ans, .epi_pct(v), .epi_n(n), .epi_n(tab$Total))
  plotly::plot_ly(tab, x = ~group) |>
    plotly::add_bars(y = ~pct, name = "Yes", marker = list(color = epi_colors$yes, line = list(color = "#ffffff", width = 1)),
                     text = lbl(tab$pct), textposition = "inside", insidetextanchor = "middle",
                     textfont = list(color = "#ffffff", size = 11), hovertemplate = hov("Yes", tab$pct, tab$Yes)) |>
    plotly::add_bars(y = ~no_pct, name = "No", marker = list(color = epi_colors$no, line = list(color = "#ffffff", width = 1)),
                     text = lbl(tab$no_pct), textposition = "inside", insidetextanchor = "middle",
                     textfont = list(color = "#ffffff", size = 11), hovertemplate = hov("No", tab$no_pct, tab$No)) |>
    plotly::layout(barmode = "stack", bargap = 0.35,
                   xaxis = list(title = "", tickangle = if (nrow(tab) > 8) -35 else 0, tickfont = list(size = 10)),
                   yaxis = list(title = "", tickformat = ".0%", range = c(0, 1), gridcolor = "#eef2f6", zeroline = FALSE),
                   legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, traceorder = "normal"),
                   margin = list(l = 40, r = 10, t = 10, b = 10)) |>
    .epi_plotly_base()
}

# Overall Yes/No donut with the Yes % in the middle (Power BI donuts).
epi_plotly_donut <- function(o) {
  if (is.null(o) || !o$Total) return(epi_plotly_empty("No answers"))
  plotly::plot_ly(labels = c("Yes", "No"), values = c(o$Yes, o$No), type = "pie", hole = 0.68, sort = FALSE,
                  direction = "clockwise", textinfo = "none",
                  marker = list(colors = c(epi_colors$yes, epi_colors$no), line = list(color = "#ffffff", width = 2)),
                  hovertemplate = "%{label}: %{value:,} (%{percent})<extra></extra>") |>
    plotly::layout(showlegend = FALSE, margin = list(l = 5, r = 5, t = 5, b = 5),
                   annotations = list(list(text = sprintf("<b>%s</b><br><span style='font-size:11px'>Yes, n=%s</span>",
                                                          .epi_pct(o$pct), .epi_n(o$Total)),
                                           showarrow = FALSE, font = list(size = 20, color = epi_colors$ink)))) |>
    .epi_plotly_base()
}

# Clustered columns of coverage %. cov = epi_coverage(); x = group column.
epi_plotly_coverage <- function(cov, colors, x_title = "") {
  if (!nrow(cov)) return(epi_plotly_empty("No community assessments for this selection"))
  p <- plotly::plot_ly()
  for (m in levels(droplevels(cov$marker))) {
    s <- cov[cov$marker == m, ]
    p <- plotly::add_bars(p, data = s, x = ~group, y = ~pct, name = m, marker = list(color = colors[[m]]),
                          text = .epi_pct(s$pct), textposition = "none",
                          hovertemplate = sprintf("<b>%%{x}</b><br>%s: %%{y:.0%%}<br>%s of %s children<extra></extra>",
                                                  m, .epi_n(s$vaccinated), .epi_n(s$children)))
  }
  p |>
    plotly::layout(barmode = "group", bargap = 0.25,
                   xaxis = list(title = x_title, type = "category", tickfont = list(size = 11)),
                   yaxis = list(title = "", tickformat = ".0%", rangemode = "tozero", gridcolor = "#eef2f6", zeroline = FALSE),
                   legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.15),
                   margin = list(l = 45, r = 10, t = 10, b = 10)) |>
    .epi_plotly_base()
}

# Year colours follow the calendar year (not the selection): current year =
# blue, earlier years lighter steps of the same hue, so a comparison reads
# "this year vs before" and a year keeps its colour when the selection changes.
epi_year_colors <- function(years) {
  years <- sort(unique(as.character(years)))
  ramp <- c("#2a78d6", "#86b6ef", "#b7d3f6", "#cde2fb")
  off <- pmin(pmax(as.integer(format(Sys.Date(), "%Y")) - as.integer(years), 0), length(ramp) - 1)
  stats::setNames(ramp[off + 1], years)
}

# Coverage by marker: one bar per marker (name under the bar), years side by
# side when several are selected, value on top of each bar.
# cov = epi_coverage(d, markers, by = "Year").
epi_plotly_coverage_markers <- function(cov, markers) {
  if (!nrow(cov)) return(epi_plotly_empty("No community assessments for this selection"))
  years <- sort(unique(as.character(cov$group)))
  cols <- epi_year_colors(years)
  m <- markers[markers %in% as.character(cov$marker)]
  p <- plotly::plot_ly()
  for (y in years) {
    s <- cov[as.character(cov$group) == y, ]
    s <- s[match(m, as.character(s$marker)), ]
    p <- plotly::add_bars(p, x = m, y = s$pct, name = y, marker = list(color = cols[[y]]),
                          text = .epi_pct(s$pct), textposition = "outside", cliponaxis = FALSE,
                          textfont = list(size = if (length(years) > 2) 9 else 11, color = epi_colors$ink),
                          customdata = sprintf("%s of %s children", .epi_n(s$vaccinated), .epi_n(s$children)),
                          hovertemplate = paste0("<b>%{x}</b> ", y, "<br>%{y:.0%} (%{customdata})<extra></extra>"))
  }
  p |>
    plotly::layout(barmode = "group", bargap = 0.25, showlegend = length(years) > 1,
                   xaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = m,
                                tickfont = list(size = 11)),
                   yaxis = list(title = "", tickformat = ".0%", range = c(0, 1.12), gridcolor = "#eef2f6", zeroline = FALSE),
                   legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
                   margin = list(l = 45, r = 10, t = 25, b = 10)) |>
    .epi_plotly_base()
}

epi_ms_coverage_markers <- function(cov, markers, axis_size = 11) {
  years <- sort(unique(as.character(cov$group)))
  cols <- epi_year_colors(years)
  m <- markers[markers %in% as.character(cov$marker)]
  df <- data.frame(marker = factor(as.character(cov$marker), levels = m), year = factor(as.character(cov$group), levels = years),
                   v = round(cov$pct, 3))
  df <- df[!is.na(df$marker), ]
  df <- df[order(df$marker, df$year), ]
  ch <- mschart::ms_barchart(df, x = "marker", y = "v", group = "year")
  ch <- mschart::chart_settings(ch, grouping = "clustered", gap_width = 60, overlap = 0)
  ch <- mschart::chart_data_fill(ch, values = cols)
  ch <- mschart::chart_data_stroke(ch, values = cols)
  ch <- mschart::chart_data_labels(ch, show_val = TRUE, position = "outEnd", num_fmt = "0%")
  ch <- mschart::chart_labels_text(ch, values = lapply(cols, function(z) epi_fp(if (length(years) > 2) 8 else 10, "#404040")))
  ch <- mschart::chart_labels(ch, title = NULL, xlab = NULL, ylab = NULL)
  ch <- mschart::chart_ax_y(ch, num_fmt = "0%", limit_min = 0, limit_max = 1)
  .epi_ms_theme(ch, legend = if (length(years) > 1) "t" else "n", axis_size = axis_size)
}

# Horizontal bars, one series (reasons children were not vaccinated).
epi_plotly_reasons <- function(rs) {
  rs <- rs[rs$Visits > 0, , drop = FALSE]
  if (!nrow(rs)) return(epi_plotly_empty("No reasons recorded for this selection"))
  rs$Reason <- factor(rs$Reason, levels = rev(rs$Reason))
  plotly::plot_ly(rs, y = ~Reason, x = ~pct, type = "bar", orientation = "h",
                  marker = list(color = epi_colors$bar_single),
                  text = .epi_pct(rs$pct), textposition = "outside", cliponaxis = FALSE,
                  hovertemplate = "<b>%{y}</b><br>%{x:.0%} of community visits (%{customdata:,})<extra></extra>",
                  customdata = rs$Visits) |>
    plotly::layout(xaxis = list(title = "", tickformat = ".0%", rangemode = "tozero", gridcolor = "#eef2f6",
                                range = c(0, max(rs$pct) * 1.18)),
                   yaxis = list(title = ""), margin = list(l = 10, r = 30, t = 10, b = 10), bargap = 0.35) |>
    .epi_plotly_base()
}

# ---- PowerPoint (mschart) -------------------------------------------------------
epi_fp <- function(size = 9, color = "#404040", bold = FALSE, italic = FALSE) {
  officer::fp_text(font.size = size, color = color, bold = bold, italic = italic, font.family = epi_font)
}
.epi_no_line <- officer::fp_border(width = 0, color = "transparent", style = "none")
.epi_grid <- officer::fp_border(width = 0.5, color = "#D9D9D9")

.epi_ms_theme <- function(ch, legend = "t", axis_size = 9) {
  mschart::chart_theme(
    ch,
    main_title = epi_fp(11, "#404040", bold = TRUE),
    axis_text_x = epi_fp(axis_size, "#595959"),
    axis_text_y = epi_fp(axis_size - 1, "#7F7F7F"),
    legend_text = epi_fp(9, "#404040"),
    grid_major_line_x = .epi_no_line, grid_major_line_y = .epi_grid,
    grid_minor_line_x = .epi_no_line, grid_minor_line_y = .epi_no_line,
    axis_ticks_x = .epi_no_line, axis_ticks_y = .epi_no_line,
    chart_background = "transparent", plot_background = "transparent",
    chart_border = .epi_no_line, plot_border = .epi_no_line,
    legend_position = legend
  )
}

# 100% stacked Yes/No columns; values are shares so the labels read as %.
epi_ms_yesno <- function(tab, axis_size = 9) {
  long <- rbind(data.frame(group = tab$group, answer = "Yes", v = round(tab$pct, 3), stringsAsFactors = FALSE),
                data.frame(group = tab$group, answer = "No", v = round(1 - tab$pct, 3), stringsAsFactors = FALSE))
  long$group <- factor(long$group, levels = sort(unique(tab$group)))
  long$answer <- factor(long$answer, levels = c("Yes", "No"))
  long <- long[order(long$group, long$answer), ]
  cols <- c(Yes = epi_colors$yes, No = epi_colors$no)
  ch <- mschart::ms_barchart(long, x = "group", y = "v", group = "answer")
  ch <- mschart::as_bar_stack(ch, percent = FALSE, gap_width = 60)
  ch <- mschart::chart_data_fill(ch, values = cols)
  ch <- mschart::chart_data_stroke(ch, values = c(Yes = "#FFFFFF", No = "#FFFFFF"))
  # Labels on the Yes segment only (the No label is drawn in the No colour, so
  # it is hidden); values under 7% are formatted blank so thin segments stay clean.
  ch <- mschart::chart_data_labels(ch, show_val = TRUE, position = "ctr", num_fmt = '[<0.07]"";0%')
  ch <- mschart::chart_labels_text(ch, values = list(Yes = epi_fp(9, "#FFFFFF", bold = TRUE), No = epi_fp(8, epi_colors$no)))
  ch <- mschart::chart_labels(ch, title = NULL, xlab = NULL, ylab = NULL)
  ch <- mschart::chart_ax_y(ch, num_fmt = "0%", limit_min = 0, limit_max = 1)
  .epi_ms_theme(ch, legend = "t", axis_size = axis_size)
}

epi_ms_pie <- function(o) {
  df <- data.frame(answer = factor(c("Yes", "No"), levels = c("Yes", "No")), n = c(o$Yes, o$No))
  df <- df[df$n > 0, , drop = FALSE]
  df$answer <- droplevels(df$answer)
  cols <- c(Yes = epi_colors$yes, No = epi_colors$no)[levels(df$answer)]
  ch <- mschart::ms_piechart(df, x = "answer", y = "n")
  ch <- mschart::chart_settings(ch, vary_colors = TRUE)
  ch <- mschart::chart_data_fill(ch, values = cols)
  ch <- mschart::chart_data_stroke(ch, values = stats::setNames(rep("#FFFFFF", length(cols)), names(cols)))
  # no labels: the % is written next to the pie
  ch <- mschart::chart_data_labels(ch, show_val = FALSE, show_percent = FALSE)
  ch <- mschart::chart_labels(ch, title = NULL)
  .epi_ms_theme(ch, legend = "n")
}

# Clustered coverage columns; cov = epi_coverage(), colors = epi_marker_colors().
epi_ms_coverage <- function(cov, colors, axis_size = 9) {
  df <- data.frame(group = factor(cov$group, levels = sort(unique(as.character(cov$group)))),
                   marker = factor(as.character(cov$marker), levels = names(colors)),
                   v = round(cov$pct, 3))
  df <- df[!is.na(df$marker), ]
  df <- df[order(df$group, df$marker), ]
  present <- names(colors)[names(colors) %in% as.character(df$marker)]
  cols <- colors[present]
  ch <- mschart::ms_barchart(df, x = "group", y = "v", group = "marker")
  ch <- mschart::chart_settings(ch, grouping = "clustered", gap_width = 60, overlap = 0)
  ch <- mschart::chart_data_fill(ch, values = cols)
  ch <- mschart::chart_data_stroke(ch, values = cols)
  ch <- mschart::chart_data_labels(ch, show_val = length(unique(df$group)) <= 3, position = "outEnd", num_fmt = "0%")
  ch <- mschart::chart_labels_text(ch, values = lapply(cols, function(z) epi_fp(7, "#404040")))
  ch <- mschart::chart_labels(ch, title = NULL, xlab = NULL, ylab = NULL)
  ch <- mschart::chart_ax_y(ch, num_fmt = "0%", limit_min = 0)
  .epi_ms_theme(ch, legend = "t", axis_size = axis_size)
}

epi_ms_reasons <- function(rs) {
  rs <- rs[rs$Visits > 0, , drop = FALSE]
  rs$Reason <- factor(rs$Reason, levels = rev(rs$Reason))
  rs$v <- round(rs$pct, 3); rs$s <- "Share of community visits"
  ch <- mschart::ms_barchart(rs, x = "Reason", y = "v", group = "s")
  ch <- mschart::chart_settings(ch, dir = "horizontal", grouping = "clustered", gap_width = 50)
  ch <- mschart::chart_data_fill(ch, values = c(`Share of community visits` = epi_colors$bar_single))
  ch <- mschart::chart_data_stroke(ch, values = c(`Share of community visits` = epi_colors$bar_single))
  ch <- mschart::chart_data_labels(ch, show_val = TRUE, position = "outEnd", num_fmt = "0%")
  ch <- mschart::chart_labels_text(ch, values = epi_fp(9, "#404040"))
  ch <- mschart::chart_labels(ch, title = NULL, xlab = NULL, ylab = NULL)
  ch <- mschart::chart_ax_y(ch, num_fmt = "0%", limit_min = 0)
  .epi_ms_theme(ch, legend = "n", axis_size = 10)
}
