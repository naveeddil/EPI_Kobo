# epi_geo.R ---------------------------------------------------------------
# Afghanistan boundaries (reference/geography/afg_geography.rds, the same file
# used by AFP_ZeroDose / PAKAFG_Surveillance) and the coverage map.
# Kobo "District Code" joins to district_code. A district is "covered"
# (supervised / assessed) when it has at least one visit in the selection.
# The same ggplot is drawn in the app (renderPlot) and saved as PNG for the deck.

.epi_geo_cache <- new.env()

epi_geo <- function(paths = epi_paths()) {
  if (!is.null(.epi_geo_cache$geo)) return(.epi_geo_cache$geo)
  suppressMessages(sf::sf_use_s2(FALSE))
  g <- readRDS(paths$geography)
  d <- g$districts
  d$district_code <- as.integer(d$district_code)
  d$region <- toupper(d$region)
  regions <- suppressMessages(suppressWarnings(
    d |> dplyr::group_by(region) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")))
  provinces <- suppressMessages(suppressWarnings(
    d |> dplyr::group_by(province, region) |> dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")))
  outline <- suppressMessages(suppressWarnings(sf::st_union(d)))
  lab <- suppressMessages(suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(regions))))
  labels <- data.frame(region = regions$region, x = lab[, 1], y = lab[, 2], stringsAsFactors = FALSE)
  geo <- list(districts = d, provinces = provinces, regions = regions, outline = outline, labels = labels)
  .epi_geo_cache$geo <- geo
  geo
}

# Codes in the data that are not in the boundary file (shown as a note under the map)
epi_unmatched_codes <- function(codes, geo = epi_geo()) {
  codes <- unique(codes[!is.na(codes)])
  sort(setdiff(codes, geo$districts$district_code))
}

# visits: data.frame(DCode, Visits). focus_regions: zoom to these regions and
# fade the rest (the Power BI map did the same when a region was selected).
epi_map <- function(visits, labels = c("Supervised", "Not supervised"), focus_regions = NULL,
                    geo = epi_geo(), legend = TRUE, base_size = 11) {
  d <- geo$districts
  d$status <- factor(ifelse(d$district_code %in% visits$DCode[visits$Visits > 0], labels[1], labels[2]),
                     levels = labels)
  focus_regions <- intersect(toupper(focus_regions), geo$regions$region)
  in_focus <- if (length(focus_regions)) d$region %in% focus_regions else rep(TRUE, nrow(d))
  d$alpha <- ifelse(in_focus, 1, 0.35)
  fills <- stats::setNames(c(epi_colors$covered, epi_colors$not_covered), labels)
  lab <- geo$labels
  lab$alpha <- if (length(focus_regions)) ifelse(lab$region %in% focus_regions, 1, 0.4) else 1

  bb <- if (length(focus_regions)) sf::st_bbox(geo$regions[geo$regions$region %in% focus_regions, ]) else sf::st_bbox(geo$outline)
  pad_x <- (bb[["xmax"]] - bb[["xmin"]]) * 0.03; pad_y <- (bb[["ymax"]] - bb[["ymin"]]) * 0.03

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = d, ggplot2::aes(fill = status, alpha = alpha), color = epi_colors$district_ln, linewidth = 0.12) +
    ggplot2::geom_sf(data = geo$provinces, fill = NA, color = epi_colors$province_ln, linewidth = 0.25) +
    ggplot2::geom_sf(data = geo$regions, fill = NA, color = epi_colors$region_ln,
                     linewidth = ifelse(geo$regions$region %in% focus_regions, 0.9, 0.5)) +
    ggplot2::geom_text(data = lab, ggplot2::aes(x = x, y = y, label = region, alpha = alpha),
                       size = base_size * 0.27, fontface = "bold", color = epi_colors$region_label) +
    ggplot2::scale_fill_manual(values = fills, drop = FALSE, name = NULL) +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_sf(xlim = c(bb[["xmin"]] - pad_x, bb[["xmax"]] + pad_x),
                      ylim = c(bb[["ymin"]] - pad_y, bb[["ymax"]] + pad_y), expand = FALSE, datum = NA) +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(legend.position = if (legend) "bottom" else "none",
                   legend.key.size = ggplot2::unit(0.9, "lines"),
                   legend.text = ggplot2::element_text(color = epi_colors$ink),
                   plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   plot.margin = ggplot2::margin(2, 2, 2, 2))
}

# Category map for the Naveed Saeed EPI deck. level = "district" or "province";
# status = category per row of that layer (factor levels = names(colors));
# text = optional label per row (drawn on provinces). Regions outside
# focus_regions are faded.
epi_map_status <- function(status, colors, level = c("district", "province"), text = NULL, focus_regions = NULL,
                           geo = epi_geo(), base_size = 11, legend = TRUE, text_size = 2.1) {
  level <- match.arg(level)
  d <- if (level == "district") geo$districts else geo$provinces
  d$status <- factor(status, levels = names(colors))
  focus_regions <- intersect(toupper(focus_regions), geo$regions$region)
  d$alpha <- if (length(focus_regions)) ifelse(d$region %in% focus_regions, 1, 0.3) else 1
  bb <- if (length(focus_regions)) sf::st_bbox(geo$regions[geo$regions$region %in% focus_regions, ]) else sf::st_bbox(geo$outline)
  pad_x <- (bb[["xmax"]] - bb[["xmin"]]) * 0.02; pad_y <- (bb[["ymax"]] - bb[["ymin"]]) * 0.02
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = d, ggplot2::aes(fill = status, alpha = alpha), color = epi_colors$district_ln,
                     linewidth = if (level == "district") 0.12 else 0.2, show.legend = TRUE)   # keep keys of empty classes
  if (level == "district") p <- p + ggplot2::geom_sf(data = geo$provinces, fill = NA, color = epi_colors$province_ln, linewidth = 0.25)
  p <- p + ggplot2::geom_sf(data = geo$regions, fill = NA, color = epi_colors$region_ln, linewidth = 0.6)
  if (!is.null(text)) {
    xy <- suppressMessages(suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(d))))
    lab <- data.frame(x = xy[, 1], y = xy[, 2], label = text, alpha = d$alpha, stringsAsFactors = FALSE)
    lab <- lab[!is.na(lab$label) & nzchar(lab$label), ]
    p <- p + ggplot2::geom_text(data = lab, ggplot2::aes(x = x, y = y, label = label, alpha = alpha), size = text_size,
                                lineheight = 0.85, color = epi_colors$ink)
  } else {
    lab <- geo$labels
    p <- p + ggplot2::geom_text(data = lab, ggplot2::aes(x = x, y = y, label = region), size = base_size * 0.27,
                                fontface = "bold", color = epi_colors$region_label)
  }
  p + ggplot2::scale_fill_manual(values = colors, drop = FALSE, name = NULL) +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_sf(xlim = c(bb[["xmin"]] - pad_x, bb[["xmax"]] + pad_x),
                      ylim = c(bb[["ymin"]] - pad_y, bb[["ymax"]] + pad_y), expand = FALSE, datum = NA) +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(legend.position = if (legend) "bottom" else "none",
                   legend.key.size = ggplot2::unit(0.9, "lines"),
                   legend.text = ggplot2::element_text(color = epi_colors$ink),
                   plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   plot.margin = ggplot2::margin(2, 2, 2, 2))
}

epi_save_map <- function(plot, file, width, height, dpi = 300) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi, device = ragg::agg_png, bg = "transparent")
  file
}
