# Routine EPI Supportive Supervision dashboard -------------------------------
# Live replacement of the Power BI report "Routine_EPI_Dashboard": data comes
# straight from KoboToolbox (export EPI_BACKUP) when "Refresh from Kobo" is
# pressed. Three pages (Health center, Session, Community) + PowerPoint export.
# Look and feel copied from PAKAFG_Surveillance / AFP_ZeroDose (www/app.css,
# WHO logo, page banners, content cards). Launch with source("RUN_WEB_APP.R").

library(shiny)
library(bslib)
library(DT)

epi_app_root <- normalizePath(Sys.getenv("EPI_PROJECT_ROOT", getwd()), winslash = "/")
if (file.exists(file.path(epi_app_root, ".Renviron"))) readRenviron(file.path(epi_app_root, ".Renviron"))
source(file.path(epi_app_root, "R", "epi_load.R"))
epi_source_all(epi_app_root)
app_version <- epi_version(epi_app_root)

# ---- UI helpers (same pattern as PAKAFG_Surveillance / AFP_ZeroDose) ---------------
page_banner <- function(title, subtitle, ...) {
  tags$div(class = "page-banner", tags$div(tags$h2(title), tags$p(subtitle)),
           tags$div(class = "epi-banner-actions", ...))
}

content_card <- function(title, subtitle = NULL, ...) {
  tags$div(class = "content-card", tags$h3(title),
           if (!is.null(subtitle)) tags$p(class = "card-subtitle", subtitle), ...)
}

metric_static <- function(label, value, note = NULL, color = NULL) {
  tags$div(class = "metric-card",
           tags$div(class = "metric-label", label),
           tags$div(class = "metric-value", style = if (!is.null(color)) paste0("color:", color), value),
           if (!is.null(note)) tags$div(class = "metric-note", note))
}

metric_grid <- function(...) tags$div(class = "epi-metric-grid", ...)

app_theme <- bs_theme(
  version = 5, bootswatch = "flatly",
  primary = "#0072BC", secondary = "#64748B", success = "#2E8540", danger = "#D71920", warning = "#F3A712",
  base_font = font_collection("Aptos", "Segoe UI", "Arial", "sans-serif")
)

asset_cache_bust <- function(relative_path) {
  f <- file.path(epi_app_root, "www", relative_path)
  paste0(relative_path, "?v=", if (file.exists(f)) as.integer(file.info(f)$mtime) else as.integer(Sys.time()))
}

# One card per Yes/No indicator: bars by region/province + overall donut
indicator_card <- function(prefix, def) {
  id <- paste0(prefix, "_", def$key)
  column(6, content_card(def$title, NULL,
    fluidRow(
      column(8, plotly::plotlyOutput(paste0(id, "_bar"), height = "270px")),
      column(4, plotly::plotlyOutput(paste0(id, "_donut"), height = "230px"))
    )))
}

indicator_grid <- function(prefix, defs) {
  cards <- lapply(seq_len(nrow(defs)), function(i) indicator_card(prefix, defs[i, ]))
  tagList(lapply(split(cards, ceiling(seq_along(cards) / 2)), function(r) do.call(fluidRow, r)))
}

ppt_button <- function(id, label = "Export this page to PowerPoint") {
  downloadButton(id, label, class = "btn-light btn-sm epi-ppt-btn", icon = icon("file-powerpoint"))
}

filter_bar <- tags$div(
  class = "epi-filter-bar",
  tags$div(class = "epi-filters",
    selectizeInput("f_region", "Region", choices = NULL, multiple = TRUE, width = "210px",
                   options = list(placeholder = "All regions", plugins = list("remove_button"))),
    selectizeInput("f_province", "Province", choices = NULL, multiple = TRUE, width = "210px",
                   options = list(placeholder = "All provinces", plugins = list("remove_button"))),
    selectizeInput("f_year", "Year", choices = NULL, multiple = TRUE, width = "150px",
                   options = list(placeholder = "All years", plugins = list("remove_button"))),
    selectizeInput("f_month", "Month", choices = month.name, multiple = TRUE, width = "210px",
                   options = list(placeholder = "All months", plugins = list("remove_button"))),
    tags$div(title = "Auto = by region, or by province when exactly one region is selected",
             selectInput("f_geo", "Bars by", width = "130px",
                         choices = c("Auto" = "Auto", "Region" = "Region", "Province" = "Province"))),
    actionButton("f_reset", "Reset", icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm epi-reset")
  ),
  tags$div(class = "epi-data-status", uiOutput("data_status")),
  uiOutput("hist_note", class = "epi-hist-note")
)

dashboard_page <- function(prefix, title, subtitle, map_title, map_subtitle, defs) {
  tags$div(class = "app-shell",
    page_banner(title, subtitle, ppt_button(paste0("ppt_", prefix))),
    uiOutput(paste0(prefix, "_metrics")),
    fluidRow(
      column(7, content_card(map_title, map_subtitle,
                             plotOutput(paste0(prefix, "_map"), height = "470px"),
                             uiOutput(paste0(prefix, "_map_note")))),
      column(5, content_card("Total visits", "By region, or by province when one region is selected.",
                             DTOutput(paste0(prefix, "_visits"))))
    ),
    indicator_grid(prefix, defs)
  )
}

# ---- UI ---------------------------------------------------------------------------
ui <- tagList(
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = asset_cache_bust("who-logo.svg")),
    tags$link(rel = "stylesheet", type = "text/css", href = asset_cache_bust("app.css")),
    tags$style(HTML("
      .epi-metric-grid {display:grid; grid-template-columns:repeat(auto-fit, minmax(170px, 1fr)); gap:14px; margin-bottom:18px}
      .epi-filter-bar {display:flex; flex-wrap:wrap; justify-content:space-between; align-items:flex-end; gap:12px;
                       background:#fff; border-bottom:1px solid var(--line); padding:10px 24px 2px 24px;
                       box-shadow:0 1px 6px rgba(13,53,82,.06); position:sticky; top:0; z-index:1000}
      .epi-filters {display:flex; flex-wrap:wrap; gap:10px; align-items:flex-end}
      .epi-filters .form-group {margin-bottom:8px}
      .epi-filters .selectize-input {min-height:34px; padding:5px 8px}
      .epi-filters .form-group label {font-size:12px; font-weight:600; color:var(--muted); margin-bottom:2px}
      .epi-reset {margin-bottom:10px}
      .epi-data-status {display:flex; align-items:center; gap:10px; margin-bottom:10px; font-size:13px; color:var(--muted)}
      .epi-data-status .status-live {color:var(--who-green); font-weight:600}
      .epi-data-status .status-snapshot {color:#B9770E; font-weight:600}
      .epi-hist-note {flex-basis:100%; font-size:12px; color:#8a5a00}
      .epi-hist-note:not(:empty) {background:#fff7e6; border:1px solid #f3d9a4; border-radius:6px; padding:4px 10px;
                                  margin:0 0 8px 0}
      .epi-banner-actions {display:flex; gap:8px; align-items:center}
      .page-banner {display:flex; justify-content:space-between; align-items:center; gap:16px; flex-wrap:wrap}
      .content-card .plotly {margin-bottom:0}
      .epi-note {font-size:12px; color:var(--muted); margin-top:4px}
      .epi-subhead {font-size:16px; font-weight:700; color:var(--ink); margin:14px 0 2px 0}
      .epi-section {font-size:20px; font-weight:700; color:var(--who-blue-dark); margin:22px 0 10px 2px}
      @media (max-width: 768px) {.epi-filter-bar {position:static; padding:8px 16px} .epi-filters .form-group {min-width:140px}}
    ")),
    tags$script(HTML("
      $(document).on('click', '#refresh', function() {
        var b = $(this); if (b.prop('disabled')) return;
        b.data('label', b.html()); b.prop('disabled', true).html('Downloading from Kobo...');
      });
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('epi-refresh-done', function(x) {
          var b = $('#refresh'); b.prop('disabled', false); if (b.data('label')) b.html(b.data('label'));
        });
      });
    ")),
    tags$title("Routine EPI Dashboard")
  ),

  navbarPage(
    title = tags$span(class = "navbar-brand-content",
                      tags$img(src = asset_cache_bust("who-logo.svg"), alt = "WHO logo", class = "navbar-who-logo"),
                      tags$span(class = "navbar-brand-sep", "|"), "ROUTINE EPI",
                      tags$span(class = "navbar-menu-version", paste0("v", app_version))),
    id = "main_nav", theme = app_theme, collapsible = TRUE, windowTitle = "Routine EPI Dashboard",
    selected = Sys.getenv("EPI_START_TAB", "home"),
    header = filter_bar,

    tabPanel("Home", value = "home", icon = icon("house"),
      tags$div(class = "app-shell",
        page_banner("Routine EPI Supervision - Summary",
                    "All three checklists together: how many were submitted, where and when."),
        content_card(textOutput("home_last_title", inline = TRUE),
                     "Last complete month, for the region / province filters (year and month filters do not apply here).",
                     uiOutput("home_last_metrics"),
                     tags$h4(class = "epi-subhead", "Reporting compliance by region"),
                     tags$p(class = "card-subtitle", "Provinces with at least one checklist of that type in the month, out of",
                            " the provinces in the region (denominator, from the boundary file; 34 for all Afghanistan)."),
                     DTOutput("home_last_compliance")),
        tags$h3(class = "epi-section", textOutput("home_period_title", inline = TRUE)),
        uiOutput("home_metrics"),
        content_card("Checklists submitted per month", "Stacked by checklist type (month of the visit date).",
                     plotly::plotlyOutput("home_month", height = "330px")),
        fluidRow(
          column(5, content_card("Reporting compliance by month",
                                 "Provinces with at least one checklist of that type / provinces in the selected regions (34 for all Afghanistan).",
                                 plotly::plotlyOutput("home_comp_trend", height = "360px"))),
          column(7, content_card("Compliance: checklists by province and month",
                                 "Red = no checklist that month; amber = fewer than 5.",
                                 radioButtons("comp_pages", NULL, inline = TRUE,
                                              choices = c("All checklists" = "all", setNames(names(epi_pages), epi_pages))),
                                 DTOutput("home_comp_grid")))),
        content_card("Responses by month for each region",
                     "Number of checklists per region and month. Darker = more.",
                     radioButtons("home_pages", NULL, inline = TRUE,
                                  choices = c("All checklists" = "all", setNames(names(epi_pages), epi_pages))),
                     DTOutput("home_region_month")),
        fluidRow(
          column(6, content_card("Checklists by region and type", NULL, DTOutput("home_region_type"))),
          column(6, content_card("Regions by month", "Each region's monthly count (all checklists).",
                                 plotly::plotlyOutput("home_region_lines", height = "360px"))))
      )),

    tabPanel("Health center", value = "health", icon = icon("hospital"),
      dashboard_page("h", "Supportive Supervision (Health center)",
                     "Health-center checklist: micro-plan, outreach, cold chain, training and supervision.",
                     "Districts supervised", "Light blue = at least one health-center visit in the selection.",
                     epi_health_indicators)),

    tabPanel("Session", value = "session", icon = icon("syringe"),
      dashboard_page("s", "Supportive Supervision (Session)",
                     "Session checklist: referrals, vaccine handling, safe injection, recording and messages.",
                     "Districts assessed", "Light blue = at least one session visit in the selection.",
                     epi_session_indicators)),

    tabPanel("Community", value = "community", icon = icon("people-roof"),
      tags$div(class = "app-shell",
        page_banner("Coverage Assessments (Community)",
                    "Children 12-23 months checked in the community: share vaccinated by marker.",
                    ppt_button("ppt_c")),
        uiOutput("c_metrics"),
        fluidRow(
          column(5, content_card("Districts assessed", "Light blue = at least one community assessment in the selection.",
                                 plotOutput("c_map", height = "430px"), uiOutput("c_map_note"))),
          column(7, content_card("Overall coverage by marker",
                                 "Years side by side when more than one year is selected. The markers chosen here apply to every chart on this page and to the PowerPoint.",
                                 selectizeInput("c_markers", NULL, choices = epi_markers$label, selected = epi_default_markers,
                                                multiple = TRUE, width = "100%",
                                                options = list(maxItems = 8, plugins = list("remove_button"))),
                                 plotly::plotlyOutput("c_overall", height = "360px")))
        ),
        content_card(textOutput("c_geo_title", inline = TRUE), "Coverage = vaccinated / children 12-23 months checked.",
                     plotly::plotlyOutput("c_geo", height = "400px")),
        fluidRow(
          column(6, content_card("Why children have not been vaccinated",
                                 "Share of community visits that recorded each reason (several can be ticked).",
                                 plotly::plotlyOutput("c_reasons", height = "360px"))),
          column(6, content_card("Coverage table", NULL, DTOutput("c_table"))))
      )),

    tabPanel("Data", value = "data", icon = icon("table"),
      tags$div(class = "app-shell",
        page_banner("Data", "The records behind the dashboards, with the filters above applied.",
                    downloadButton("data_xlsx", "Download filtered data (Excel)", class = "btn-light btn-sm")),
        content_card("Kobo data source", NULL,
          uiOutput("data_admin_controls"),
          tags$p(class = "epi-note", "Downloads the latest EPI_BACKUP export (about a minute). All pages update",
                 " when it finishes. Only an administrator can refresh the data.")),
        content_card("Records", NULL,
          radioButtons("data_page", NULL, choices = setNames(names(epi_pages), epi_pages), inline = TRUE),
          DTOutput("data_tbl")))),

    tabPanel("PowerPoint", value = "ppt", icon = icon("file-powerpoint"),
      tags$div(class = "app-shell",
        page_banner("Export to PowerPoint", "Dashboard deck (A) and the EPI supervision deck (B). Copies are saved in outputs/presentations/."),
        uiOutput("ppt_scope"),
        fluidRow(
          column(6, content_card("A. Meer Ali deck", "The three dashboard pages, slide by slide (PEI template).",
            checkboxGroupInput("ppt_pages", "Dashboards to include", choices = setNames(names(epi_pages), epi_pages),
                               selected = names(epi_pages)),
            downloadButton("ppt_all", "Build dashboard deck", class = "btn-primary", icon = icon("file-powerpoint")),
            tags$hr(),
            tags$ul(
            tags$li("Title slide with the filters and the data date."),
            tags$li("Per dashboard: a divider, the coverage map (300 dpi picture) with visits by region / province."),
            tags$li("Health center and Session: two indicators per slide - native PowerPoint 100% stacked charts by",
                    " region / province (right-click > Edit Data) plus the overall share - and a summary table."),
            tags$li("Community: overall coverage by year, coverage by region / province, coverage table, reasons",
                    " children were not vaccinated."),
            tags$li("No speaker notes.")))),
          column(6, content_card("B. Naveed Saeed EPI deck",
                                 "One deck: findings, provincial map grids, trends, actions; short technical annex last.",
            selectInput("ppt_ref_month", "Report month", choices = NULL, width = "220px"),
            tags$p(class = "epi-note", "Uses the Region / Province filters only. Period = the report month and January",
                   " to the report month of that year; the Year and Month filters do not apply."),
            downloadButton("ppt_review", "Build EPI deck", class = "btn-primary", icon = icon("file-powerpoint")),
            tags$hr(),
            tags$p(class = "epi-note", "One deck built from the slide list below; slide numbers and 'see slide /",
                   " annex' references follow it. Takes about a minute (many maps)."),
            DTOutput("ppt_manifest")))
        ))),

    tabPanel("Admin", value = "admin", icon = icon("user-shield"),
      tags$div(class = "app-shell",
        page_banner("Administration", "Manage administrative access and protected data operations."),
        uiOutput("admin_page"))),

    tabPanel("Help", value = "help", icon = icon("circle-question"),
      tags$div(class = "app-shell",
        page_banner("How to use", "Where the data comes from and what each page shows."),
        content_card("Data", NULL, tags$ul(
          tags$li(tags$b("Refresh from Kobo"), " downloads the EPI_BACKUP export of the Routine EPI checklist form",
                  " straight from KoboToolbox (about a minute). This control requires the administrator password;",
                  " public visitors have read-only access."),
          tags$li("The last download is kept in app_data/kobo_cache.rds, so the app opens instantly next time;",
                  " the last 5 raw exports are kept in archive/kobo_downloads/."),
          tags$li(tags$b("Historic data:"), " the files in data/Historic are added to the Kobo data; rows already",
                  " in Kobo (same _id) are skipped. Their Source column says 'Historic (file)', and a note under the",
                  " filters shows when a selection includes them. Coverage: East region Jan 2020 - Dec 2023 (Excel",
                  " dashboard 2022-2023); all regions Oct 2023 (sessions) and Jan 2024 (health centers, community)",
                  " from the 2023 Power BI file; Kobo from Jul 2024. The old checklists did not have every question:",
                  " for 2020-2023 the Session page has only tally sheet, immunization cards and the two messages;",
                  " Oct 2023 sessions have all but the two messages. A new file dropped in data/Historic is picked",
                  " up when the app restarts."),
          tags$li("Visits before ", format(epi_start_date(), "%d %b %Y"), " are not shown (same as the Power BI",
                  " StartDate parameter; change EPI_START_DATE in .Renviron)."))),
        content_card("Definitions", NULL, tags$ul(
          tags$li("Yes/No indicators: % = Yes / (Yes + No) among checklists of that type; blank answers are not counted."),
          tags$li("Map: a district is supervised / assessed when it has at least one visit in the selection",
                  " (joined on District Code)."),
          tags$li("Community coverage: sum of vaccinated children / sum of children 12-23 months checked."),
          tags$li("Bars are by region, or by province when exactly one region is selected (change with 'Bars by')."))),
        page_banner("Versions & Changelog", paste0("Current version ", app_version, ".")),
        content_card("Changelog", NULL, uiOutput("help_changelog"))))
  )
)

# ---- Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  paths <- epi_paths(epi_app_root)
  rv <- reactiveValues(data = epi_load_data(paths))
  tables <- reactive(rv$data[c("health", "session", "community")])
  is_admin <- reactiveVal(FALSE)

  output$data_admin_controls <- renderUI({
    if (is_admin()) {
      tags$div(class = "d-flex flex-wrap align-items-center gap-3",
        actionButton("refresh", "Refresh from Kobo", icon = icon("cloud-arrow-down"), class = "btn-primary"),
        uiOutput("refresh_info"),
        tags$span(class = "text-success", icon("shield-halved"), "Administrator"),
        actionButton("admin_logout", "Sign out", class = "btn-outline-secondary btn-sm"))
    } else {
      tags$div(class = "d-flex flex-wrap align-items-center gap-3",
        tags$span(class = "epi-note", "Public access is read-only."),
        actionButton("admin_login", "Administrator sign in", icon = icon("lock"),
                     class = "btn-outline-secondary btn-sm"))
    }
  })

  output$admin_page <- renderUI({
    configured <- nzchar(Sys.getenv("ADMIN_PASSWORD", unset = ""))
    hosted <- nzchar(Sys.getenv("CONNECT_CONTENT_GUID", unset = "")) ||
              nzchar(Sys.getenv("RSCONNECT_CONTENT_GUID", unset = ""))
    role <- if (is_admin()) "Administrator" else "Public user"
    role_color <- if (is_admin()) "#2E8540" else "#64748B"
    tagList(
      metric_grid(
        metric_static("Current role", role, if (is_admin()) "Protected controls enabled" else "Read-only access", role_color),
        metric_static("Admin password", if (configured) "Configured" else "Not configured",
                      "Connect Cloud secret: ADMIN_PASSWORD", if (configured) "#2E8540" else "#D71920")),
      content_card("Access management", "Lightweight access model for this public application.",
        tags$div(class = "d-flex flex-wrap align-items-center gap-2",
          if (is_admin()) {
            tagList(actionButton("admin_open_data", "Open data management", icon = icon("database"), class = "btn-primary"),
                    actionButton("admin_logout_page", "Sign out", icon = icon("right-from-bracket"),
                                 class = "btn-outline-secondary"))
          } else {
            actionButton("admin_login_page", "Administrator sign in", icon = icon("lock"), class = "btn-primary")
          }),
        tags$hr(),
        tags$ul(
          tags$li(tags$b("Public user:"), " view dashboards and download filtered data and presentations."),
          tags$li(tags$b("Administrator:"), " all public permissions plus refresh/import data from Kobo."),
          tags$li("Named accounts, invitations, and per-user regional access require authenticated private hosting and are not enabled."))),
      if (is_admin()) content_card("Change administrator password", NULL,
        if (hosted) {
          tags$p(class = "epi-note", "For Connect Cloud, change ADMIN_PASSWORD under Application Settings > Variables,",
                 " then restart the application. Runtime password-file changes are not persistent in the cloud.")
        } else {
          tagList(
            passwordInput("admin_current_password", "Current password", width = "360px"),
            passwordInput("admin_new_password", "New password", width = "360px"),
            passwordInput("admin_confirm_password", "Confirm new password", width = "360px"),
            tags$p(class = "epi-note", "Use at least 12 characters. Allowed: letters, numbers, and ! @ % _ + . , : = -"),
            actionButton("admin_change_password", "Change password", icon = icon("key"), class = "btn-primary"))
        })
    )
  })

  show_admin_login <- function() {
    showModal(modalDialog(
      title = "Administrator sign in",
      passwordInput("admin_password", "Password", width = "100%"),
      tags$p(class = "epi-note", "The password is configured as the ADMIN_PASSWORD secret in Connect Cloud."),
      footer = tagList(modalButton("Cancel"), actionButton("admin_submit", "Sign in", class = "btn-primary")),
      easyClose = TRUE
    ))
  }

  observeEvent(input$admin_login, {
    show_admin_login()
  })

  observeEvent(input$admin_login_page, show_admin_login())

  observeEvent(input$admin_submit, {
    configured <- Sys.getenv("ADMIN_PASSWORD", unset = "")
    if (nzchar(configured) && identical(input$admin_password, configured)) {
      is_admin(TRUE)
      removeModal()
      showNotification("Administrator access enabled for this session.", type = "message")
    } else {
      showNotification("Incorrect password, or ADMIN_PASSWORD has not been configured.", type = "error")
    }
  })

  observeEvent(input$admin_logout, {
    is_admin(FALSE)
    showNotification("Administrator access ended.", type = "message")
  })

  observeEvent(input$admin_logout_page, {
    is_admin(FALSE)
    showNotification("Administrator access ended.", type = "message")
  })

  observeEvent(input$admin_open_data, updateNavbarPage(session, "main_nav", selected = "data"))

  observeEvent(input$admin_change_password, {
    current <- Sys.getenv("ADMIN_PASSWORD", unset = "")
    new_password <- input$admin_new_password %||% ""
    if (!identical(input$admin_current_password, current)) {
      showNotification("The current password is incorrect.", type = "error")
      return()
    }
    if (nchar(new_password) < 12L) {
      showNotification("The new password must contain at least 12 characters.", type = "error")
      return()
    }
    if (!grepl("^[A-Za-z0-9!@%_+.,:=-]+$", new_password)) {
      showNotification("The new password contains unsupported characters.", type = "error")
      return()
    }
    if (!identical(new_password, input$admin_confirm_password)) {
      showNotification("The new password and confirmation do not match.", type = "error")
      return()
    }

    env_file <- file.path(epi_app_root, ".Renviron")
    lines <- if (file.exists(env_file)) readLines(env_file, warn = FALSE) else character()
    setting <- paste0("ADMIN_PASSWORD=", new_password)
    hit <- grepl("^ADMIN_PASSWORD=", lines)
    if (any(hit)) lines[which(hit)[1]] <- setting else lines <- c(lines, setting)
    tmp <- paste0(env_file, ".tmp")
    result <- tryCatch({
      writeLines(lines, tmp, useBytes = TRUE)
      if (!file.copy(tmp, env_file, overwrite = TRUE)) stop("could not replace .Renviron")
      unlink(tmp)
      Sys.setenv(ADMIN_PASSWORD = new_password)
      TRUE
    }, error = function(e) e)
    if (inherits(result, "error")) {
      showNotification(paste("Password change failed:", conditionMessage(result)), type = "error", duration = NULL)
    } else {
      updateTextInput(session, "admin_current_password", value = "")
      updateTextInput(session, "admin_new_password", value = "")
      updateTextInput(session, "admin_confirm_password", value = "")
      showNotification("Administrator password changed successfully.", type = "message")
    }
  })

  # ---- filter choices ----
  # Year filter: the current year is selected when the app opens (and on Reset)
  current_year <- format(Sys.Date(), "%Y")
  year_init <- reactiveVal(FALSE)
  observe({
    t <- tables()
    regions <- sort(unique(unlist(lapply(t, function(x) x$Region))))
    years <- sort(unique(unlist(lapply(t, function(x) x$Year[!is.na(x$VisitDate) & x$VisitDate >= epi_start_date()]))),
                  decreasing = TRUE)
    updateSelectizeInput(session, "f_region", choices = regions, selected = isolate(input$f_region))
    sel <- if (!isolate(year_init())) intersect(current_year, years) else isolate(input$f_year)
    updateSelectizeInput(session, "f_year", choices = years, selected = sel)
    year_init(TRUE)
  })
  observe({
    t <- tables()
    reg <- input$f_region
    pr <- unique(do.call(rbind, lapply(t, function(x) x[, c("Region", "Province")])))
    if (length(reg)) pr <- pr[pr$Region %in% reg, ]
    ch <- sort(unique(pr$Province[!is.na(pr$Province)]))
    updateSelectizeInput(session, "f_province", choices = ch, selected = intersect(isolate(input$f_province), ch))
  })
  observeEvent(input$f_reset, {
    for (id in c("f_region", "f_province", "f_month")) updateSelectizeInput(session, id, selected = character())
    updateSelectizeInput(session, "f_year", selected = current_year)
    updateSelectInput(session, "f_geo", selected = "Auto")
  })

  filters <- debounce(reactive(list(regions = input$f_region, provinces = input$f_province,
                                    years = input$f_year, months = input$f_month)), 400)
  fdata <- reactive(lapply(tables(), epi_filter, f = filters()))
  geo_level <- reactive(epi_geo_level(input$f_geo, filters()$regions))
  markers <- reactive({ m <- input$c_markers; if (length(m)) m else epi_default_markers })

  # ---- refresh ----
  output$data_status <- renderUI({
    m <- rv$data$meta
    live <- grepl("^Kobo", m$source %||% "")
    when <- if (!is.null(m$fetched_at) && !is.na(m$fetched_at)) format(m$fetched_at, "%d %b %Y %H:%M") else "never"
    tags$span(
      tags$span(class = if (live) "status-live" else "status-snapshot",
                icon(if (live) "circle-check" else "triangle-exclamation"),
                if (live) "Kobo data" else "Snapshot"),
      sprintf(" · %s · %s records", when, format(m$rows %||% 0, big.mark = ",")),
      title = m$source)
  })

  output$refresh_info <- renderUI({
    m <- rv$data$meta
    h <- rv$data$historic
    tagList(
      tags$span(class = "epi-note", sprintf("Current data: %s. Last download: %s (%s records).", m$source %||% "-",
                                            if (!is.null(m$fetched_at) && !is.na(m$fetched_at)) format(m$fetched_at, "%d %b %Y %H:%M") else "never",
                                            format(m$rows %||% 0, big.mark = ","))),
      if (!is.null(h) && nrow(h)) {
        n <- function(v) format(v %||% rep(0L, nrow(h)), big.mark = ",", trim = TRUE)
        tags$div(class = "epi-note",
          "Historic files (data/Historic) added to the Kobo data. A row is skipped when the same visit (same _id, or",
          "same checklist, date, district, facility and answers) is already in Kobo or in a file listed before it: ",
          paste(sprintf("%s: %s of %s rows added (%s already in Kobo, %s in an earlier file)",
                        sub("^Historic [(](.*)[)]$", "\\1", h$Source), n(h$Added), n(h$Read), n(h$InKobo),
                        n(h$InEarlier)), collapse = "; "), ".")
      })
  })

  # Flag under the filter bar when the selection includes historic (pre-Kobo) records
  output$hist_note <- renderUI({
    d <- fdata()
    n <- epi_historic_count(d)
    if (!n) return(NULL)
    tot <- sum(vapply(d, nrow, integer(1)))
    yrs <- sort(unique(unlist(lapply(d, function(x) x$Year[x$Source != "Kobo"]))))
    yrs <- yrs[!is.na(yrs)]
    tagList(icon("clock-rotate-left"), tags$b(" Historic data: "),
            sprintf(paste("%s of %s records in this selection (%s) come from files kept before Kobo (old checklist).",
                          "Questions not asked then are blank and left out of the %%. The files run from 2019 to",
                          "Sep 2024 (Kobo from Sep 2024); some months and regions have few or no checklists."),
                    fmt_n(n), fmt_n(tot), paste(unique(range(yrs)), collapse = "-")))
  })

  observeEvent(input$refresh, {
    req(is_admin())
    # the button disables itself on click (JS in the page head); re-enable when done
    on.exit(session$sendCustomMessage("epi-refresh-done", TRUE), add = TRUE)
    note <- showNotification("Downloading the latest EPI_BACKUP export from Kobo (about a minute)...",
                             duration = NULL, type = "message")
    on.exit(removeNotification(note), add = TRUE)   # add = TRUE: keep the epi-refresh-done handler above
    res <- withProgress(message = "Refreshing from Kobo", detail = "Kobo is preparing the export", value = 0.3, {
      tryCatch(epi_refresh_from_kobo(paths), error = function(e) e)
    })
    if (inherits(res, "error")) {
      showNotification(paste("Refresh failed:", conditionMessage(res)), type = "error", duration = NULL)
    } else {
      rv$data <- res
      showNotification(sprintf("Updated from Kobo: %s records (Health center %s, Session %s, Community %s) in %ss.",
                               format(res$meta$rows, big.mark = ","), format(nrow(res$health), big.mark = ","),
                               format(nrow(res$session), big.mark = ","), format(nrow(res$community), big.mark = ","),
                               res$meta$seconds), type = "message", duration = 8)
    }
  })

  # ---- shared renderers ----
  fmt_n <- function(x) format(x, big.mark = ",")
  visits_dt <- function(d) {
    tab <- epi_visits_by(d, geo_level())
    names(tab) <- c(geo_level(), "Visits")
    if (nrow(tab)) { tot <- tab[1, ]; tot[[1]] <- "Total"; tot$Visits <- sum(tab$Visits); tab <- rbind(tab, tot) }
    datatable(tab, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = "430px",
                                                    scrollCollapse = TRUE)) |>
      formatRound("Visits", digits = 0, mark = ",") |>
      formatStyle(geo_level(), target = "row", fontWeight = styleEqual("Total", "bold"))
  }
  map_note <- function(d) {
    u <- epi_unmatched_codes(d$DCode)
    if (!length(u)) return(NULL)
    tags$p(class = "epi-note", sprintf("%s visit(s) have a district code not in the boundary file: %s",
                                       sum(d$DCode %in% u), paste(u, collapse = ", ")))
  }
  yn_metrics <- function(d, what) {
    metric_grid(
      metric_static("Visits", fmt_n(nrow(d)), epi_filter_label(filters())),
      metric_static(paste("Districts", what), length(unique(d$DCode[!is.na(d$DCode)])),
                    sprintf("of %s districts", nrow(epi_geo()$districts))),
      metric_static("Provinces", length(unique(d$Province[!is.na(d$Province)]))),
      metric_static("Health facilities", fmt_n(length(unique(d$Facility[!is.na(d$Facility)])))),
      metric_static("Latest visit", if (nrow(d)) format(max(d$VisitDate), "%d %b %Y") else "-")
    )
  }
  render_yn_page <- function(prefix, page, defs, map_labels, what) {
    d <- reactive(fdata()[[page]])
    output[[paste0(prefix, "_metrics")]] <- renderUI(yn_metrics(d(), what))
    output[[paste0(prefix, "_map")]] <- renderPlot(
      epi_map(epi_visits_by_district(d()), map_labels, filters()$regions, base_size = 12), bg = "transparent", res = 96)
    output[[paste0(prefix, "_map_note")]] <- renderUI(map_note(d()))
    output[[paste0(prefix, "_visits")]] <- renderDT(visits_dt(d()))
    for (i in seq_len(nrow(defs))) local({
      key <- defs$key[i]
      output[[paste0(prefix, "_", key, "_bar")]] <- plotly::renderPlotly(
        epi_plotly_yesno(epi_yn_by(d(), key, geo_level()), geo_level()))
      output[[paste0(prefix, "_", key, "_donut")]] <- plotly::renderPlotly(epi_plotly_donut(epi_yn_overall(d(), key)))
    })
  }
  render_yn_page("h", "health", epi_health_indicators, c("Supervised", "Not supervised"), "supervised")
  render_yn_page("s", "session", epi_session_indicators, c("Assessed", "Not assessed"), "assessed")

  # ---- last month + compliance ----
  # Region / province filters only (the period comes from the report month)
  base_data <- reactive(lapply(tables(), epi_filter, f = list(regions = filters()$regions,
                                                               provinces = filters()$provinces)))
  ref_month <- reactive(epi_ref_month(base_data()))
  comp_months <- reactive({
    now <- format(Sys.Date(), "%Y-%m")
    yrs <- filters()$years
    m <- if (length(yrs)) unlist(lapply(sort(yrs), function(y) sprintf("%s-%02d", y, 1:12)))
         else sort(unique(epi_responses(fdata())$month))
    m <- m[m <= now]
    if (length(filters()$months)) m <- m[month.name[as.integer(substr(m, 6, 7))] %in% filters()$months]
    m
  })

  output$home_last_title <- renderText(paste("Last month:", format(as.Date(paste0(ref_month(), "-01")), "%B %Y")))
  output$home_last_metrics <- renderUI({
    ref <- ref_month(); prev <- .epi_add_months(ref, -1)
    cur <- epi_subset_months(base_data(), ref); bef <- epi_subset_months(base_data(), prev)
    n <- vapply(cur, nrow, integer(1)); np <- vapply(bef, nrow, integer(1))
    delta <- function(a, b) if (b > 0) sprintf("%s%s vs %s (%s)", if (a >= b) "+" else "", .epi_pct((a - b) / b),
                                               epi_month_label(prev), fmt_n(b)) else paste("none in", epi_month_label(prev))
    scope <- epi_province_scope(filters()$regions, filters()$provinces)
    k <- length(intersect(scope$Province, epi_responses(cur)$Province))
    metric_grid(
      metric_static("Checklists", fmt_n(sum(n)), delta(sum(n), sum(np))),
      metric_static("Health center", fmt_n(n[["health"]]), delta(n[["health"]], np[["health"]]), epi_colors$series[1]),
      metric_static("Session", fmt_n(n[["session"]]), delta(n[["session"]], np[["session"]]), epi_colors$series[2]),
      metric_static("Community", fmt_n(n[["community"]]), delta(n[["community"]], np[["community"]]), epi_colors$series[3]),
      metric_static("Provinces reporting", sprintf("%s / %s", k, nrow(scope)), "at least one checklist",
                    if (k < nrow(scope)) "#D71920" else "#2E8540"))
  })
  output$home_last_compliance <- renderDT({
    t <- epi_compliance_by_region(base_data(), ref_month(), filters()$regions, filters()$provinces)
    if (is.null(t) || !nrow(t)) return(datatable(data.frame(Message = "No provinces in scope"), rownames = FALSE,
                                                 options = list(dom = "t")))
    types <- unname(epi_pages)
    show <- data.frame(Region = t$Region, `Provinces (denominator)` = t$Provinces, check.names = FALSE)
    for (p in types) show[[p]] <- epi_compliance_label(t[[paste(p, "n")]], t$Provinces)
    show$Checklists <- t$Checklists
    show$`No checklist at all` <- t$`No checklist at all`
    for (p in types) show[[paste0(".", p)]] <- t[[p]]          # hidden shares that drive the colours
    hidden <- which(startsWith(names(show), ".")) - 1
    dt <- datatable(show, rownames = FALSE,
                    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE,
                                   columnDefs = list(list(visible = FALSE, targets = hidden)))) |>
      formatRound("Checklists", digits = 0, mark = ",") |>
      formatStyle("Region", target = "row", fontWeight = styleEqual("Total", "bold"))
    for (p in types) dt <- formatStyle(dt, p, valueColumns = paste0(".", p),
                                       backgroundColor = styleInterval(c(0.75 - 1e-9, 0.9 - 1e-9),
                                                                       c("#F8C9C4", "#FCE8B2", "#CDEBD3")))
    dt
  })
  output$home_period_title <- renderText({
    m <- comp_months()
    if (!length(m)) return("Selected period")
    sprintf("Selected period: %s", if (length(m) == 1) epi_month_label(m) else paste(epi_month_label(min(m)), "-", epi_month_label(max(m))))
  })
  output$home_comp_trend <- plotly::renderPlotly({
    m <- comp_months()
    if (!length(m)) return(epi_plotly_empty("No months in the selection"))
    cm <- epi_compliance_by_month(fdata(), m, filters()$regions, filters()$provinces)
    p <- plotly::plot_ly()
    for (i in seq_along(epi_pages)) {
      s <- cm[cm$checklist == epi_pages[[i]], ]
      p <- plotly::add_trace(p, type = "scatter", mode = "lines+markers", x = epi_month_label(s$month), y = s$pct,
                             name = epi_pages[[i]], line = list(width = 2.5, color = epi_colors$series[i]),
                             marker = list(size = 7, color = epi_colors$series[i]), customdata = paste(s$reporting, "of", s$expected),
                             hovertemplate = paste0("<b>", epi_pages[[i]], "</b> %{x}<br>%{y:.0%} of provinces (%{customdata})<extra></extra>"))
    }
    p |> plotly::layout(xaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = epi_month_label(m)),
                        yaxis = list(title = "", tickformat = ".0%", range = c(0, 1.05), gridcolor = "#eef2f6", zeroline = FALSE),
                        legend = list(orientation = "h", y = -0.15), hovermode = "x unified",
                        margin = list(l = 45, r = 10, t = 10, b = 10)) |> .epi_plotly_base()
  })
  output$home_comp_grid <- renderDT({
    m <- comp_months()
    pages <- if (identical(input$comp_pages, "all") || is.null(input$comp_pages)) names(epi_pages) else input$comp_pages
    if (!length(m)) return(datatable(data.frame(Message = "No months in the selection"), rownames = FALSE, options = list(dom = "t")))
    g <- epi_compliance_grid(fdata(), m, pages, filters()$regions, filters()$provinces)
    mc <- epi_month_label(m)
    datatable(g, rownames = FALSE, extensions = "FixedColumns",
              options = list(dom = "t", paging = FALSE, ordering = TRUE, scrollX = TRUE, scrollY = "360px",
                             scrollCollapse = TRUE, fixedColumns = list(leftColumns = 2))) |>
      formatStyle(mc, backgroundColor = styleInterval(c(0, 4), c("#F8C9C4", "#FCE8B2", "#FFFFFF"))) |>
      formatStyle("Months reported", fontWeight = "bold",
                  backgroundColor = styleInterval(c(length(mc) - 2, length(mc) - 1), c("#F8C9C4", "#FCE8B2", "#CDEBD3"))) |>
      formatRound("Total", digits = 0, mark = ",")
  })

  # ---- home (combined summary) ----
  output$home_metrics <- renderUI({
    r <- epi_responses(fdata())
    n <- vapply(fdata(), nrow, integer(1))
    last <- suppressWarnings(max(unlist(lapply(fdata(), function(d) as.numeric(d$VisitDate))), na.rm = TRUE))
    metric_grid(
      metric_static("Checklists", fmt_n(nrow(r)), epi_filter_label(filters())),
      metric_static("Health center", fmt_n(n[["health"]]), "visits", epi_colors$series[1]),
      metric_static("Session", fmt_n(n[["session"]]), "visits", epi_colors$series[2]),
      metric_static("Community", fmt_n(n[["community"]]), "assessments", epi_colors$series[3]),
      metric_static("Regions reporting", length(unique(r$Region[!is.na(r$Region)]))),
      metric_static("Latest visit", if (is.finite(last)) format(as.Date(last, origin = "1970-01-01"), "%d %b %Y") else "-"))
  })
  output$home_month <- plotly::renderPlotly({
    m <- epi_responses_by_month(fdata())
    if (!nrow(m) || !sum(m$n)) return(epi_plotly_empty("No checklists for this selection"))
    m$label <- epi_month_label(m$month)
    lv <- epi_month_label(sort(unique(m$month)))
    p <- plotly::plot_ly()
    for (i in seq_along(epi_pages)) {
      s <- m[m$checklist == epi_pages[[i]], ]
      p <- plotly::add_bars(p, x = factor(s$label, levels = lv), y = s$n, name = epi_pages[[i]],
                            marker = list(color = epi_colors$series[i], line = list(color = "#ffffff", width = 1)),
                            hovertemplate = paste0("<b>%{x}</b><br>", epi_pages[[i]], ": %{y:,}<extra></extra>"))
    }
    p |> plotly::layout(barmode = "stack", bargap = 0.3, xaxis = list(title = "", type = "category"),
                        yaxis = list(title = "", gridcolor = "#eef2f6", zeroline = FALSE),
                        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12),
                        margin = list(l = 45, r = 10, t = 10, b = 10)) |> .epi_plotly_base()
  })
  output$home_region_month <- renderDT({
    pages <- if (identical(input$home_pages, "all") || is.null(input$home_pages)) names(epi_pages) else input$home_pages
    t <- epi_region_month_table(fdata(), pages)
    if (!nrow(t)) return(datatable(data.frame(Message = "No checklists for this selection"), rownames = FALSE,
                                   options = list(dom = "t")))
    mcols <- setdiff(names(t), c("Region", "Total"))
    vals <- unlist(t[t$Region != "Total", mcols]); mx <- max(vals, 1)
    cuts <- unique(round(mx * c(0.1, 0.25, 0.4, 0.55, 0.7, 0.85)))
    ramp <- c("#ffffff", "#e3eefb", "#cde2fb", "#b7d3f6", "#9ec5f4", "#6da7ec", "#3987e5")[seq_len(length(cuts) + 1)]
    ink <- ifelse(seq_along(ramp) >= length(ramp) - 1 & length(ramp) > 4, "#ffffff", "#17324d")
    datatable(t, rownames = FALSE, extensions = "FixedColumns",
              options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE,
                             fixedColumns = list(leftColumns = 1))) |>
      formatRound(setdiff(names(t), "Region"), digits = 0, mark = ",") |>
      formatStyle(mcols, backgroundColor = styleInterval(cuts, ramp), color = styleInterval(cuts, ink)) |>
      formatStyle("Region", target = "row", fontWeight = styleEqual("Total", "bold")) |>
      formatStyle("Total", fontWeight = "bold")
  })
  output$home_region_type <- renderDT({
    r <- epi_responses(fdata())
    r <- r[!is.na(r$Region), ]
    if (!nrow(r)) return(datatable(data.frame(Message = "No checklists for this selection"), rownames = FALSE,
                                   options = list(dom = "t")))
    t <- as.data.frame.matrix(table(r$Region, factor(r$checklist, levels = unname(epi_pages))))
    t$Total <- rowSums(t)
    t <- cbind(Region = rownames(t), t); rownames(t) <- NULL
    tot <- t[1, ]; tot$Region <- "Total"; for (j in names(t)[-1]) tot[[j]] <- sum(t[[j]])
    t <- rbind(t, tot)
    datatable(t, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE)) |>
      formatRound(setdiff(names(t), "Region"), digits = 0, mark = ",") |>
      formatStyle("Region", target = "row", fontWeight = styleEqual("Total", "bold"))
  })
  output$home_region_lines <- plotly::renderPlotly({
    r <- epi_responses(fdata())
    r <- r[!is.na(r$Region) & !is.na(r$month), ]
    if (!nrow(r)) return(epi_plotly_empty("No checklists for this selection"))
    months <- sort(unique(r$month)); regs <- sort(unique(r$Region))
    t <- table(factor(r$Region, levels = regs), factor(r$month, levels = months))
    p <- plotly::plot_ly()
    for (i in seq_along(regs)) {
      p <- plotly::add_trace(p, type = "scatter", mode = "lines+markers", x = epi_month_label(months), y = as.numeric(t[i, ]),
                             name = regs[i], line = list(width = 2, color = epi_colors$series[(i - 1) %% 8 + 1]),
                             marker = list(size = 6, color = epi_colors$series[(i - 1) %% 8 + 1]),
                             hovertemplate = paste0("<b>", regs[i], "</b><br>%{x}: %{y:,}<extra></extra>"))
    }
    p |> plotly::layout(xaxis = list(title = "", type = "category", categoryorder = "array",
                                     categoryarray = epi_month_label(months)),
                        yaxis = list(title = "", gridcolor = "#eef2f6", zeroline = FALSE, rangemode = "tozero"),
                        legend = list(orientation = "h", y = -0.15), hovermode = "x unified",
                        margin = list(l = 45, r = 10, t = 10, b = 10)) |> .epi_plotly_base()
  })

  # ---- community ----
  cdat <- reactive(fdata()$community)
  output$c_metrics <- renderUI({
    d <- cdat()
    cov <- epi_coverage(d, c("Vaccination card", "Penta-3", "Measles-1"))
    pc <- function(m) { v <- cov$pct[cov$marker == m]; if (length(v)) .epi_pct(v) else "-" }
    metric_grid(
      metric_static("Community visits", fmt_n(nrow(d)), epi_filter_label(filters())),
      metric_static("Children 12-23 m checked", fmt_n(sum(d$children, na.rm = TRUE))),
      metric_static("Vaccination card", pc("Vaccination card"), "card available", "#2a78d6"),
      metric_static("Penta-3", pc("Penta-3"), "coverage", "#2a78d6"),
      metric_static("Measles-1", pc("Measles-1"), "coverage", "#2a78d6"))
  })
  output$c_map <- renderPlot(epi_map(epi_visits_by_district(cdat()), c("Assessed", "Not assessed"), filters()$regions,
                                     base_size = 12), bg = "transparent", res = 96)
  output$c_map_note <- renderUI(map_note(cdat()))
  output$c_overall <- plotly::renderPlotly(
    epi_plotly_coverage_markers(epi_coverage(cdat(), markers(), by = "Year"), markers()))
  output$c_geo_title <- renderText(paste("Coverage by", tolower(geo_level()), "and marker"))
  output$c_geo <- plotly::renderPlotly(
    epi_plotly_coverage(epi_coverage(cdat(), markers(), by = geo_level()), epi_marker_colors(markers())))
  output$c_reasons <- plotly::renderPlotly(epi_plotly_reasons(epi_reason_summary(cdat())))
  output$c_table <- renderDT({
    cov <- epi_coverage(cdat(), markers(), by = geo_level())
    if (!nrow(cov)) return(datatable(data.frame(Message = "No community assessments for this selection"), rownames = FALSE,
                                     options = list(dom = "t")))
    wide <- as.data.frame(tidyr::pivot_wider(cov[, c("group", "children", "marker", "pct")],
                                             names_from = "marker", values_from = "pct"), check.names = FALSE)
    names(wide)[1:2] <- c(geo_level(), "Children")
    datatable(wide, rownames = FALSE, options = list(dom = "t", paging = FALSE, scrollX = TRUE, scrollY = "320px")) |>
      formatPercentage(as.character(markers()), digits = 0) |>
      formatRound("Children", digits = 0, mark = ",")
  })

  # ---- data page ----
  output$data_tbl <- renderDT({
    page <- input$data_page
    datatable(epi_export_table(fdata()[[page]], page, short = TRUE), rownames = FALSE, filter = "top",
              class = "compact stripe nowrap",
              options = list(pageLength = 15, scrollX = TRUE, autoWidth = FALSE, lengthMenu = c(10, 15, 25, 50, 100)))
  })
  output$data_xlsx <- downloadHandler(
    filename = function() sprintf("Routine_EPI_data_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      sheets <- lapply(names(epi_pages), function(p) epi_export_table(fdata()[[p]], p))
      names(sheets) <- unname(epi_pages)
      sheets$`Read me` <- data.frame(Item = c("Source", "Downloaded", "Filters", "Start date", "Historic records"),
                                     Value = c(rv$data$meta$source, format(rv$data$meta$fetched_at),
                                               epi_filter_label(filters()), format(epi_start_date()),
                                               paste(fmt_n(epi_historic_count(fdata())), "(column Source; files in",
                                                     "data/Historic kept before Kobo, not every question was asked)")))
      writexl::write_xlsx(sheets, file)
    })

  # ---- PowerPoint ----
  output$ppt_scope <- renderUI({
    n <- vapply(fdata(), nrow, integer(1))
    tags$p(class = "card-subtitle", sprintf("Filters: %s. Visits: Health center %s, Session %s, Community %s.",
                                            epi_filter_label(filters()), fmt_n(n[["health"]]), fmt_n(n[["session"]]),
                                            fmt_n(n[["community"]])))
  })
  ppt_handler <- function(pages_fun) downloadHandler(
    filename = function() sprintf("Routine_EPI_Dashboard_%s.pptx", format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      pages <- pages_fun()
      if (!length(pages)) pages <- names(epi_pages)
      withProgress(message = "Building PowerPoint", value = 0.05, {
        epi_build_deck(fdata(), file, pages = pages, filters = filters(), geo_choice = input$f_geo,
                       markers = markers(), as_of = rv$data$meta$fetched_at, paths = paths,
                       progress = function(m) incProgress(0.22, detail = m))
      })
      dir.create(paths$decks, recursive = TRUE, showWarnings = FALSE)
      file.copy(file, file.path(paths$decks, sprintf("Routine_EPI_Dashboard_%s.pptx", format(Sys.time(), "%Y%m%d_%H%M%S"))))
    })
  observe({
    have <- sort(unique(epi_responses(base_data())$month), decreasing = TRUE)
    have <- have[!is.na(have)]
    sel <- isolate(input$ppt_ref_month)
    if (!length(sel) || !sel %in% have) sel <- ref_month()
    updateSelectInput(session, "ppt_ref_month", choices = setNames(have, format(as.Date(paste0(have, "-01")), "%B %Y")),
                      selected = sel)
  })
  output$ppt_h <- ppt_handler(function() "health")
  output$ppt_s <- ppt_handler(function() "session")
  output$ppt_c <- ppt_handler(function() "community")
  output$ppt_all <- ppt_handler(function() input$ppt_pages)
  output$ppt_review <- downloadHandler(
    filename = function() sprintf("EPI_Supervision_Deck_%s.pptx", format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      withProgress(message = "Building the EPI deck", value = 0.05, {
        epi_build_review_deck(base_data(), file, filters = filters()[c("regions", "provinces")], geo_choice = input$f_geo,
                              as_of = rv$data$meta$fetched_at, paths = paths, ref_month = input$ppt_ref_month,
                              progress = function(m) incProgress(0.04, detail = m))
      })
      dir.create(paths$decks, recursive = TRUE, showWarnings = FALSE)
      file.copy(file, file.path(paths$decks, sprintf("EPI_Supervision_Deck_%s.pptx",
                                                     format(Sys.time(), "%Y%m%d_%H%M%S"))))
    })
  output$ppt_manifest <- renderDT({
    m <- epi_review_manifest()
    m <- data.frame(Slide = m$order, Part = ifelse(m$deck == "annex", paste(m$part, m$annex_label), m$part),
                    Title = gsub("\\{[a-z_]+\\}", "...", m$title), Visual = m$map_type, `From v6` = m$v6_source,
                    check.names = FALSE)
    datatable(m, rownames = FALSE, options = list(dom = "t", pageLength = 30, ordering = FALSE), class = "compact")
  })

  # ---- help ----
  output$help_changelog <- renderUI({
    f <- paths$changelog
    if (!file.exists(f)) return(tags$p("No changelog yet."))
    HTML(commonmark::markdown_html(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")))
  })
}

shinyApp(ui, server)
