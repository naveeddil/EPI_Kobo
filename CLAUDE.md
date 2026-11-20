# CLAUDE.md

Guidance for Claude Code (and people) working in this repository.

## What this project is

**Routine EPI dashboard** is an R/Shiny app for WHO Afghanistan. It replaces the Power BI report
`reference/powerbi/Routine_EPI_Dashboard.pbix`. It downloads the Routine EPI checklist form data
straight from KoboToolbox (export setting EPI_BACKUP), shows the Power BI pages (Health center,
Session, Community) plus a combined Home summary, and builds two PowerPoint decks.

Standalone, but laid out like `../AFP_ZeroDose` and `../PAKAFG_Surveillance` (UI look, folder
layout, versioning, map protocol). Never read files from those folders at runtime; copy what is
needed (geography, template, CSS and logo were copied).

## Run / check (from the project folder)

Rscript is not on PATH: `C:/Users/saeedn/AppData/Local/Programs/R/R-4.6.1/bin/Rscript.exe`.

| Task | Command |
|---|---|
| App | `source("RUN_WEB_APP.R")` (port 3842). `EPI_START_TAB=<tab value>` opens a given page |
| Refresh from Kobo without app | `Rscript Scripts/01_refresh_from_kobo.R` |
| Build decks without app | `Rscript Scripts/02_build_presentation.R [dashboard\|review\|both] [REGION,..] [YEAR,..]` |
| Regression check | `Rscript Scripts/90_EPI_CHECK.R` (maths, data, map, both decks, shiny::testServer) |
| Deck B v7 build + QA | `Rscript Scripts/03_review_deck_qa.R [YYYY-MM] [out dir]` (builds the deck, v6 reconciliation, mapping, validation .md) |

Headless browser screenshots: chromote cannot open its debugging port on this machine (browser
policy). `chrome --headless=new --screenshot` works for static HTML (e.g. a plotly widget saved with
`htmlwidgets::saveWidget`) but does not wait for Shiny outputs. Slide renders: PowerPoint COM
`Slides.Item(i).Export(...)` - build into the scratch/temp folder first; files just written to
OneDrive can be locked by sync ("The cloud operation was unsuccessful").

## Layout

```
app.R                 Shiny UI + server (navbarPage; shared filter bar in `header`)
RUN_WEB_APP.R         launcher
R/epi_load.R          epi_source_all(): checks packages, sources R/ files in order
R/epi_config.R        paths, Kobo URL, start date, colours
R/epi_kobo.R          download (503/Retry-After), read CSV/XLSX, prepare, cache
R/epi_historic.R      data/Historic readers (old Kobo XML export, 2020-23 Excel dashboard), merge
R/epi_indicators.R    indicator definitions (from the Power Query/DAX), prep, filters, summaries
R/epi_geo.R           afg_geography.rds -> districts/provinces/regions; epi_map() (ggplot)
R/epi_charts.R        plotly (screen) + mschart (PowerPoint) chart builders
R/epi_deck.R          deck A: dashboard deck (reference/epi_template.pptx, PEI master)
R/epi_deck_review.R   deck B look + reused slides (trends, reasons, region x month); full v6 map annex
R/epi_deck_review_v7.R deck B v7: slide manifest, shared figure helpers, map-led modules, builder
reference/            geography/afg_geography.rds, epi_template.pptx, powerbi/ (old PBIX, Power
                      Query, Deneb specs, boundary JSONs, images - reference only)
data/                 EPI_Kobo_Data.xlsx (snapshot; used only until the first refresh),
                      Historic/ (old files merged into every load)
www/                  app.css + who-logo.svg (copied from PAKAFG_Surveillance)
app_data/             kobo_cache.rds, historic_raw.rds (not in git)
archive/ outputs/     raw Kobo downloads (last 5), built decks (not in git)
```

## Data decisions (do not change silently)

- Source: public EPI_BACKUP export, CSV, `;`-separated. Optional `KOBO_TOKEN` in .Renviron.
- Split by "Select Checklist is to be filled:" = Health Center / Session / Community. "WHO Staff
  Monthly reports" rows are not used (they were not in Power BI either).
- Visits with no date or before `EPI_START_DATE` (default 2019-01-01 since v0.4.0; 2020-01-01 in v0.3.0; was 2025-01-01)
  are dropped.
- Historic files (`data/Historic/*.xlsx`, `R/epi_historic.R`) are added to every load: converted
  to the export's column labels, deduplicated by _id, skipped when the _id is in the live export.
  `Source` column = "Kobo" / "Historic (<file>)". Old dashboards (sheets Health / Community /
  Session, 1 = Yes / 2 = No, columns matched on header text): the 2020-2023 file (East only) has no
  Session sheet, so session rows are derived from its Health sheet with only 4 indicators (tally
  sheet, immunization cards, two messages) and its unheaded Community markers are in Kobo order;
  "1_Power BI  EPI 2023.xlsx" (no _id: id = row number) has Session Oct 2023 (8 indicators, not the
  two messages), Health + Community Jan 2024. No reasons in either. Cache rebuilds when the files
  change (`epi_historic_signature`).
- "RI Data  2019-2026.xlsx" (all regions, Jan 2019 - Sep 2024, user-made _ids) repeats the two
  files above under other ids, so historic rows are also matched on the visit
  (`.epi_visit_key`: checklist, date, district, facility, answers; session key without the two
  messages): skipped when already in the live export, or in a file earlier in file-name order.
  Within one file only the exact-duplicate rule applies. Non-standard regions (NE, SE, SR) are taken
  from the province; "Kundoz" -> Kunduz.
- Yes/No % = Yes / (Yes + No); blanks not counted. Region upper case. Province spelling taken from
  the geography file when it matches ignoring case.
- Community coverage = sum(vaccinated) / sum(children 12-23 m checked) (rows with children > 0).
  Drop-out = (Penta-1 - Penta-3) / Penta-1.
- Bars by region; by province when exactly one region is selected (Power BI GeoAxis behaviour).
- Deck B shows VVM stage 3/4 inverted ("sessions with no vaccines in VVM stage 3/4") so higher
  is better everywhere. RAG bands: >= 90% green, 75-89% amber, < 75% red.
- Deliberate fixes vs Power BI: NGO supervision uses its own No measure (PBI used PEMT No); the
  last Session chart is titled "immunization cards" (PBI said "administration technique").
- The Year filter starts on the current year. "Last month" = the last complete calendar month
  (`epi_ref_month()`); if it has no data, the latest month that has data.
- Reporting compliance: a province complies in a month when it has >= 1 checklist of that type;
  provinces expected = all provinces of the selected regions in afg_geography.rds. Grid colours:
  0 = red, 1-4 = amber. Deck B counts districts instead (>= 1 checklist / districts in scope, on
  District Code).
- Deck B ignores the Year/Month filters: report month + Jan..report month (YTD).
- Deck B v7 figure rules (validated in v6, keep): national completion counts duplicate
  facility-months once, provincial / regional completion uses all records; mobile supervision only
  where mobile sessions planned > 0, PEMT monthly and NGO quarterly never combined; safety
  exception = session with any of 6 priority items unfavourable, ranked only with >= 3 sessions
  (fewer = listed, not ranked); community maps need >= 10 children, other maps >= 3 observations;
  map classes valid band / low sample / not applicable / no observations kept distinct.
- "Refresh from Kobo" is only on the Data page (avoid repeated clicks); the button disables
  itself until the download finishes.

## Deck conventions

- Deck A (`epi_deck.R`): PEI template, layouts "Title Slide", "Custom Layout" (dividers),
  "Standard with No Body". Title + footer + "Data as of" on every content slide. Two indicators
  per slide. Native mschart charts; maps PNG 300 dpi.
- Deck B (`epi_deck_review.R`): officer's default template resized to 16:9 at runtime. Text-box
  fills do not render on that template, so fills (bands, tiles, legend) are stretched solid PNGs
  (`.rv_rect`). Every slide: kicker, title, rule, footer, page number.
- Neither deck has speaker notes (removed in v0.4.0). Deck B tables
  stretched to the content width with `.rv_fit()` (12.3 in, ~92% of the slide).
- Deck B v7 (v0.6.0): ONE deck (no management-only / map-annex exports - user's choice). Slides
  come only from `epi_review_manifest()`; each module adds exactly one slide (the builder stops
  otherwise). Titles / kickers live in the manifest; references use `.rv_ref(ctx, id)`. The v6
  "by area" tables are replaced by grids of provincial maps (`.rv_grid`): all 8 health-center
  categories, 4 supervision indicators, 5 session categories, 7 safety items, 8 antigens - national
  value + lowest area in each map title. Annex = tables only (question values, exception register).
- v0.7.0: manifest column `part` (Overview / Health-center / Session / Community checklist / Year to
  date and actions / Technical annex) drives divider slides (`.rv_m_divider`), the cover contents
  and the notes structure line. The health-center grid omits the two supervision averages (mapped
  by indicator on the supervision slide) and maps vaccinator training instead.
- v0.8.0: one vaccine-safety slide (`safety_practice`): 7 item maps + a "Sessions to follow up"
  panel in the 8th cell (exception count, expired / VVM, provinces to verify first, low-sample list).
  The combined exception map slide was dropped (user: too hard to explain, repeated the item maps);
  the exception rule and `.rv_safety*` helpers are unchanged and still feed findings, tracker, A2.
- Completion dedup (v0.7.0, do not change silently): facility-month = District Code + facility +
  year + month, first record kept, applied BEFORE splitting by region / province
  (`.rv_dedup_fm`); levels must reconcile. Community "children 12-23 m" is an aggregate count -
  there is no child-level age.

## Versioning

Same as AFP_ZeroDose: PATCH = fixes/wording; MINOR = new page/slide/feature or anything that can
change numbers; MAJOR = breaking. Bump `VERSION` + `DESCRIPTION`, add a dated `CHANGELOG.md`
entry, run `Scripts/90_EPI_CHECK.R`.

## Gotchas

- Write R scripts as files. Inline `Rscript -e` mangles backslashes in regexes (can even crash R).
- Kobo export can take ~1 minute; the app blocks while it runs (single R process).
- Kobo repeats the header "Name and Quantiry of the vaccine ?"; the reader renames the second one
  to `..._1` (as the old Power Query did).
