# Changelog

## v0.8.0 — 2026-11-20

Request: the "Priority vaccine-safety exceptions" slide is hard to explain - redesign without
duplication or skip it.

**Deck B (23 slides = 19 content + 4 dividers)**
- The combined exception map slide is removed. Its content moves into the 8th cell of the
  safety-practice slide (now "Vaccine-safety practices by province"), which already maps each of the
  7 items: "Sessions to follow up" = 14 of 156 sessions with a problem in at least one of the six
  priority items (all but AEFI), expired vaccine 1 (Hilmand), VVM 3/4 0, verify first (3+ sessions)
  Ghor 3/19, Hilmand 2/11, Badakhshan 1/6, low-sample provinces listed, not ranked; reference to
  annex A2 (facility and date of every flagged session).
- Executive findings, action tracker, session-quality note and annex A2 now point to that slide.
  No figure changes: same `.rv_safety` / `.rv_safety_priority` helpers and rules.
- QA: safety figures checked on the merged slide; new check that no separate combined map slide exists.

## v0.7.0 — 2026-11-20

Requests: show how the three checklists are differentiated (chosen: divider slides); apply the
review feedback while keeping the full map grids.

**Deck B (24 slides = 20 content + 4 dividers)**
- Parts: Overview (1-3), Health-center checklist (4-7), Session checklist (8-11), Community
  checklist (12-14), Year to date and actions (15-22), Technical annex (23-24). One divider slide
  per part (checklist colour, what the checklist assesses, assessments / districts / provinces in
  the month, the slides in the part). The cover lists every part with its slide range. Kickers name
  the checklist ("Health-center checklist | October 2026"). All numbers follow the manifest.
- Session completion (slide 5): **number change** - one deduplication rule at every level: a
  facility-month (District Code + facility + year + month) counts once (first record kept) before
  splitting by region / province, so provinces and regions add up to the national total (outreach
  6,371/6,851; mobile 1,148/1,223). National values unchanged; five provincial values move by
  < 0.3 points (Hirat, Kandahar, Uruzgan outreach; Uruzgan, Zabul mobile). Dropped duplicates are
  written to Deck_B_v7_duplicate_facility_months.csv (3 of 8 pairs differ: Samangan Dehi first
  record blank; Uruzgan BHC and Kandahar Nazo Ana two visits with different values - verify).
- Health-center slide: the two supervision averages are no longer mapped (slide 7 maps the four
  indicators); vaccinator trained in the last 12 months is mapped instead (366/452, 81%; lowest
  region BADAKHSHAN 47%; valid Yes/No answers, not a staff census). Priority maps on the top row.
- Session and community grids: priority maps first (administration / safety, readiness, caregiver
  communication; Penta-3, Measles-1, BCG, vaccination card).
- Community age range: the form stores one count of "children aged 12-23 months" per assessment;
  no child-level age exists, so 24-month-old submissions cannot be identified. Reported in the
  validation file; no value changed (antigen count > children checked: 0 assessments).

## v0.6.0 — 2026-11-20

Requests: (1) the v0.5.0 deck left out maps that were in v6 - replace each table with the
provincial maps instead (like the v6 "Health-center performance by province" slide replacing
"Health-center performance by thematic category"); (2) one deck with all slides, no second export.
No number changes.

**Deck B v7 (20 slides, one deck)**
- Health center: the thematic table is replaced by the 8 category maps (national score and lowest
  region in each map title).
- Supervision: 4 tiles (rate, n/N, period) above the 4 provincial maps (PEMT / NGO outreach,
  PEMT monthly / NGO quarterly mobile), lowest region under each.
- Session quality: the 5 category maps (labels below 90%; provinces with an expired-vaccine / VVM
  observation starred) + the weakest administration / safety / readiness questions.
- Safety: combined exception map (unchanged) followed by the 7 safety-item maps (back from the v6
  annex), provinces with an unfavourable session labelled n/N.
- Community: the 8 antigen maps (national %, n/N, lowest province 10+ children) with the
  Measles-2 caveat, P1-P3 difference and negative-difference note.
- Annex = tables only: A1 all 22 health-center questions by region (exact values, n/N);
  A2 register of every flagged safety session.
- Removed: management-only export, the separate "full technical map annex" export
  (`epi_build_map_annex()` and its helpers) - all those maps are now in the deck.

**App / scripts**
- PowerPoint page: one "Build EPI deck" button plus the slide list.
- `epi_build_review_deck()` has no export option; Scripts/02 `review` builds the single deck;
  Scripts/03 checks every v6 map is present (32 maps) and compares each against the v6 values.

## v0.5.0 — 2026-11-20

Request: redesign deck B (v6 integrated deck, 29 slides) to ~19-21 slides: fewer duplicated
charts / maps, selected tables replaced by provincial maps, new order (finding -> geography ->
implication -> action), one manifest for both exports. No number changes: every figure uses the
v6 calculation rules (checked by Scripts/03_review_deck_qa.R).

**Generator**
- New `R/epi_deck_review_v7.R`: `epi_review_manifest()` lists the slides as named modules
  (id, section, title, module, period, map type, indicators, v6 source slides, main / annex).
  `epi_build_review_deck(export = "integrated" | "management")` builds both decks from it; each
  module adds one slide, so slide numbers and every "see slide / annex" reference are computed.
  The function returns the manifest (with slide numbers) and a log of the modules' map-selection
  decisions as attributes.
- One helper per figure shared by every slide that shows it: facility-month completion,
  priority safety exceptions (overall, by province, eligible / low-sample priorities), province
  frames for maps, map classes (valid band / low sample / not applicable / no observations).
- `R/epi_deck_review.R` keeps the look and the reused slides; obsolete v6 slide functions removed
  (v6 file kept in archive/code_v6/). `epi_build_map_annex()` (all v6 maps) is kept.
- New `Scripts/03_review_deck_qa.R`: builds both decks, rechecks figures and every provincial map
  against the v6 formulas, single-region / no-data / management-only builds, navigation
  references; writes manifest, v6 -> v7 mapping, build log, QA results and a validation report.

**Deck B v7 (21 slides integrated, 17 management-only; was 29)**
- Main: title; executive findings (tiles + five findings with the decision and evidence slide);
  reporting presence (district map + districts by checklist type); outreach / mobile completion
  (two maps + national KPIs); health-center readiness and defaulter follow-up (two maps; planning,
  cold chain, recording, proximity as KPIs); supervision (four indicator tiles with n/N and period
  + two mobile maps); session quality (maps only where geography adds information + national
  scores + administration / safety questions); safety exceptions (map + 156 / 14 / expired / VVM
  tiles, eligible priorities ranked, low-sample provinces listed separately); community status
  (Penta-3 and Measles-1 maps, Measles-2 caveat, negative P1-P3 difference flagged); reasons;
  YTD overview + monthly activity; district presence by region x month; three trend tables;
  action tracker (geography with n/N, evidence slide column); definitions.
- Technical annex A1-A4: health-center questions shown only as national scores (question x
  region table); outreach supervision maps; individual safety-practice maps (items with 2+
  exceptions) + register of every flagged session; additional antigen maps (antigens with the same
  provincial pattern as Penta-3 / Measles-1 are not mapped; table states why).
- Province labels larger; provinces with an expired-vaccine / VVM observation are starred on the
  session-quality maps (a high composite score does not clear them).

**App**
- PowerPoint page: deck B card describes v7, shows the slide manifest, and has a "Full technical
  map annex (all v6 maps)" download for technical users.

## v0.4.0 — 2026-11-19

Requests: (1) the Data page sometimes stays on "Downloading from Kobo..." after the data has
updated; (2) import "RI Data  2019-2026.xlsx" (some rows already imported, _id made up);
(3) Naveed Saeed EPI deck (deck B): monthly reporting trends to show provinces / districts that do
not report, no speaker notes, tables filling the slide. Numbers change for 2019-Sep 2024.

**Fix**
- Refresh button stuck on "Downloading from Kobo...": the second `on.exit()` in the refresh
  observer replaced the first one (no `add = TRUE`), so the message that re-enables the button
  was never sent. Fixed.

**Historic data**
- New file "RI Data  2019-2026.xlsx" (sheets Health / Community / Session, 55,342 rows, all
  regions, Jan 2019 - Sep 2024): 49,209 rows added; 6,030 skipped because the same visit is
  already in an earlier file (it contains the 2020-2023 East health/community rows and the
  Oct 2023 / Jan 2024 rows of "1_Power BI  EPI 2023.xlsx"); 103 skipped because the visit is
  already in Kobo (Sep 2024, when both were in use).
- Duplicates across sources are now found by the visit, not only the _id: same checklist, date,
  district, facility and answers. A row is skipped when it is in the live export or in a file
  read earlier (file-name order). The Data page lists, per file, rows read / already in Kobo /
  already in an earlier file / added.
- Region codes NE, SE, SR in old sheets are taken from the province (NORTHEAST, SOUTHEAST, SOUTH);
  "Kundoz" -> Kunduz. Malformed Day/Month/Year values no longer stop the file from loading (the
  row has no date and is left out, as before).
- Start date (EPI_START_DATE) default 2020-01-01 -> 2019-01-01, so 2019 shows.

**Naveed Saeed EPI deck (deck B, was "management review deck")**
- Renamed in the app, the cover and the file name (Naveed_Saeed_EPI_Deck_*.pptx). Footer:
  "Routine EPI Supportive Supervision · NS EPI deck | period | slide note"; "All regions" is no
  longer repeated there (a region / province selection still is).
- Reviewed for action (25 slides for all regions, was 23 in v0.3.0):
  - New "Action points" slide after "At a glance", built from the data: provinces with no
    checklist, provinces missing session / community checklists, districts with no checklist
    (largest gaps, and districts with none since January), health-center and session indicators
    below 75% with the regions / provinces that are red, community coverage below 75%
    (Penta-3, Measles-1, Measles-2) and drop-out of 10% or more. Each row says where and what to
    do (one action text per indicator).
  - New "Districts with no checklist in <month>": every province (reporting / not reporting,
    names; * = none since January), 17 provinces per slide (34 provinces = 2 slides).
  - New "Districts reporting by region and month" (count and % of the region's districts).
  - "Reporting compliance by region" (Part 1) now counts districts, not provinces: per region,
    districts with at least one health-center / session / community checklist in the month, and
    with any checklist, out of the region's districts in the boundary file (400 in all); same
    red -> white -> blue shading as the region x month slide. "No checklist at all" provinces kept.
    (The app's Home page still shows the province version.)
  - New "Maps" section at the end (report month, before the definitions; on trial): districts
    with a checklist (reported / none this month / none since January, with counts and the
    provinces with most districts missing), health-center and session score by province
    (average % Yes of the indicators, RAG, lowest provinces with their weakest indicator),
    Penta-3 coverage by province (provinces with >= 10 children; lowest with Measles-1 and
    drop-out). Builds take ~1 minute longer because of the maps.
  - Order for the flow: Part 1 = glance, action points, then reporting (maps -> region table ->
    district names), then performance. Part 2 = reporting (region x month -> province x month),
    then one checklist at a time (health center by month + scorecard, session by month +
    scorecard, community coverage by month + reasons), provinces needing attention.
  - "Provinces needing attention" adds months with no checklist and the weakest health-center
    indicator per province.
  - Removed: "Reporting compliance by month" and "Key indicators by month" (line charts; the
    month-by-month tables show the same with names).
  - Districts expected = all 400 districts of the boundary file in the selected regions,
    matched on District Code.
- Speaker notes removed from every slide, in this deck and in the dashboard deck (deck A).
- Part dividers redesigned: the period in large type ("October 2026", "January - October 2026"),
  with "Last month" / "Year to date" in small capitals underneath.
- Tables are stretched to the content width (12.3 in, ~92% of the slide); rows grow to use the
  height; tables stretched a lot get a 2 pt larger font.

## v0.3.0 — 2026-11-18

Request: import the historic data in data/Historic (old Kobo export, and the 2022-2023 Excel
dashboard), drop duplicates by _id, flag it as historic. Numbers change for any selection that
includes years before 2025.

- New `R/epi_historic.R`: every .xlsx in data/Historic is read on load and refresh, turned into
  rows with the live export's column labels, and added to the Kobo data. Rows whose _id is already
  in the live export are skipped (live wins); duplicates inside a file are dropped by _id. Read once,
  then cached in app_data/historic_raw.rds until a file changes.
- "Routine EPI Dashboard _ KOBO.xlsx" (Kobo XML column names, Jul 2024 - Aug 2026): all 24,945
  rows are still in today's Kobo export with identical values (checked by _id), so it adds 0 rows
  now. It is kept as a fallback in case Kobo drops old submissions.
- "Dashboard of Routine EPI 2022-2023.xlsx" actually covers Jan 2020 - Dec 2023, East region
  (Kunar, Laghman, Nangarhar, Nuristan): 2,662 health-center, 2,619 session and 2,454 community
  rows added (answers 1 = Yes, 2 = No; 15 health rows and 1 community row have no valid date and
  are left out; 55 community rows identical to another row except _id were dropped as double
  entries). The old form was one combined checklist:
  - Health center: all 9 indicators.
  - Session (visits where a session was seen): only tally sheet, immunization cards and the two
    messages; the other 6 session questions did not exist and stay blank.
  - Community: children checked + the 13 markers (the marker columns had no header; they follow
    the Kobo order, confirmed by Penta-3 <= Penta-1 etc. in > 98% of rows). No reasons.
- "1_Power BI  EPI 2023.xlsx" (all regions, no _id: ids = row number, exact duplicate rows
  dropped): Health sheet Jan 2024 (552 health-center rows, 8 without a valid date), "Supervision
  Session" sheet Oct 2023 (149 session rows: 8 of the 10 session questions; its single "deliver
  communication messages" question is not the two Kobo messages, so those stay blank; 17 exact
  duplicates dropped), Community sheet Jan 2024 (193 rows, markers matched by header). No overlap
  with Kobo (starts Jul 2024) or the 2020-2023 file. When a file has its own Session sheet, no
  session rows are derived from its Health sheet.
- Every sheet named Health / Community / Session in an old dashboard file is read, with columns
  matched on header text; a new or changed file in data/Historic is picked up on the next app start
  (the cache stores a signature of the files).
- Start date (EPI_START_DATE) default 2025-01-01 -> 2020-01-01, so the history and Jul-Dec 2024
  from Kobo show. The Year filter still opens on the current year, so default views do not change.
- Flag: a `Source` column on every record ("Kobo" or "Historic (<file>)"), shown on the Data page
  and in the Excel download; an amber note under the filters when a selection includes historic
  records; the Data page lists rows read / added per file; both decks' source line adds
  "+ N historic records" when they include any.
- If the cache is from an older version, it is now rebuilt from the last raw download in
  archive/ (no need to refresh from Kobo after an upgrade).

Verified: `Scripts/90_EPI_CHECK.R` (25 checks pass, 5 new: decoding, historic rows flagged, every file read, no
duplicate _id, Kobo historic file = live export for the same _id).

## v0.2.1 — 2026-11-18

Requests: blank space on the Data page; a label under each coverage bar, with years side by side
when several are selected; make the compliance denominator clear. No numbers change.

- Data page: short column headers on screen (the long Kobo question texts made the header row
  very tall and left a blank band); the Excel download keeps the full question text.
- Community "Overall coverage by marker" (app and dashboard deck): one bar per marker with its name
  under the bar and the % on top; years side by side when more than one year is selected (current
  year dark blue, earlier years lighter).
- Compliance by region (Home and management deck): cells read "6 of 8 (75%)"; the denominator
  column is labelled, with a note: denominator = provinces of the region in the boundary file
  (34 for Afghanistan), numerator = provinces with at least one checklist of that type in the month.

Verified: `Scripts/90_EPI_CHECK.R` (20 checks pass); chart, table and both slides rendered.

## v0.2.0 — 2026-11-18

Request: last month analysis first, then the current year with month-wise trends, and tracking of
compliance; the pages should show the current year by default.

**App**
- Year filter starts on the current year (Reset also returns to it).
- Home: "Last month" card (last complete month: checklists by type with change vs the previous
  month, provinces reporting, reporting compliance by region) - uses the region/province filters
  only. Then the selected period: compliance by month (share of provinces reporting each
  checklist type) and a province x month grid (red = no checklist, amber = fewer than 5).
- PowerPoint page: "Report month" for the management review deck (default: last complete month).

**Management review deck (restructured, 23 slides for all regions)**
- Part 1, last month: at a glance (vs previous month), reporting compliance by region, districts
  reached, health-center and session scorecards, community coverage by region.
- Part 2, year to date (January to report month): key numbers and findings, checklists per month,
  compliance by month, province x month grid (split over slides, whole regions), health-center /
  session indicators by month, community coverage by month (+ drop-out), trend lines, YTD
  scorecards, reasons, provinces needing attention, definitions.
- The deck ignores the Year/Month filters; period = report month and its year to date.
- Rankings and the region x month slide were dropped (covered by the month-wise tables and grid).

**Compliance definition (new)**: a province complies in a month when it has at least one checklist
of that type; provinces expected = all provinces of the selected regions in the boundary file.

Verified: `Scripts/90_EPI_CHECK.R` (18 checks pass), both decks rendered in PowerPoint.

## v0.1.0 — 2026-11-18

First version. Replaces the Power BI report `Routine_EPI_Dashboard.pbix`, which read the Kobo
export through Excel/Power Query, with a Shiny app that downloads the Kobo export directly.

**Data**
- "Refresh from Kobo" (Data page only) downloads the public EPI_BACKUP export
  (`.../export-settings/esKvGvapbsM4y4zohJ2AuRL/data.csv`); handles Kobo's 503 + Retry-After
  while the export is built. Last 5 raw downloads kept in `archive/kobo_downloads/`.
- Prepared tables cached in `app_data/kobo_cache.rds`; until the first refresh the app uses the
  Excel snapshot `data/EPI_Kobo_Data.xlsx` copied from the old Power BI folder.
- Same rules as the Power Query: checklist type split (Health Center / Session / Community),
  visits from 2025-01-01 (`EPI_START_DATE`), Yes/No % = Yes / (Yes + No), region in upper case.

**Pages**
- Home: checklists per month by type, region x month table, region x type table, regions by month.
- Health center (9 indicators), Session (10 indicators), Community (coverage by marker, by year
  and by region/province, reasons not vaccinated, coverage table), as in Power BI.
- Maps follow the project protocol: `reference/geography/afg_geography.rds` + ggplot, joined on
  District Code (all codes in the current data match).
- Data page: filtered records + Excel download.

**PowerPoint (two separate decks)**
- A. Dashboard deck (PEI template): the three dashboard pages slide by slide.
- B. Management review deck (own white 16:9 design): executive summary, reporting activity,
  geographic reach, RAG scorecards, rankings, community coverage and drop-out, trends,
  provinces needing attention, definitions.

**Differences from Power BI (deliberate)**
- The Power BI "NGO supervision" chart used the PEMT "No" measure; here NGO uses its own Yes/No.
- The last Session chart was titled "administration technique" but showed immunization cards;
  it is titled "immunization cards" here.

Verified: `Scripts/90_EPI_CHECK.R` (all checks pass), live Kobo refresh (27,898 rows, ~60 s),
both decks rendered in PowerPoint and reviewed slide by slide.
