# =============================================================================
# City of Yes: TOD Carve-Out Module
# =============================================================================
# Research Question: How many housing units were lost due to TOD exemptions
# in the NYC City of Yes rezoning, and where were those lost units distributed?
#
# Two carve-outs analyzed:
#   1. R1/R2 district exclusion from TOD eligibility
#   2. LIRR/Metro North outermost station radius reduction (0.5 mi → 0.25 mi)
#
# Confirmed inputs:
#   - MapPLUTO shapefile (fields: ZoneDist1, LotArea, ResidFAR, BuiltFAR,
#                                  UnitsRes, BBL, BldgFront, BldgDepth,
#                                  HistDist, PFIRM15_FL, FIRM07_FLA)
#   - TOD zones shapefile (field: TranstZone, five known values)
#   - LIRR/Metro North stations shapefile (fields: TBD — see stub below)
#   - NYC LION streets shapefile (street width field: TBD — see stub below)
#
# Methodology:
#   - Unit loss = (proposed FAR × LotArea / 1000 sqft avg unit) capped by
#                 district unit maximum, minus adopted baseline (UnitsRes)
#   - Spatial joins: centroid-based for performance
#   - Complex unioned geometries simplified with st_simplify(dTolerance = 10)
# =============================================================================


# =============================================================================
# SECTION 0: Setup
# =============================================================================

library(sf)
library(tidyverse)
library(tmap)

tmap_mode("view")

# CRS: NAD83 / New York Long Island (EPSG:2263) — feet
NYC_CRS <- 2263

# TOD methodology constants
AVG_UNIT_SIZE_SQFT <- 1000   # NYC EIS methodology
FEET_PER_MILE      <- 5280
TOD_MIN_LOT_SQFT   <- 5000   # original proposal minimum (not a Council mod)
WIDE_STREET_FT     <- 75     # wide street threshold for TOD eligibility

# FAR and unit cap lookup by zone prefix
# Source: NYC Council Modifications Summary (November 2024)
tod_params <- tribble(
  ~zone_prefix, ~proposed_far, ~unit_cap,
  "R3",          1.0,           3,
  "R4",          1.5,           4,
  "R5",          2.0,           5
)

# TOD zone values (confirmed from TranstZone field in tod_zones shapefile)
INNER_TRANSIT_ZONE <- "Inner Transit Zone"
OUTER_TRANSIT_ZONE <- "Outer Transit Zone"


# =============================================================================
# SECTION 1: Load Data
# =============================================================================

message("Loading spatial data...")

# --- MapPLUTO ---
pluto <- st_read("data/pluto/pluto.shp", quiet = TRUE) %>%
  st_transform(NYC_CRS)

# Confirm CRS
stopifnot(st_crs(pluto)$epsg == NYC_CRS)

# --- TOD Zones ---
tod_zones <- st_read("data/tod_zones/tod_zones.shp", quiet = TRUE) %>%
  st_transform(NYC_CRS)

stopifnot(st_crs(tod_zones)$epsg == NYC_CRS)

# Confirm the five TranstZone values are what we expect
message("TranstZone values in shapefile:")
print(unique(tod_zones$TranstZone))

# --- LIRR / Metro North Stations ---
# !! CONFIRM FIELDS: run names(stations) and head(stations) before filtering !!
stations_raw <- st_read("data/stations/stations.shp", quiet = TRUE) %>%
  st_transform(NYC_CRS)

message("Station shapefile fields: ", paste(names(stations_raw), collapse = ", "))
message("First few rows of stations:")
print(head(stations_raw))

# STUB: Filter to outermost LIRR and Metro North stations only.
# Replace the field name and values below once confirmed from the output above.
# Common patterns: a "line" or "railroad" field with values like "LIRR", "MNR"
# and a "type" or "position" field for outermost vs. intermediate stations.
#
# Example (update field names as needed):
#   stations <- stations_raw %>%
#     filter(railroad %in% c("LIRR", "Metro-North"),
#            position == "outermost")
#
# If the dataset contains ONLY outermost stations, just filter by railroad:
#   stations <- stations_raw %>%
#     filter(railroad %in% c("LIRR", "Metro-North"))

stations <- stations_raw  # <-- REPLACE THIS LINE with the filter above

# --- NYC LION Streets (wide street filter) ---
# STUB: Confirm street width field name from LION.
# Standard LION fields include "FeatureTyp", "StreetWidth_Min", "RW_TYPE"
# Run names(lion) and head(lion) to confirm, then replace below.
#
# Download: https://www.nyc.gov/site/planning/data-maps/open-data/dwn-lion.page

lion <- st_read("data/lion/geo_export.shp", quiet = TRUE) %>%
  st_transform(NYC_CRS)

message("LION fields: ", paste(names(lion), collapse = ", "))

# STUB: Filter to wide streets (≥75 ft).
# Replace "StreetWidth_Min" with the confirmed field name.
#
# Example:
#   wide_streets <- lion %>%
#     filter(StreetWidth_Min >= WIDE_STREET_FT) %>%
#     st_union() %>%
#     st_simplify(preserveTopology = TRUE, dTolerance = 10)

wide_streets <- lion %>%
  filter(StreetWidth_Min >= WIDE_STREET_FT) %>%   # <-- UPDATE FIELD NAME
  st_union() %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 10)


# =============================================================================
# SECTION 2: Shared Preprocessing
# =============================================================================

message("Preprocessing PLUTO...")

# Parse zone prefix (R1, R2, R3A, etc.) from ZoneDist1
pluto <- pluto %>%
  mutate(
    zone_prefix = str_extract(ZoneDist1, "^R\\d+[A-Z]?"),
    zone_number = as.integer(str_extract(ZoneDist1, "(?<=^R)\\d+"))
  )

# Pre-compute lot centroids for all spatial joins (faster than polygon intersects)
pluto_centroids <- pluto %>%
  st_centroid()

# Build TOD zone unions — simplified for join performance
# Only care about Inner and Outer transit zones for TOD eligibility
tod_inner_outer <- tod_zones %>%
  filter(TranstZone %in% c(INNER_TRANSIT_ZONE, OUTER_TRANSIT_ZONE)) %>%
  st_union() %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 10)

# Station buffers (for Analysis 2)
station_buf_05mi  <- stations %>%
  st_buffer(dist = 0.5 * FEET_PER_MILE) %>%
  st_union() %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 10)

station_buf_025mi <- stations %>%
  st_buffer(dist = 0.25 * FEET_PER_MILE) %>%
  st_union() %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 10)

# The "donut": within 0.5 mi but outside 0.25 mi — loses eligibility under mod
station_donut <- st_difference(station_buf_05mi, station_buf_025mi) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 10)


# =============================================================================
# SECTION 3: ANALYSIS 1
# TOD Carve-Out — R1/R2 District Exclusion
# =============================================================================
# ORIGINAL: TOD available in all R1–R5 districts within the Inner or Outer
#           Transit Zone, on lots ≥5,000 sqft on wide streets (≥75ft).
# MODIFICATION: Exclude all R1 and R2 districts from TOD eligibility.
#
# Carved-out population: R1/R2 lots that ARE ≥5,000 sqft AND on a wide street
#                        AND within the Inner or Outer Transit Zone.
#
# Unit loss formula:
#   For each carved-out lot:
#     proposed_units = min(floor(proposed_far * LotArea / AVG_UNIT_SIZE), unit_cap)
#     baseline_units = UnitsRes  (existing residential units from PLUTO)
#     lost_units     = max(0, proposed_units - baseline_units)
#
# Note: R1 and R2 are not in tod_params (they have no proposed FAR under TOD —
#       the original proposal gave them the same 1.0 FAR as R3. We apply R3
#       parameters as a reasonable proxy; note this assumption in your methods.
# =============================================================================

message("\n--- Analysis 1: R1/R2 TOD Exclusion ---")

# Step 1: Attribute filter — R1/R2 lots meeting size threshold
a1_candidates <- pluto %>%
  filter(zone_number %in% c(1, 2),
         LotArea >= TOD_MIN_LOT_SQFT)

message(sprintf("  R1/R2 lots ≥5,000 sqft: %s", format(nrow(a1_candidates), big.mark = ",")))

# Step 2: Centroid spatial filter — within Inner or Outer Transit Zone
a1_centroids <- a1_candidates %>% st_centroid()

# Confirm CRS match before spatial op
stopifnot(st_crs(a1_centroids) == st_crs(tod_inner_outer))

a1_in_tod_zone <- a1_candidates %>%
  filter(lengths(st_filter(a1_centroids, tod_inner_outer)) > 0)

message(sprintf("  After TOD zone filter: %s lots", format(nrow(a1_in_tod_zone), big.mark = ",")))

# Step 3: Wide street filter — lot centroid within X feet of a wide street
# Using a buffer approach: does the lot touch a wide street segment?
# STUB: This requires the wide_streets geometry from LION.
#       If wide_streets is not yet confirmed, comment out this step and add a note.
a1_wide_street_centroids <- a1_in_tod_zone %>% st_centroid()

# 50-foot buffer around lot centroid to check street adjacency
# (adjust if needed — lot frontage approach would be more precise)
a1_on_wide_street <- a1_in_tod_zone %>%
  filter(lengths(st_intersects(
    st_buffer(a1_wide_street_centroids, 50),
    wide_streets
  )) > 0)

message(sprintf("  After wide street filter: %s lots", format(nrow(a1_on_wide_street), big.mark = ",")))

# Step 4: Calculate unit loss
# R1/R2 treated as R3-equivalent: proposed FAR = 1.0, unit cap = 3
a1_r3_params <- tod_params %>% filter(zone_prefix == "R3")

a1_results <- a1_on_wide_street %>%
  mutate(
    proposed_far    = a1_r3_params$proposed_far,
    unit_cap        = a1_r3_params$unit_cap,
    proposed_units  = pmin(floor(proposed_far * LotArea / AVG_UNIT_SIZE_SQFT), unit_cap),
    baseline_units  = UnitsRes,
    lost_units      = pmax(0L, proposed_units - baseline_units)
  ) %>%
  filter(lost_units > 0)

a1_summary <- a1_results %>%
  st_drop_geometry() %>%
  group_by(zone_prefix) %>%
  summarise(
    n_lots     = n(),
    lost_units = sum(lost_units),
    .groups    = "drop"
  )

a1_total <- sum(a1_results$lost_units)

cat("\nAnalysis 1 — Units lost from R1/R2 TOD exclusion:\n")
print(a1_summary)
cat(sprintf("TOTAL (Analysis 1): %s units\n", format(a1_total, big.mark = ",")))


# =============================================================================
# SECTION 4: ANALYSIS 2
# TOD Carve-Out — LIRR/Metro North Radius Reduction (0.5 mi → 0.25 mi)
# =============================================================================
# ORIGINAL: TOD available within 0.5 miles of all transit, including LIRR
#           and Metro North stations.
# MODIFICATION: For outermost LIRR/Metro North stations within city limits,
#               reduce the TOD radius from 0.5 miles to 0.25 miles.
#
# Carved-out population: R3–R5 lots (already TOD-eligible) that fall in the
#                        0.25–0.5 mi donut around those stations AND are on
#                        wide streets AND meet the lot size threshold.
#
# Unit loss formula: same FAR-based approach as Analysis 1,
#                    with zone-specific FAR and unit caps from tod_params.
# =============================================================================

message("\n--- Analysis 2: LIRR/Metro North Radius Reduction ---")

# Step 1: Attribute filter — R3–R5 lots meeting size threshold
a2_candidates <- pluto %>%
  filter(zone_number %in% c(3, 4, 5),
         LotArea >= TOD_MIN_LOT_SQFT)

message(sprintf("  R3–R5 lots ≥5,000 sqft: %s", format(nrow(a2_candidates), big.mark = ",")))

# Step 2: Centroid spatial filter — within the station donut
a2_centroids <- a2_candidates %>% st_centroid()

stopifnot(st_crs(a2_centroids) == st_crs(station_donut))

a2_in_donut <- a2_candidates %>%
  filter(lengths(st_filter(a2_centroids, station_donut)) > 0)

message(sprintf("  After donut filter (0.25–0.5 mi): %s lots", format(nrow(a2_in_donut), big.mark = ",")))

# Step 3: Wide street filter — same approach as Analysis 1
a2_wide_street_centroids <- a2_in_donut %>% st_centroid()

a2_on_wide_street <- a2_in_donut %>%
  filter(lengths(st_intersects(
    st_buffer(a2_wide_street_centroids, 50),
    wide_streets
  )) > 0)

message(sprintf("  After wide street filter: %s lots", format(nrow(a2_on_wide_street), big.mark = ",")))

# Step 4: Calculate unit loss with zone-specific FAR and caps
a2_results <- a2_on_wide_street %>%
  mutate(
    # Match to zone-specific parameters (R3/R4/R5)
    zone_key = case_when(
      zone_number == 3 ~ "R3",
      zone_number == 4 ~ "R4",
      zone_number == 5 ~ "R5",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(tod_params, by = c("zone_key" = "zone_prefix")) %>%
  filter(!is.na(proposed_far)) %>%
  mutate(
    proposed_units = pmin(floor(proposed_far * LotArea / AVG_UNIT_SIZE_SQFT), unit_cap),
    baseline_units = UnitsRes,
    lost_units     = pmax(0L, proposed_units - baseline_units)
  ) %>%
  filter(lost_units > 0)

a2_summary <- a2_results %>%
  st_drop_geometry() %>%
  group_by(zone_key) %>%
  summarise(
    n_lots     = n(),
    lost_units = sum(lost_units),
    .groups    = "drop"
  )

a2_total <- sum(a2_results$lost_units)

cat("\nAnalysis 2 — Units lost from LIRR/Metro North radius reduction:\n")
print(a2_summary)
cat(sprintf("TOTAL (Analysis 2): %s units\n", format(a2_total, big.mark = ",")))


# =============================================================================
# SECTION 5: Combined Results
# =============================================================================

tod_summary <- tibble(
  Analysis            = c(
    "1. R1/R2 District Exclusion",
    "2. LIRR/Metro North Radius Reduction (0.5 → 0.25 mi)"
  ),
  `Eligible Lots (After All Filters)` = c(nrow(a1_results), nrow(a2_results)),
  `Units Lost`                        = c(a1_total, a2_total)
) %>%
  mutate(`% of TOD Total` = round(`Units Lost` / sum(`Units Lost`) * 100, 1))

cat("\n")
cat("================================================\n")
cat("  TOD MODULE: TOTAL ESTIMATED UNITS LOST        \n")
cat("================================================\n")
print(tod_summary)
cat("------------------------------------------------\n")
cat(sprintf("  TOTAL: %s units\n", format(sum(tod_summary$`Units Lost`), big.mark = ",")))
cat("================================================\n")

dir.create("output", showWarnings = FALSE)
write_csv(tod_summary, "output/tod_carveout_summary.csv")


# =============================================================================
# SECTION 6: Mapping (tmap, interactive)
# =============================================================================

message("\nBuilding maps...")

# Combine both analyses for a unified view
# Tag each lot with its analysis source
a1_map_data <- a1_results %>%
  select(BBL, ZoneDist1, LotArea, lost_units, geometry) %>%
  mutate(carveout = "R1/R2 Exclusion")

a2_map_data <- a2_results %>%
  select(BBL, ZoneDist1, LotArea, lost_units, geometry) %>%
  mutate(carveout = "LIRR/MN Radius Reduction")

all_lost_lots <- bind_rows(a1_map_data, a2_map_data) %>%
  mutate(carveout = factor(carveout))

# --- Map 1: Lost lots colored by carve-out type ---
map1 <- tm_shape(all_lost_lots) +
  tm_polygons(
    col            = "carveout",
    palette        = c("R1/R2 Exclusion" = "#E15759", "LIRR/MN Radius Reduction" = "#4E79A7"),
    title          = "TOD Carve-Out Type",
    popup.vars     = c("BBL", "ZoneDist1", "LotArea", "lost_units", "carveout"),
    popup.format   = list(lost_units = list(format = "d")),
    alpha          = 0.75,
    border.alpha   = 0.3
  ) +
  tm_layout(title = "City of Yes — TOD Units Lost by Carve-Out Type")

# --- Map 2: Lost units graduated symbols ---
# Use centroids for graduated symbol map
all_lost_centroids <- all_lost_lots %>%
  st_centroid()

map2 <- tm_shape(all_lost_centroids) +
  tm_bubbles(
    size           = "lost_units",
    col            = "carveout",
    palette        = c("R1/R2 Exclusion" = "#E15759", "LIRR/MN Radius Reduction" = "#4E79A7"),
    title.size     = "Units Lost",
    title.col      = "Carve-Out Type",
    scale          = 2,
    alpha          = 0.65,
    popup.vars     = c("BBL", "ZoneDist1", "LotArea", "lost_units", "carveout")
  ) +
  tm_layout(title = "City of Yes — TOD Units Lost (Graduated Symbols)")

# --- Map 3: Station buffers context map ---
# Shows the donut zone for Analysis 2 and the transit zone boundary
map3 <- tm_shape(tod_zones %>% filter(TranstZone %in% c(INNER_TRANSIT_ZONE, OUTER_TRANSIT_ZONE))) +
  tm_polygons(
    col          = "TranstZone",
    palette      = c("Inner Transit Zone" = "#B3CDE3", "Outer Transit Zone" = "#DECBE4"),
    alpha        = 0.4,
    title        = "TOD Zone"
  ) +
  tm_shape(station_donut) +
  tm_polygons(col = "#FBB4AE", alpha = 0.35, border.col = "red", border.alpha = 0.4,
              title = "0.25–0.5 mi Donut (Lost Eligibility)") +
  tm_shape(stations) +
  tm_dots(col = "black", size = 0.05, title = "LIRR/Metro North Stations") +
  tm_layout(title = "City of Yes — TOD Geography: Zones and Station Buffer")

# Print maps
print(map1)
print(map2)
print(map3)

# Save lot-level data for use in Burden Shift module
st_write(a1_map_data, "output/tod_a1_r1r2_lots.gpkg",      delete_dsn = TRUE, quiet = TRUE)
st_write(a2_map_data, "output/tod_a2_lirr_mn_lots.gpkg",   delete_dsn = TRUE, quiet = TRUE)
st_write(all_lost_lots, "output/tod_all_lost_lots.gpkg",    delete_dsn = TRUE, quiet = TRUE)

message("\nOutputs written to ./output/")
message("  tod_carveout_summary.csv      — summary table")
message("  tod_a1_r1r2_lots.gpkg         — Analysis 1 lot-level spatial data")
message("  tod_a2_lirr_mn_lots.gpkg      — Analysis 2 lot-level spatial data")
message("  tod_all_lost_lots.gpkg        — combined (input for Burden Shift module)")
