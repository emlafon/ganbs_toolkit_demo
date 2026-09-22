# ============================================================
# generate_synthetic_data.R
# ------------------------------------------------------------
# Purpose: Build fake (but realistically-shaped) newborn
# screening data so the portfolio functions in this package
# have something to run against. None of this is real DPH
# data - it's randomly generated to just LOOK like it.
#
# This script lives in data-raw/ by convention (usethis
# calls this folder "data-raw" specifically) - it's the
# recipe for creating example data, not the data itself.
# ============================================================

library(dplyr)
library(tidyr)

# set.seed() locks in the random number generator so that
# every time this script runs, it produces the SAME "random"
# data. Useful for reproducibility - anyone cloning this repo
# gets identical example output.
set.seed(42)

# ------------------------------------------------------------
# 1. County-level data for the choropleth map function
# ------------------------------------------------------------
# We need a list of Georgia county names to join against
# tigris shapefiles later. This is a small subset for the demo
# - the real function would use all 159 GA counties.
ga_counties_demo <- c(
  "Fulton", "Gwinnett", "Cobb", "DeKalb", "Chatham",
  "Richmond", "Muscogee", "Clarke", "Bibb", "Hall"
)

# tibble() is the tidyverse version of data.frame() - behaves
# more predictably (e.g., never auto-converts strings to factors)
county_screening_data <- tibble(
  county = ga_counties_demo,
  # rpois() draws random counts from a Poisson distribution,
  # which is a natural fit for "count of rare events" data
  # like screen-positive cases - matches how you'd model this
  # in real analysis anyway
  screen_positive_count = rpois(n = length(ga_counties_demo), lambda = 8),
  total_births = round(runif(length(ga_counties_demo), min = 500, max = 5000)),
  # This mimics your real SCD subcontractor split (AU vs CHOA)
  # so the choropleth function's color-by-subcontractor logic
  # has something to key off of
  subcontractor = sample(c("Subcontractor_A", "Subcontractor_B"),
                          size = length(ga_counties_demo),
                          replace = TRUE)
) %>%
  # mutate() adds a new column - here, a simple rate calculation
  # (per 1,000 births) which is a standard epi metric
  mutate(rate_per_1000 = round((screen_positive_count / total_births) * 1000, 2))

# ------------------------------------------------------------
# 2. Specimen-level data for the unsat trend pipeline
# ------------------------------------------------------------
# This fakes the shape of a SendSS-style specimen export:
# one row per specimen, with a collection date and an
# unsatisfactory (unsat) flag.

n_specimens <- 2000

# seq() with "by = " builds a sequence of dates - here, every
# day across two fake years, so we can later aggregate up to
# months/quarters for trend charts
date_range <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day")

specimen_data <- tibble(
  specimen_id = paste0("SPEC", sprintf("%05d", 1:n_specimens)),
  # sample() with replace = TRUE lets the same date get picked
  # more than once - realistic, since many specimens are
  # collected on any given day
  collection_date = sample(date_range, n_specimens, replace = TRUE),
  facility_id = paste0("FAC", sprintf("%03d", sample(1:20, n_specimens, replace = TRUE))),
  # rbinom() flips a weighted coin per specimen - here about a
  # 3% unsat rate, which is in a realistic ballpark for NBS
  # programs (your real rate will differ, this is just a demo)
  unsat_flag = rbinom(n_specimens, size = 1, prob = 0.03)
) %>%
  mutate(
    # Break collection_date into month/quarter buckets up front,
    # since the trend function will need to aggregate by those
    collection_month = format(collection_date, "%Y-%m"),
    collection_quarter = paste0(format(collection_date, "%Y"), "-Q",
                                 ceiling(as.numeric(format(collection_date, "%m")) / 3)),
    # birth_date is mostly 0-2 days before collection (realistic - most
    # specimens are collected within the first couple days of life),
    # but sample() with weighted probabilities gives a long tail out to
    # 20 days so the QI report's later bins (7-14 days, >14 days)
    # actually have some specimens in them too, instead of empty rows
    birth_date = collection_date - sample(
      x = 0:20,
      size = n_specimens,
      replace = TRUE,
      # prob = weights each offset value - heavily favors 0-2 days,
      # then tapers off, mimicking a realistic distribution instead
      # of a flat/uniform one
      prob = c(rep(10, 3), rep(2, 5), rep(0.5, 13))
    ),
    # report_date is a few days after collection, simulating lab
    # turnaround time - runif(1,10) gives a realistic spread
    report_date = collection_date + round(runif(n_specimens, min = 1, max = 10))
  )

# Introduce a small handful of missing birth_date values (about 1%) so
# the "Unknown" category bin in the QI report isn't always empty either -
# missing/bad dates do happen in real specimen data, so this is a more
# honest demo of what the categorize_time() function needs to handle
na_rows <- sample(seq_len(n_specimens), size = round(n_specimens * 0.01))
specimen_data$birth_date[na_rows] <- NA

# ------------------------------------------------------------
# 3. Save both datasets as package data
# ------------------------------------------------------------
# usethis::use_data() is the standard way to bundle example
# data INSIDE an R package (saves to data/ as .rda files).
# Once this runs, users can load your example data just by
# typing data(county_screening_data) after installing the
# package - no separate CSV download needed.
usethis::use_data(county_screening_data, overwrite = TRUE)
usethis::use_data(specimen_data, overwrite = TRUE)

# Quick sanity check - print the first few rows of each so you
# can eyeball that the fake data looks reasonable before moving on
print(head(county_screening_data))
print(head(specimen_data))
