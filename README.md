# ganbs.toolkit.demo

A portfolio demonstration of R functions built for newborn screening (NBS)
epidemiology reporting workflows, adapted from tools used in a real state
public health surveillance program.

**All data in this package is synthetic** — randomly generated to mimic the
shape of real specimen and county-level surveillance data, with no real
values, facility names, or case counts included.

## What's in here

- **`data-raw/generate_synthetic_data.R`** — generates two bundled
  example datasets (`county_screening_data`, `specimen_data`) with
  fully fake values, used by every function below
- **`build_choropleth()`** — plots a Georgia county-level metric as a
  quartile-shaded map, using `tigris` Census shapefiles
- **`categorize_time()` / `calculate_qi_intervals()` / `build_qi_report()`**
  — calculates newborn screening timeliness intervals (birth-to-collection,
  collection-to-report) and buckets them into standard reporting bins,
  optionally exporting an Excel summary workbook
- **`map_portal_condition()` / `map_gender()` / `map_ethnicity()` / `map_race()`**
  — maps messy free-text lab result strings and demographic codes to
  standardized reporting-portal terminology, using public RUSP
  (Recommended Uniform Screening Panel) disorder names

## Try it

```r
devtools::load_all()

# County choropleth
build_choropleth(county_screening_data)

# Timeliness report
build_qi_report(specimen_data)

# Condition-name mapping
map_portal_condition(condition = "MCADD", source = "General Abnormal")
```

## Installation

```r
# install.packages("devtools")
devtools::install_github("yourusername/ganbs_toolkit_demo")
```

## Background

Built by a state newborn screening epidemiologist to demonstrate package
development, data pipeline design, and epi visualization skills using R
(tidyverse, ggplot2, tigris).
