#' Synthetic County-Level Screening Data
#'
#' A fabricated dataset shaped like county-level newborn screening
#' summary data, used to demonstrate [build_choropleth()]. Every value
#' is randomly generated - see `data-raw/generate_synthetic_data.R` for
#' exactly how. No real surveillance data is included anywhere in this
#' package.
#'
#' @format A data frame with 10 rows and 5 columns:
#' \describe{
#'   \item{county}{Georgia county name}
#'   \item{screen_positive_count}{Fake count of screen-positive cases}
#'   \item{total_births}{Fake total births for the county}
#'   \item{subcontractor}{Fake follow-up subcontractor assignment}
#'   \item{rate_per_1000}{Fake rate per 1,000 births}
#' }
#' @source Randomly generated in `data-raw/generate_synthetic_data.R`
#'   for portfolio demonstration purposes.
"county_screening_data"


#' Synthetic Specimen-Level Data
#'
#' A fabricated dataset shaped like a specimen-level newborn screening
#' export, used to demonstrate [calculate_qi_intervals()] and
#' [build_qi_report()]. Every value is randomly generated - see
#' `data-raw/generate_synthetic_data.R` for exactly how. No real
#' surveillance data is included anywhere in this package.
#'
#' @format A data frame with 2,000 rows and 8 columns:
#' \describe{
#'   \item{specimen_id}{Fake specimen identifier}
#'   \item{collection_date}{Fake specimen collection date}
#'   \item{facility_id}{Fake birthing facility identifier}
#'   \item{unsat_flag}{1 if the fake specimen was unsatisfactory, else 0}
#'   \item{collection_month}{Collection date, truncated to year-month}
#'   \item{collection_quarter}{Collection date, truncated to year-quarter}
#'   \item{birth_date}{Fake birth date (a few days before collection)}
#'   \item{report_date}{Fake result report date (a few days after collection)}
#' }
#' @source Randomly generated in `data-raw/generate_synthetic_data.R`
#'   for portfolio demonstration purposes.
"specimen_data"
