#' Categorize a Time Interval into Reporting Bins
#'
#' Buckets a numeric time value (days or hours) into the ordered
#' category labels commonly used in newborn screening timeliness
#' reporting (e.g., NewSTEPs/Title V style bins). Returns a factor with
#' levels in the correct display order, so counts and tables sort
#' properly (e.g., "2 days" before "10 days") instead of alphabetically.
#'
#' @param value A numeric vector of time intervals.
#' @param type Either "days" or "hours". Controls which bin set is used.
#'
#' @return An ordered factor with the same length as `value`.
#'
#' @examples
#' categorize_time(c(0, 3, 12, -1), type = "days")
#'
#' @export
categorize_time <- function(value, type = "days") {
  if (type == "hours") {
    levs <- c("0-11.9 hours old", "12-24 hours old", "24.1-48 hours old",
              "48.1-72 hours old", ">72 hours old", "Unknown")
    out <- dplyr::case_when(
      value < 0 | is.na(value) ~ "Unknown",
      value <= 11.9 ~ "0-11.9 hours old",
      value <= 24 ~ "12-24 hours old",
      value <= 48 ~ "24.1-48 hours old",
      value <= 72 ~ "48.1-72 hours old",
      TRUE ~ ">72 hours old"
    )
  } else {
    levs <- c("0 days", "1 day", "2 days", "3 days", "4 days", "5 days",
              "6 days", "7-14 days", ">14 days", "Unknown")
    out <- dplyr::case_when(
      value < 0 | is.na(value) ~ "Unknown",
      value == 0 ~ "0 days",
      value == 1 ~ "1 day",
      value == 2 ~ "2 days",
      value == 3 ~ "3 days",
      value == 4 ~ "4 days",
      value == 5 ~ "5 days",
      value == 6 ~ "6 days",
      value >= 7 & value <= 14 ~ "7-14 days",
      value > 14 ~ ">14 days",
      TRUE ~ "Unknown"
    )
  }
  factor(out, levels = levs)
}


#' Calculate Newborn Screening Timeliness Intervals
#'
#' Takes specimen-level data with birth, collection, and report dates,
#' and calculates the standard timeliness intervals used in NBS quality
#' indicator reporting: birth-to-collection and collection-to-report.
#' Each interval is also bucketed into reporting-ready category bins
#' using [categorize_time()].
#'
#' This is a portfolio-demo rebuild of a real annual QI reporting
#' pipeline - genericized to take a data frame argument (rather than
#' reading fixed file paths) so it can run on any similarly-shaped
#' dataset, including the synthetic example data bundled with this
#' package.
#'
#' @param specimen_df A data frame with one row per specimen. Must
#'   contain Date columns for birth, collection, and report dates.
#' @param birth_col String. Column name holding the birth date.
#'   Defaults to "birth_date".
#' @param collection_col String. Column name holding the specimen
#'   collection date. Defaults to "collection_date".
#' @param report_col String. Column name holding the result report
#'   date. Defaults to "report_date".
#'
#' @return The input data frame with four new columns added:
#'   `days_birth_to_collection`, `days_collection_to_report`, and their
#'   categorized versions `birth_to_collection_cat` /
#'   `collection_to_report_cat`.
#'
#' @examples
#' data(specimen_data)
#' result <- calculate_qi_intervals(specimen_data)
#' table(result$birth_to_collection_cat)
#'
#' @importFrom dplyr .data
#' @export
calculate_qi_intervals <- function(specimen_df,
                                    birth_col = "birth_date",
                                    collection_col = "collection_date",
                                    report_col = "report_date") {

  specimen_df |>
    dplyr::mutate(
      # as.numeric(difftime(...)) turns a Date subtraction into a
      # plain number of days, which is easier to bucket with case_when
      # than working with difftime objects directly
      days_birth_to_collection = as.numeric(difftime(
        .data[[collection_col]], .data[[birth_col]], units = "days"
      )),
      days_collection_to_report = as.numeric(difftime(
        .data[[report_col]], .data[[collection_col]], units = "days"
      )),
      # Apply the shared categorize_time() helper to both intervals.
      # .data$days_birth_to_collection refers to the column we just
      # created two lines above, in this same mutate() call -
      # referencing it via .data (instead of a bare name) is what lets
      # R CMD check confirm this isn't an undefined global variable.
      birth_to_collection_cat = categorize_time(.data$days_birth_to_collection, type = "days"),
      collection_to_report_cat = categorize_time(.data$days_collection_to_report, type = "days")
    )
}


#' Build a Timeliness Summary Report
#'
#' Runs [calculate_qi_intervals()] on specimen-level data, then
#' summarizes counts in each timeliness category and (optionally)
#' writes the result to an Excel workbook - one sheet per interval,
#' matching the shape of a real NewSTEPs/Title V QI submission.
#'
#' @param specimen_df A data frame with one row per specimen (see
#'   [calculate_qi_intervals()] for required columns).
#' @param output_path Optional. If provided, an .xlsx file is written
#'   to this path. If NULL (the default), no file is written - the
#'   summary tables are just returned as a list.
#'
#' @return A named list of two summary tibbles: `birth_to_collection`
#'   and `collection_to_report`.
#'
#' @examples
#' data(specimen_data)
#' summary_tables <- build_qi_report(specimen_data)
#' summary_tables$birth_to_collection
#'
#' @export
build_qi_report <- function(specimen_df, output_path = NULL) {

  intervals <- calculate_qi_intervals(specimen_df)

  # .drop = FALSE keeps every category level in the output table even
  # if a level has zero specimens in it this period - important for
  # reporting, since a QI submission shouldn't silently drop a bin
  summary_list <- list(
    birth_to_collection = intervals |>
      dplyr::count(.data$birth_to_collection_cat, .drop = FALSE),
    collection_to_report = intervals |>
      dplyr::count(.data$collection_to_report_cat, .drop = FALSE)
  )

  # Only write a file if the caller actually asked for one - keeps
  # this function usable for quick interactive checks too, not just
  # full report generation
  if (!is.null(output_path)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required to write output_path. Install it with install.packages('openxlsx').")
    }
    openxlsx::write.xlsx(summary_list, file = output_path, overwrite = TRUE)
    message("Report written to: ", output_path)
  }

  summary_list
}
