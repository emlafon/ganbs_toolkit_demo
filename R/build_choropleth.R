#' Build a Georgia County Choropleth Map
#'
#' Plots a county-level metric (e.g., a screening rate) across Georgia
#' counties, shaded by quartile. This is a portfolio-demo rebuild of a
#' choropleth pattern originally built for internal NBS reporting -
#' rewritten here to run entirely on synthetic example data, with no
#' database connections or real surveillance results.
#'
#' @param county_data A data frame with one row per county. Must contain
#'   a column with county names (matching Census county names, e.g.
#'   "Fulton" not "Fulton County") and a numeric column to map.
#' @param county_col String. Name of the column in `county_data` holding
#'   county names. Defaults to "county".
#' @param value_col String. Name of the numeric column to shade by.
#'   Defaults to "rate_per_1000".
#' @param n_quartiles Integer. How many quantile bins to split the value
#'   into for shading. Defaults to 4 (quartiles).
#'
#' @return A ggplot object - a filled Georgia county map.
#'
#' @details
#' This function pulls Georgia's county boundary shapefile from the
#' Census Bureau via the `tigris` package the first time it runs, which
#' requires an internet connection. `tigris` caches the shapefile
#' locally after the first download, so repeat runs are fast.
#'
#' @examples
#' # Load the bundled synthetic example data
#' data(county_screening_data)
#' build_choropleth(county_screening_data)
#'
#' @export
build_choropleth <- function(county_data,
                              county_col = "county",
                              value_col = "rate_per_1000",
                              n_quartiles = 4) {

  # These packages are only needed inside this function, so we call them
  # with :: instead of library() - keeps the package's dependency list
  # explicit and avoids cluttering the user's whole R session
  if (!requireNamespace("tigris", quietly = TRUE)) {
    stop("Package 'tigris' is required for build_choropleth(). Install it with install.packages('tigris').")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for build_choropleth(). Install it with install.packages('sf').")
  }

  # options(tigris_use_cache = TRUE) tells tigris to save downloaded
  # shapefiles to a local cache folder instead of re-downloading them
  # every time this function runs - much faster on repeat use
  options(tigris_use_cache = TRUE)

  # tigris::counties() pulls county boundary shapefiles from the Census
  # Bureau's TIGER/Line database. state = "GA" limits it to Georgia's
  # 159 counties. class = "sf" returns it as a "simple features" spatial
  # object, which ggplot2 can plot directly with geom_sf().
  # cb = TRUE uses the lower-resolution "cartographic boundary" file,
  # which is smaller/faster and plenty precise for a state-level map.
  ga_counties_sf <- tigris::counties(state = "GA", cb = TRUE, class = "sf")

  # The shapefile's county name column is always called NAME (e.g.,
  # "Fulton"). We rename it here with plain base R - not a dplyr/NSE
  # verb - so the join below works regardless of what column name the
  # user's own data happens to use, and so R CMD check doesn't flag
  # NAME as an undefined "global variable" (a common false alarm when
  # a literal column name is passed to a tidy-eval function like
  # dplyr::rename() instead of assigned this way).
  names(ga_counties_sf)[names(ga_counties_sf) == "NAME"] <- county_col

  # Join the user's data (rates, counts, whatever) onto the spatial
  # boundaries by matching county name. left_join keeps every county
  # shape even if a county is missing from county_data (it'll just
  # show up blank/NA on the map, which is useful for spotting gaps).
  # The native |> pipe (built into base R since version 4.1, no
  # library/import needed) replaces the older magrittr %>% pipe here.
  mapped_data <- ga_counties_sf |>
    dplyr::left_join(county_data, by = county_col)

  # ntile() splits the value column into n_quartiles equal-sized groups
  # (1 = lowest values, n_quartiles = highest) - this is what creates
  # the "Q1/Q2/Q3/Q4" style shading instead of a continuous color scale,
  # which is often easier to read at a glance on a map.
  # .data[[value_col]] is the tidy-eval-safe way to reference a column
  # by a string name stored in a variable, inside a dplyr verb.
  mapped_data <- mapped_data |>
    dplyr::mutate(
      quartile = dplyr::ntile(.data[[value_col]], n_quartiles),
      # .data$quartile (not bare quartile) refers to the column we just
      # created on the line above, within this same mutate() call - the
      # .data pronoun is what lets R CMD check confirm this isn't an
      # undefined global variable
      quartile = factor(.data$quartile, labels = paste0("Q", 1:n_quartiles))
    )

  # geom_sf() knows how to draw spatial polygons directly from an sf
  # object - no need to manually extract lat/long coordinates the way
  # older base-R mapping approaches required.
  # aes(fill = .data$quartile) - rather than bare aes(fill = quartile) -
  # is the tidy-eval-safe way to point ggplot2 at a column, and avoids
  # R CMD check flagging 'quartile' as an undefined global variable.
  ggplot2::ggplot(mapped_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$quartile), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_brewer(palette = "Blues", na.value = "grey90", name = "Quartile") +
    ggplot2::labs(
      title = "Georgia County Choropleth (Demo)",
      subtitle = paste("Shaded by", value_col, "quartile - synthetic example data")
    ) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}
