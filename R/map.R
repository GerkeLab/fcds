# Declare tidy eval default arguments as global variables for rcmd check
if(getRversion() >= "2.15.1") utils::globalVariables(c("rate"))

#' Join Florida County Boundaries to a data frame
#'
#' Uses the `sf` and `USAboundaries` packages to join the input data with the
#' county shapes required for spatial data visualization. See
#' [USAboundaries::us_counties()] for more information about the county
#' shapes used.
#'
#' @return An `sf` data frame, created by [sf::st_sf()].
#' @param data A data frame
#' @param ... Additional arguments passed to [USAboundaries::us_counties()].
#' @export
join_boundaries_fl <- function(data, ...) {
  requires_package(c("sf", "USAboundaries"), "join_boundaries_fl()")

  florida_counties <- USAboundaries::us_counties(states = "Florida", ...) %>%
    select(county_fips = .data$countyfp, "geometry", "geoid")

  if (!"county_fips" %in% names(data)) {
    county_fips <- fcds::county_fips_fl
    names(county_fips) <- sub("^fips_code$", "county_fips", names(county_fips))

    data <-
      data %>%
      quiet_left_join(county_fips, by = "county_name") %>%
      mutate(county_fips = sprintf("%03d", as.integer(.data$county_fips)))
  }

  sf::st_sf(quiet_inner_join(florida_counties, data, by = "county_fips"))
}

#' @describeIn fcds_map Add a column with the name "`label`" to the input data.
#'   The `...` are passed to [glue::glue_data()], so you can construct labels
#'   row-wise with references to the column in `data`.
#' @export
fcds_map_add_label <- function(data, ...) {
  data$label <- data %>%
    glue::glue_data(...) %>%
    purrr::map(~ {
      attr(.x, "html") <- TRUE
      class(.x) <- c("html", "character")
      .x
    })

  data
}

#' Map FCDS Incidence Data
#'
#' Two interfaces are provided to mapping funcitonality in R. `fcds_map()` uses
#' [ggplot2::geom_sf()] and `fcds_map_leaflet()` uses [leaflet::leaflet()] to
#' create static and interactive plots of the input data. The defaults of both
#' functions are designed to work with **fcds** data, in particular after an
#' [age-adjusted rate][age_adjust] has been calculated.
#'
#' @examples
#' fcds_example_rates <-
#'   fcds::fcds_example %>%
#'   count_fcds(moffitt_catchment = TRUE) %>%
#'   complete_age_groups() %>%
#'   age_adjust() %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(rate = rate / 5) %>%
#'   dplyr::mutate(year_group = factor(year_group, unique(year_group)))
#'
#' if (all(fcds:::suggests_package(c("sf", "ggplot2")))) {
#'   fcds_map(fcds_example_rates) +
#'     ggplot2::facet_wrap(~year_group, ncol = 4) +
#'     ggplot2::theme(legend.position = c(0.93, 0.25))
#' }
#'
#' \dontrun{
#' fcds_map_leaflet(fcds_example_rates)
#' }
#' @param data A data frame containing FCDS data. At a minimum either
#'   `county_name` or `county_fips` are required to be included in the data. If
#'   the input data is not an `sf` data frame, an attempt will be made to join
#'   the data with the Florida county boundaries via [join_boundaries_fl()].
#' @param fill The unquoted column name to be used for the fill aesthetic.
#'   Defaults to `rate`.
#' @param geom_sf.color The color of the [ggplot2::geom_sf()] shape, typically
#'   the color of the county outline.
#' @param geom_sf.size The size of the [ggplot2::geom_sf()] shape, typically the
#'   size of the county outline.
#' @param coord_sf_hide If `TRUE` (the default), removes lat/long guides.
#' @param theme The theme to be applied to the plot, by default
#'   [ggplot2::theme_minimal()]. Set to `NULL` to use the global ggplot2 default
#'   theme.
#' @param scale_fill The fill scale to be applied to the [ggplot2::geom_sf()]
#'   shape. By default [ggplot2::scale_fill_viridis_c()]. Set equal to `NULL` to
#'   skip the addition of this scale.
#' @param ... Ignored in `fcds_map()`.
#'
#'   Additional arguments passed to [leaflet::leaflet()] in
#'   `fcds_map_leaflet()`.
#'
#'   Additional arguments passed to [glue::glue_data()] in
#'   `fcds_map_add_label()`.
#' @return A ggplot2 plot object
#' @export
fcds_map <- function(
  data,
  ...,
  fill = rate,
  geom_sf.color = "grey20",
  geom_sf.size = 0.25,
  coord_sf_hide = TRUE,
  theme = ggplot2::theme_minimal(),
  scale_fill = ggplot2::scale_fill_viridis_c()
) {
  requires_package(c("sf", "ggplot2"), "fcds_map()")
  fill <- rlang::enquo(fill)
  fill_name <- rlang::quo_name(fill)

  if (!isTRUE(inherits(data, "sf"))) {
    data <- join_boundaries_fl(data)
  }

  geometry <- NULL # quiet rcmd check

  g <-
    ggplot2::ggplot(data) +
    ggplot2::aes(fill = !!fill, geometry = geometry) +
    ggplot2::geom_sf(color = geom_sf.color, size = geom_sf.size)

  if (!is.null(scale_fill)) g <- g + scale_fill
  if (!is.null(theme)) g <- g + theme
  if (coord_sf_hide) g <- g + ggplot2::coord_sf(datum = NA)

  g
}

#' @describeIn fcds_map Creates an interactive map using [leaflet::leaflet()].
#'
#' @param palette Set the color palette of the shape fill. You may provide a
#'   vector of colors that will be used for the palette. The number of colors
#'   provided determines the number of bins for the values mapped to the fill,
#'   unless specified by `palette_bins`.
#' @param palette_bins The number of bins in the color `palette`.
#' @param group_name Character name of the column in `data` containing group
#'   layers.
#' @param group_labels Labels for the group levels in the column referenced by
#'   `group_name`. These labels should match the values in `data[[group_name]]`.
#' @param proxy_id If provided, a [leaflet::leafletProxy()] map is created
#'   instead. For use in Shiny apps.
#' @export
fcds_map_leaflet <- function(
  data,
  ...,
  palette = "Blues",
  palette_bins = if (length(palette == 1)) 5L else length(palette),
  group_name = "year_group",
  group_labels = rev(fcds_const(group_name)),
  proxy_id = NULL
) {
  requires_package(c("sf", "leaflet"), "fcds_map_leaflet()")
  has_group <- !is.null(group_name)
  if (has_group) {
    stopifnot(group_name %in% names(data))
    group <- rlang::sym(group_name)
  }

  has_label <- TRUE
  if (!"label" %in% names(data)) {
    expected_columns <- c("county_name", "n", "rate", "population")
    if (length(setdiff(expected_columns, names(data))) > 0) {
      warn("Use fcds_map_add_label() to add hover labels to counties.")
      has_label <- FALSE
    } else {
      data <- fcds_map_add_label(
        data,
        "<strong>{county_name}</strong>",
        "Cases/year: {format(n, big.mark = ',')}",
        "Rate/year: {format(rate, big.mark = ',')}",
        "Population: {format(population, big.mark = ',')}",
        .sep = "<br>"
      )
    }
  }

  if (!isTRUE(inherits(data, "sf"))) {
    data <- join_boundaries_fl(data)
  }

  pal <- leaflet::colorBin(palette, domain = data$rate, bins = palette_bins)

  leaflet_base <- if (is.null(proxy_id)) {
    leaflet::leaflet(data, ...)
  } else leaflet::leafletProxy(proxy_id, data = data, ...)

  map <-
    leaflet_base %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    leaflet::addPolygons(
      fillColor = ~ pal(rate),
      group = if (has_group) rlang::new_formula(NULL, rlang::sym(group_name)),
      opacity = 1,
      weight = 2,
      color = "#D4DADC",
      fillOpacity = 0.7,
      highlight = leaflet::highlightOptions(
        weight = 2,
        color = "#fff",
        bringToFront = TRUE,
        fillOpacity = 0.7
      ),
      label = if (has_label) ~ label,
      labelOptions = if (has_label) leaflet::labelOptions(
        textsize = "14px"
      )
    ) %>%
    leaflet::addLegend(
      pal = pal,
      values = ~rate,
      opacity = 0.7,
      title = "Age Adjusted Rate",
      position = "bottomleft"
    )

  if (has_group) {
    map <- map %>%
    leaflet::addLayersControl(
      baseGroups = group_labels,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  }

  map
}

