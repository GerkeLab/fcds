
join_boundaries_fl <- function(data) {
  requires_package(c("sf", "USAboundaries"), "join_boundaries_fl()")
  # library(sf)

  florida_counties <- USAboundaries::us_counties(states = "Florida") %>%
    select(county_fips = countyfp, "geometry", "geoid")

  if (!"county_fips" %in% names(data)) {
    county_fips <- fcds::county_fips_fl
    data <-
      data %>%
      quiet_left_join(county_fips, by = "county_name") %>%
      mutate(county_fips = sprintf("%03d", as.integer(.data$county_fips)))
  }

  sf::st_sf(quiet_inner_join(florida_counties, data, by = "county_fips"))
}

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

#' @export
fcds_map_leaflet <- function(data, palette = "moffitt_blue", ..., proxy_id = NULL) {
  requires_package(c("sf", "leaflet"), "fcds_map_leaflet()")
  if (is.null(data$label)) {
    data <- fcds_map_add_label(
      data,
      "<strong>{county_name}</strong>",
      "Cases/year: {format(n, big.mark = ',')}",
      "Rate/year: {format(rate, big.mark = ',')}",
      "Population: {format(population, big.mark = ',')}",
      .sep = "<br>"
    )
  }

  if (!isTRUE(inherits(data, "sf"))) {
    data <- join_boundaries_fl(data)
  }

  moffitt_blue <- function(n = 3) {
    colorspace::sequential_hcl(n, 250, 64, c(37, 90), power = 1, rev = TRUE)
  }
  moffitt_red <- function(n = 3) {
    colorspace::sequential_hcl(n, 4.350477, 142.1399, c(50.48059, 90), power = 1, rev = TRUE)
  }

  palette <- switch(
    palette,
    moffitt_blue = moffitt_blue(5),
    moffitt_red = moffitt_red(5),
    palette
  )

  pal <- leaflet::colorBin(palette, domain = data$rate, bins = 5)

  leaflet_base <- if (is.null(proxy_id)) {
    leaflet::leaflet(data, ...)
  } else leaflet::leafletProxy(proxy_id, data = data, ...)

  leaflet_base %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
    # leaflet::addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
    leaflet::addPolygons(
      fillColor = ~ pal(rate),
      group = ~ year_group,
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
      label = ~ label,
      labelOptions = leaflet::labelOptions(
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
}


#' @export
fcds_map <- function(data) {
  requires_package(c("sf", "ggplot2"), "fcds_map()")

  if (!isTRUE(inherits(data, "sf"))) {
    data <- join_boundaries_fl(data)
  }

  ggplot2::ggplot(data) +
    ggplot2::aes(fill = rate, geometry = geometry) +
    ggplot2::geom_sf(color = "grey20", size = 0.25) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::theme_minimal()
}
