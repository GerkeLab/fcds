library(shiny)
library(tidyverse)
library(sf)

library(USAboundaries)
florida_counties <- us_counties(states = "Florida")

fl_dec_2000 <- readRDS(here::here("data", "fl_dec_2000.rds")) %>%
  mutate(
    age_group = fct_reorder(age_group, age_low),
    age_group = fct_relevel(age_group, "All")
  ) %>%
  left_join(florida_counties %>% select(geoid, geometry), by = "geoid")

fl_vals <- function(column, sort = TRUE) {
  column <- rlang::enquo(column)
  x <- fl_dec_2000 %>% pull(!!column)
  if (is.factor(x)) {
    x <- levels(x)
  } else {
    x <- unique(x)
    x <- if (sort) base::sort(x) else x
  }
  c("All" = "", x[x != "All"])
}

uniq_vals <- function(x, column) {
  column <- rlang::enquo(column)
  x %>% pull(!!column) %>% unique()
}

ui <- fluidPage(
    titlePanel("US 2000 Census Data"),
    mainPanel(
        inputPanel(
            selectizeInput("sex", "Sex", choices = fl_vals(sex), multiple = TRUE),
            selectizeInput("race", "Race", choices = fl_vals(race), multiple = TRUE),
            selectizeInput("hispanic", "Hispanic", choices = fl_vals(hispanic), multiple = TRUE),
            selectizeInput("age_group", "Age", choices = fl_vals(age_group, sort = FALSE), multiple = TRUE)
        ),
        plotOutput("map", height = "800px", width = "100%")
    )
)

server <- function(input, output, session) {

    fl_data <- reactive({
      vals <- c("sex", "race", "hispanic", "age_group") %>%
        set_names() %>%
        map(~ input[[.]])
      empty_or_missing <- map_lgl(vals, is_null)
      vals[empty_or_missing] <- "All"
      fl_dec_2000 %>%
        filter(
          sex %in% vals$sex,
          race %in% vals$race,
          hispanic %in% vals$hispanic,
          age_group %in% vals$age_group
        )
    })

    # Choice of race limits hispanic choices
    observe({
      s_hispanic <- isolate(input$hispanic)
      c_hispanic <- c("All" = "", "Yes", "No")
      if (is.null(input$race) || input$race[1] == "" || input$race[1] == "White") {
        updateSelectInput(session, "hispanic", choices = c_hispanic, selected = s_hispanic)
      } else {
        updateSelectInput(session, "hispanic", choices = c("All" = ""), selected = "")
      }
    })

    # Choices of hispanic limits race choices
    observe({
      s_race <- isolate(input$race)
      if (is.null(input$hispanic) || input$hispanic[1] == "") {
        c_race <- fl_vals(race)
        updateSelectInput(session, "race", choices = c_race, selected = s_race)
      } else {
        c_race <- c("All" = "", "White")
        s_race <- intersect(c_race, s_race)
        if (!length(s_race)) s_race <- ""
        updateSelectInput(session, "race", choices = c_race, selected = s_race)
      }
    })

    output$map <- renderPlot({
        req(fl_data())
        g <- ggplot(fl_data()) +
            aes(fill = value, geometry = geometry) +
            geom_sf(color = "grey20", size = 0.25) +
            scale_fill_viridis_c() +
            coord_sf(datum = NA) +
            labs(fill = NULL) +
            theme_minimal()

        mult <- map_lgl(
          set_names(c("sex", "race", "hispanic", "age_group")),
          ~ length(input[[.]]) > 1
        )
        if (any(mult)) {
          mult_vars <- map(names(mult[mult]), rlang::sym)
          g <- g + facet_wrap(mult_vars)
        }

        g
    })
}

shinyApp(ui = ui, server = server)
