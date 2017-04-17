# Fertility Rate vs. Life Expectation

library(shiny)
library(ggplot2)
library(ggvis)
library(dplyr)

df <- read.csv('data_agg.csv', stringsAsFactors=FALSE)

ui <- fluidPage(
  titlePanel("Life Expectancy vs. Fertility Rate"),

  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "cont",
        label = "Region",
        choices = list(
          "All",
          "South Asia",
          "Europe and Central Asia",
          "Middle East and North Africa",
          "East Asia and Pacific",
          "Sub-Saharan Africa",
          "Latin America and Caribbean",
          "North America"),
        selected = "All"
      ),
      
      sliderInput(
        inputId = "pop",
        label = "Population",
        value = 0.5,
        min = 0.0, max = 1,
        ticks = FALSE),
      
      sliderInput(
        inputId = "year",
        label = "Year",
        value=1960, min=1960, max=2014, step=1,
        animate=animationOptions(interval=300), ticks=FALSE),
      
      width = 3
    ),
    mainPanel(
      ggvisOutput("plot"),
      width = 9
    ),
    position = c("right")
  )
)

server <- function(input, output) {
  
  country_info <- function(x) {
    df_row <- df[df$Country.Code == x$Country.Code & df$Year == input$year,]
    
    if (df_row$Population > 1000000) {
      population <- paste(round(df_row$Population/1000000, 3), "Million")
    } else {
      population <- df_row$Population
    }
    
    paste0(
      "Country: ", df_row$Country.Name, "<br>", "Population: ", population
    )
  }
  
  df_subset <- reactive({
    df_temp <- df[df$Year == input$year,]
    
    if (input$cont != 'All') {
      df_temp['Selected.Region'] <- (df_temp$Region == input$cont) * 0.9 + 0.1
    } else {
      df_temp['Selected.Region'] <- 1
    }
    
    df_temp
  })
  
  vis <- reactive({
    df_subset %>%
      ggvis(x = ~Life.Expectancy, y = ~Fertility.Rate,
            size := ~Population, fill = ~Region, key := ~Country.Code,
            stroke := "black", opacity := ~Selected.Region) %>%
      mutate(Population = (Population)^(0.7+0.3*input$pop) / 100000 + 30) %>%
      layer_points() %>%
      add_tooltip(country_info, "hover") %>%
      add_tooltip(country_info, "click") %>%
      scale_numeric("x", domain = c(20,85)) %>%
      scale_numeric("y", domain = c(0,10)) %>%
      set_options(height = 500, width = 950)
  })
  
  vis %>% bind_shiny("plot")
}

shinyApp(ui = ui, server = server)