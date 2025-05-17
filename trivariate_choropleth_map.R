install.packages("https://cran.r-project.org/src/contrib/Archive/tricolore/tricolore_1.2.2.tar.gz", repos = NULL, type = "source")

install.packages('tricol')
library(tricolore)
library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(scales)
library(readr)

# Load your dataset
state_data <- read.csv("state_level_beauty_wellness_econ_means.csv")

# Add region info for mapping
state_data$region <- tolower(state.name[match(state_data$state, state.abb)])
us_states_map <- map_data("state")

# UI
ui <- fluidPage(
  titlePanel("Interactive Trivariate Choropleth: Beauty, Wellness, Econ"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("beauty", "Select Beauty Variable:",
                  choices = grep("^beauty", names(state_data), value = TRUE)),
      selectInput("wellness", "Select Wellness Variable:",
                  choices = grep("^wellness", names(state_data), value = TRUE)),
      selectInput("econ", "Select Economic Variable:",
                  choices = c("r_gdp", "r_persinc", "r_pce", "disp_persinc", "rpc_persinc", "rpc_pce"))
    ),
    mainPanel(
      plotOutput("choropleth", height = "600px"),
      plotOutput("legendPlot", height = "400px")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive tricolore output
  reactive_data <- reactive({
    df <- state_data %>%
      mutate(
        total = !!sym(input$beauty) + !!sym(input$wellness) + !!sym(input$econ),
        beauty_norm = !!sym(input$beauty) / total,
        wellness_norm = !!sym(input$wellness) / total,
        econ_norm = !!sym(input$econ) / total
      )
    
    tricol <- Tricolore(df,
                        p1 = "econ_norm",
                        p2 = "beauty_norm",
                        p3 = "wellness_norm",
                        hue = 1,
                        chroma = 1,
                        lightness = 1,
                        contrast = 1,
                        spread = 2,
                        center = NA)
    
    df$color <- tricol$rgb
    df$legend_plot <- tricol$ternary
    df
  })
  
  output$choropleth <- renderPlot({
    plot_data <- left_join(us_states_map, reactive_data(), by = "region")
    
    ggplot(plot_data, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = color), color = "white") +
      scale_fill_identity() +
      coord_fixed(1.3) +
      theme_void() +
      labs(
        title = "Trivariate Choropleth",
        subtitle = paste(input$beauty, "+", input$wellness, "+", input$econ)
      )
  })
  
  output$legendPlot <- renderPlot({
    df <- reactive_data()
    print(df$legend_plot[1])  # extract ternary ggplot object
  })
}





# Run the app
shinyApp(ui = ui, server = server)

