# Install and load the required packages
#install.packages("shiny")
library(shiny)
#install.packages("httr")
library(httr)
#install.packages("jsonlite")
library(jsonlite)



# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("City Bike Information"),

  # Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
      selectInput("city_id", "Select City:",
                  choices = c("malmobybike", "lundahoj"),
                  selected = "malmobybike"
      ),
      actionButton("get_info_button", "Get Information")),
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("busiest_station_info"),
      textOutput("least_busy_station_info")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  network_info_list <- reactive({
    # Fetch and parse data from the city bike network API
    api_url <- "http://api.citybik.es/v2/networks"
    response <- httr::GET(api_url)
    api_data <- httr::content(response, "text")
    parsed_data <- jsonlite::fromJSON(api_data)

    # List of network IDs to retrieve detailed information for
    network_ids <- c("malmobybike", "lundahoj")
    network_info_list <- list()

    for (network_id in network_ids) {
      network_url <- paste0(api_url, "/", network_id)
      network_response <- httr::GET(network_url)
      network_data <- httr::content(network_response, "text")
      network_info <- jsonlite::fromJSON(network_data)
      if (!is.null(network_info)) {
        network_info_list[[network_id]] <- network_info
      }
    }
    return(network_info_list)
  })
  observeEvent(input$get_info_button, {
    city_id <- input$city_id
    city_info <- network_info_list()[[city_id]]

    if (!is.null(city_info)) {
      results <- find_busiest_and_least_busy_stations(city_info)

      busiest_station <- results$busiest
      least_busy_station <- results$least_busy

      busiest_text <- paste(
        "Busiest Station Details:",
        "Name:", busiest_station$name,
        "Address:", busiest_station$extra$address,
        "Empty Slots:", busiest_station$empty_slots
      )

      least_busy_text <- paste(
        "Least Busy Station Details:",
        "Name:", least_busy_station$name,
        "Address:", least_busy_station$extra$address,
        "Empty Slots:", least_busy_station$empty_slots
      )

      output$busiest_station_info <- renderText(busiest_text)
      output$least_busy_station_info <- renderText(least_busy_text)
    } else {
      output$busiest_station_info <- renderText("No detailed information available for this city.")
      output$least_busy_station_info <- renderText(NULL)
    }
  })

}
# Run the application
shinyApp(ui = ui, server = server)
