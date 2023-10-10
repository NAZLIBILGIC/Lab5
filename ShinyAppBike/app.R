# Install and load the required packages
#install.packages("shiny")
library(shiny)
#install.packages("httr")
library(httr)
#install.packages("jsonlite")
library(jsonlite)




if (!("Lab5" %in% installed.packages()[, "Package"])) {
  devtools::install_github("NAZLIBILGIC/Lab5")
  library("Lab5")
}

source("RScripts/Lab5.R")

# Define UI for application
ui <- fluidPage(# Application title
  titlePanel("City Bike Station Status"),

  # Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Select City:",
                  choices = c("Lundahoj", "Malmobybike")),
      actionButton("get_info", "Get Information")
    ),
    # Show a plot of the generated distribution
    mainPanel(textOutput("city_info"))
  ))

server <- function(input, output) {
  observeEvent(input$get_info, {
    city_url <- switch(input$city,
                       "Lundahoj" = "http://api.citybik.es/v2/networks/lundahoj",
                       "Malmobybike" = "http://api.citybik.es/v2/networks/malmobybike")

    # Fetch data from the API
    response <- GET(city_url)
    api_data <- content(response, "text")

    # Save the bike data to a JSON file
    city_id <- basename(city_url)

    writeLines(api_data, paste0(city_id, "_data.json"))

    # Load the data from the JSON file
    city_data <- readLines(paste0(city_id, "_data.json"))

    # Get busy and least busy station
    results <- findBusiestStationsFromData(city_data, city_id)

    output$city_info <- renderText({
      paste(
        "City:",
        input$city,
        "\n",
        "Busiest Station Details:\n",
        "Name: ",
        results$busiest$name,
        "\n",
        "Address: ",
        results$busiest$extra$address,
        "\n",
        "Free Bikes: ",
        results$busiest$free_bikes,
        "\n",
        "Least Busy Station Details:\n",
        "Name: ",
        results$least_busy$name,
        "\n",
        "Address: ",
        results$least_busy$extra$address,
        "\n",
        "Free Bikes: ",
        results$least_busy$free_bikes,
        "\n"
      )
    })
  })
}



# Run the application
shinyApp(ui = ui, server = server)
