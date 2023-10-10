library(httr)
library(jsonlite)

#'
#'@importFrom httr GET
#'
#'@importFrom jsonlite fromJSON
#'
#'@param city_info The network information for a city.
#'
#'@param city_id The city ID's.
#'
#'@description
#'
#'@return A list containing the busiest and the least busy city bike station details.
#'
#'@export


city_api_urls <- c(
  "http://api.citybik.es/v2/networks/lundahoj",
  "http://api.citybik.es/v2/networks/malmobybike"
)

city_results <- list()

for (api_url in city_api_urls) {
  # Fetch data from the API
  response <- GET(api_url)
  api_data <- content(response, "text")

  # Save the bike data to a JSON file
  city_id <- basename(api_url)  # Extract the city ID from the URL
  writeLines(api_data, paste0(city_id, "_data.json"))

  findBusiestStationsFromData <- function(city_data, city_id) {
    city_info <- jsonlite::fromJSON(city_data)

    stations <- city_info$network$stations
    stations_sorted <- stations[order(-stations$free_bikes), ]
    busiest_station <- stations_sorted[1, ]
    least_busy_station <- stations_sorted[nrow(stations_sorted), ]

    # Check if free_bikes is a positive integer
    if (!is.integer(busiest_station$free_bikes) || busiest_station$free_bikes <= 0) {
      busiest_station$free_bikes <- 0
    }

    # Set the station's name as the address if the address is missing
    if (is.null(busiest_station$extra$address) || busiest_station$extra$address == "") {
      busiest_station$extra$address <- busiest_station$name
    }

    if (!is.integer(least_busy_station$free_bikes) || least_busy_station$free_bikes <= 0) {
      least_busy_station$free_bikes <- 0
    }

    if (is.null(least_busy_station$extra$address) || least_busy_station$extra$address == "") {
      least_busy_station$extra$address <- least_busy_station$name
    }

    return(list(busiest = busiest_station, least_busy = least_busy_station))
  }

  # Load the data from the JSON file
  city_data <- readLines(paste0(city_id, "_data.json"))

  # get busy and least busy station
  results <- findBusiestStationsFromData(city_data, city_id)

  city_results[[city_id]] <- results #put to list
}

for (city_id in names(city_results)) {
  results <- city_results[[city_id]]

  cat(paste("City:", city_id, "\n"))

  # Print Busiest Station Details
  cat("Busiest Station Details:\n")
  cat("Name: ", results$busiest$name, "\n")
  cat("Address: ", results$busiest$extra$address, "\n")
  cat("Free Bikes: ", results$busiest$free_bikes, "\n")

  # Print Least Busy Station Details
  cat("Least Busy Station Details:\n")
  cat("Name: ", results$least_busy$name, "\n")
  cat("Address: ", results$least_busy$extra$address, "\n")
  cat("Free Bikes: ", results$least_busy$free_bikes, "\n")
}




