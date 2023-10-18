#library(httr)
#library(jsonlite)

#'city_bikes documentation
#'@importFrom httr GET
#'
#'@importFrom jsonlite fromJSON
#'
#'
#'@description taking the information from city bikes API. With the information we get two
#'cities and find the least busy and busy stations. Show them to the user.
#'fetchCityBikeData function handling the APIs and fins the busy and least busy station.
#'
#'
#'@param api_urls API handle
#'
#'
#'
#'@return A list containing the busiest and the least busy city bike station details.
#'
#'@export
fetchCityBikeData <- function(api_urls=c("http://api.citybik.es/v2/networks/lundahoj",
                                         "http://api.citybik.es/v2/networks/malmobybike"
)) {
  # List of City Bike API URLs for two cities
  city_api_urls <- c(
      "http://api.citybik.es/v2/networks/lundahoj",
      "http://api.citybik.es/v2/networks/malmobybike"
    )
  city_results <- list()

  for (api_url in api_urls) {
    # Fetch data from the API
    response <- GET(api_url)
    api_data <- content(response, "text")

    # Save the bike data to a JSON file
    city_id <- basename(api_url)  # Extract the city ID from the URL
    writeLines(api_data, paste0(city_id, "_data.json"))

    city_data <- fromJSON(api_data)

    # Find the busiest and least busy stations
    stations <- city_data$network$stations
    stations_sorted <- stations[order(-stations$free_bikes),] #sort station based on freebike number
    busiest_station <- stations_sorted[1,]
    least_busy_station <- stations_sorted[nrow(stations_sorted),]

    # Check and modify station data
    checkStationData <- function(station) {
      if (!is.integer(station$free_bikes) || station$free_bikes <= 0) {
        station$free_bikes <- 0
      }
      if (is.null(station$extra$address) ||
          station$extra$address == "") {
        station$extra$address <- station$name # if address is empty put name as the address
      }
      return(station)
    }

    busiest_station <- checkStationData(busiest_station)
    least_busy_station <- checkStationData(least_busy_station)

    result <- list(busiest = busiest_station,
                   least_busy = least_busy_station)

    city_results[[city_id]] <- result
  }
  return(city_results)
}


# Fetch data and get results
#results <- fetchCityBikeData(city_api_urls)

# Print the results
#for (city_id in names(results)) {
#  results_city <- results[[city_id]]

#  cat(paste("City:", city_id, "\n"))

# Print Busiest Station Details
#  cat("Busiest Station Details:\n")
#  cat("Name: ", results_city$busiest$name, "\n")
#  cat("Address: ", results_city$busiest$extra$address, "\n")
#  cat("Free Bikes: ", results_city$busiest$free_bikes, "\n")

# Print Least Busy Station Details
#  cat("Least Busy Station Details:\n")
#  cat("Name: ", results_city$least_busy$name, "\n")
#  cat("Address: ", results_city$least_busy$extra$address, "\n")
#  cat("Free Bikes: ", results_city$least_busy$free_bikes, "\n")
#}
