# Install and load necessary packages
#
#install.packages("httr")
library(httr)
library(jsonlite)

bikeStationStatus <- function(network_ids) {
  api_url <- "http://api.citybik.es/v2/networks"
  response <- GET(api_url)
  http_status <- status_code(response)
  api_data <- content(response, "text")
  parsed_data <- fromJSON(api_data)
  network_info_list <- list()

  for (network_id in network_ids) {
    network_url <- paste0(api_url, "/", network_id)
    network_response <- GET(network_url)
    network_data <- content(network_response, "text")
    network_info <- fromJSON(network_data)
    if (!is.null(network_info)) {
      network_info_list[[network_id]] <- network_info
    }
  }

  bikeStationStatu <- function(city_info) {
    stations <- city_info$network$stations
    stations_sorted <- stations[order(-stations$free_bikes), ]
    busiest_station <- stations_sorted[1, ]
    least_busy_station <- stations_sorted[nrow(stations_sorted), ]
    return(list(busiest = busiest_station, least_busy = least_busy_station))
  }

  results_list <- list()

  for (network_id in network_ids) {
    city_info <- network_info_list[[network_id]]
    if (!is.null(city_info)) {
      results <- bikeStationStatu(city_info)
      results_list[[network_id]] <- results
    } else {
      results_list[[network_id]] <- NULL
    }
  }

  return(results_list)
}

# Specify the network IDs you want to retrieve detailed information for
network_ids <- c("malmobybike", "lundahoj")

# Call the function to get the results
results <- bikeStationStatus(network_ids)

# Print the results
for (network_id in network_ids) {
  if (!is.null(results[[network_id]])) {
    cat("City:", network_id, "\n")

    busiest_station <- results[[network_id]]$busiest
    least_busy_station <- results[[network_id]]$least_busy

    cat("Busiest Station Details:\n")
    cat("Name: ", busiest_station$name, "\n")
    cat("Address: ", busiest_station$extra$address, "\n")
    cat("Empty Slots: ", busiest_station$empty_slots, "\n")

    cat("Least Busy Station Details:\n")
    cat("Name: ", least_busy_station$name, "\n")
    cat("Address: ", least_busy_station$extra$address, "\n")
    cat("Empty Slots: ", least_busy_station$empty_slots, "\n")

    cat("\n")
  } else {
    cat("No detailed information available for", network_id, "\n")
  }
}
