# Install and load necessary packages
# install.packages("httr")
library(httr)
library(jsonlite)


api_url <- "http://api.citybik.es/v2/networks"

response <- GET(api_url)

http_status <- status_code(response)
#print(http_status)
api_data <- content(response, "text")

# Parse the JSON data
parsed_data <- fromJSON(api_data)

# List of network IDs to retrieve detailed information for
network_ids <- c("malmobybike", "lundahoj")
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

for (network_id in network_ids) {
  if (network_id %in% names(network_info_list)) {
    print(paste("Detailed information for", network_id, ":"))
    print(network_info_list[[network_id]])
  } else {
    print(paste("No detailed information available for", network_id))
  }
}

find_busiest_and_least_busy_stations <- function(city_info) {
  stations <- city_info$network$stations

  # Sort stations by bike availability
  stations_sorted <- stations[order(-stations$free_bikes), ]

  # busiest and least busy stations
  busiest_station <- stations_sorted[1, ]
  least_busy_station <- stations_sorted[nrow(stations_sorted), ]

  # Return the results
  return(list(busiest = busiest_station, least_busy = least_busy_station))
}

# Iterate through the cities and determine busiest and least busy stations
for (city_id in c("malmobybike", "styr-staell-goeteborg", "lundahoj")) {
  city_info <- network_info_list[[city_id]]
  if (!is.null(city_info)) {
    results <- find_busiest_and_least_busy_stations(city_info)
    cat("City:", city_id, "\n")

    busiest_station <- results$busiest
    least_busy_station <- results$least_busy

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
    cat("No detailed information available for", city_id, "\n")
  }
}




