library(testthat)

# Load your functions (if not already loaded)
# source("your_script.R")

# Test case for bikeStationStatus
test_that("bikeStationStatus retrieves data for valid network IDs", {
  network_ids <- c("malmobybike", "lundahoj")
  results <- bikeStationStatus(network_ids)
  expect_is(results, "list")  # Expect the result to be a list
  expect_true(length(results) == length(network_ids))  # Expect the result list to have the same length as input
})

# Test case for bikeStationStatu
test_that("bikeStationStatu returns the busiest and least busy stations", {
  # Create a sample city_info with stations data
  city_info <- list(
    network = list(
      stations = data.frame(
        name = c("Station A", "Station B", "Station C"),
        free_bikes = c(5, 10, 3),
        extra = list(address = c("Address A", "Address B", "Address C"))
      )
    )
  )

  results <- bikeStationStatu(city_info)
  expect_is(results, "list")  # Expect the result to be a list
  expect_named(results, c("busiest", "least_busy"))  # Expect the result to have named components
  expect_equal(results$busiest$name, "Station B")  # Check the name of the busiest station
  expect_equal(results$least_busy$name, "Station C")  # Check the name of the least busy station
})
