source('Lab5.R')

#Function check
test_that("bikeStationStatu functionality", {
  # Create a sample city_info object
  sample_city_info <- list(
    network = list(
      stations = data.frame(
        name = c("Station A", "Station B"),
        free_bikes = c(10, 5),
        extra = list(address = c("Address A", "Address B")),
        empty_slots = c(5, 10)
      )
    )
  )

  results <- bikeStationStatu(sample_city_info)

  # Test that the function returns a list
  expect_is(results, "list")

  # Busy check
  expect_equal(results$busiest$name, "Station A")
  expect_equal(results$busiest$empty_slots, 5)
  expect_equal(results$busiest$free_bikes, 10)

  #least busy check
  expect_equal(results$least_busy$name, "Station B")
  expect_equal(results$least_busy$empty_slots, 10)
  expect_equal(results$least_busy$free_bikes, 5)

})
