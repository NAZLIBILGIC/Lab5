library(testthat)


test_that("Checking the busiest and least busy stations works correctly", {
  sample_city_data <- '{
    "network": {
      "stations": [
        {"name": "Station A", "free_bikes": 10, "empty_slots": 5, "address": "Address a"},
        {"name": "Station B", "free_bikes": 5, "empty_slots": 10, "address": "Address b"},
        {"name": "Station C", "free_bikes": 20, "empty_slots": 2, "address": "Address c"},
        {"name": "Station D", "free_bikes": 15, "empty_slots": 7, "address": "Address d"}
      ]
    }
  }'

  result <- findBusiestStationsFromData(sample_city_data)

  # Tests for the busiest station
  expect_equal(result$busiest$name, "Station C")
  expect_equal(result$busiest$free_bikes, 20)
  expect_equal(result$busiest$empty_slots, 2)
  expect_equal(result$busiest$address, "Address c")
  expect_true(is.numeric(result$busiest$empty_slots) && result$busiest$empty_slots >= 0) #input type check for empty slots
  expect_true(is.numeric(result$least_busy$free_bikes) && result$busiest$empty_slots >= 0)


  # Tests for the least busy station
  expect_equal(result$least_busy$name, "Station B")
  expect_equal(result$least_busy$free_bikes, 5)
  expect_equal(result$least_busy$empty_slots, 10)
  expect_equal(result$least_busy$address, "Address b")

})
