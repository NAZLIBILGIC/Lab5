library(testthat)
library(httr)
library(jsonlite)


test_that("Test fetching city bike data", {

  # Mock API response
  api_response <- '
  {
    "network": {
      "stations": [
        {"name": "Station A", "free_bikes": 3, "extra": {"address": "Address A"}},
        {"name": "Station B", "free_bikes": 5, "extra": {"address": "Address B"}},
        {"name": "Station C", "free_bikes": 1, "extra": {"address": ""}}
      ]
    }
  }'

  #getting the request and content function
  with_mock(
    GET = function(url, ...) {
      structure(list(url = url, status_code = 200, headers = list('Content-Type' = 'application/json')), class = "response")
    },
    content = function(...) {
      api_response
    },
    {
      results <- fetchCityBikeData(c("http://api.citybik.es/v2/networks/mockcity"))
      expect_equal(length(results), 1)
      expect_equal(names(results), "mockcity")

      # Test the busiest and least busy stations
      expect_equal(results$mockcity$busiest$name, "Station B")
      expect_equal(results$mockcity$busiest$free_bikes, 5)

      expect_equal(results$mockcity$least_busy$name, "Station C")
      expect_equal(results$mockcity$least_busy$free_bikes, 1)
      expect_equal(results$mockcity$least_busy$extra$address, "Station C") # Address should be replaced with name



    }
  )
  test_that("Test address replacement for multiple stations", {
    api_response_missing_addresses <- '
  {
    "network": {
      "stations": [
        {"name": "Station A", "free_bikes": 3},
        {"name": "Station B", "free_bikes": 5},
        {"name": "Station C", "free_bikes": 1}
      ]
    }
  }'

    with_mock(
      GET = function(url, ...) {
        structure(list(url = url, status_code = 200, headers = list('Content-Type' = 'application/json')), class = "response")
      },
      content = function(...) {
        api_response_missing_addresses
      },
      {
        results <- fetchCityBikeData(c("http://api.citybik.es/v2/networks/mockcity"))
        expect_equal(results$mockcity$busiest$extra$address, "Station B")
        expect_equal(results$mockcity$least_busy$extra$address, "Station C")
      }
    )
  })
  test_that("Test with default URLs", {
    results <- fetchCityBikeData()
    expect_true(!is.null(results))
  })
})



