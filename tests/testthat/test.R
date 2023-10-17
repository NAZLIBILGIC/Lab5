library(testthat)

test_that("Checking the busiest and least busy stations works correctly", {

  # Mocking the GET and content functions
  mock_response <- structure(list(content = function(x, ...) sample_city_data), class = "response")

  with_mock(
    `httr::GET` = function(url, ...) mock_response,
    `httr::content` = function(response, ...) response$content(response),

    {
      result <- fetchCityBikeData(api_urls = c("mock_url"))

      # Adjusting the tests based on the functionality of fetchCityBikeData
      city_result <- result$mock_url

      # Tests for the busiest station
      expect_equal(city_result$busiest$name, "Station C")
      expect_equal(city_result$busiest$free_bikes, 20)
      expect_equal(city_result$busiest$extra$address, "Address c")

      # Tests for the least busy station
      expect_equal(city_result$least_busy$name, "Station B")
      expect_equal(city_result$least_busy$free_bikes, 5)
      expect_equal(city_result$least_busy$extra$address, "Address b")

      # Additional test conditions
      expect_true(is.numeric(city_result$busiest$free_bikes) && city_result$busiest$free_bikes >= 0)
      expect_true(is.numeric(city_result$least_busy$free_bikes) && city_result$least_busy$free_bikes >= 0)
    }
  )
})
