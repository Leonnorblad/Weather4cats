library(testthat)

# Testing the input ####
test_that("Not a valid date", {
  expect_error(get_weather_forecast(city="Link\u00f6ping", date="2020-09-26"))  # To long ago
  expect_error(get_weather_forecast(city="Link\u00f6ping", date="26-09-2022"))  # Wrong format
  expect_error(get_weather_forecast(city="Link\u00f6ping", date="Hello world")) # Wrong format
  expect_error(get_weather_forecast(city="Link\u00f6ping", date=Sys.Date()+8))  # To long ahead
})

test_that("Not a valid area", {
  expect_error(get_weather_forecast(city="Jonkoping", date=Sys.Date())) # Not a valid city
  expect_error(get_weather_forecast(city="No city", date=Sys.Date()))   # Wrong format
  expect_error(get_weather_forecast(city="link\u00f6ping", date=Sys.Date())) # No uppercase for the first letter
})

test_that("Missing arguments", {
  expect_error(get_weather_forecast())                 # Missing city and date
  expect_error(get_weather_forecast(city="Link\u00f6ping")) # Missing date
  expect_error(get_weather_forecast(date=Sys.date()))  # Missing city 
})


# Testing the output ####
test_that("Dimentions of output is correct", {
  expect_equal(dim(get_weather_forecast(city="Link\u00f6ping", date=Sys.Date())), c(24, 8)) # Dimensions
})

test_that("Name of colums in output is correct", {
  expect_equal(colnames(get_weather_forecast(city="Link\u00f6ping", date=Sys.Date())), # Correct colnames
               c("Time", "Temperature", "Humidity", "Precipitation",
                 "Cloudcover", "Wind_speed", "Wind_direction", "City"))
})
