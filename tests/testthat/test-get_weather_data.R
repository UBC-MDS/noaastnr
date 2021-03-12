station_number <-  "911803-99999"
year <- 2015
weather_df <- get_weather_data(station_number, year)

test_that("Weather data should be returned as a tibble.", {
expect_equal(class(weather_df)[1],"tbl_df")
})

test_that("Test data should have 6 rows (observations).", {
  expect_equal(nrow(weather_df),6)
})

test_that("Dataframe should have 6 columns.", {
  expect_equal(ncol(weather_df),6)
})

test_that("Datetime columns should be represented as date time objects.", {
  expect_equal(class(weather_df$datetime)[1],"POSIXct")
  expect_equal(class(weather_df$datetime)[2],"POSIXt")
})

test_that("Datetime columns should be represented as date time objects.", {
  expect_equal(class(weather_df$air_temp),"numeric")
  expect_equal(class(weather_df$atm_press),"numeric")
  expect_equal(class(weather_df$wind_spd),"numeric")
  expect_equal(class(weather_df$wind_dir),"numeric")
})

test_that("The only station number in the data table should be station_number", {
  expect_equal(dplyr::distinct(weather_df,stn)$stn,station_number)
})
