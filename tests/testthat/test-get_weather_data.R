station_number <-  "911803-99999"
year <- 2015
weather_df <- noaastnr.get_weather_data(station_number, year)

test_that("Weather data should be returned as a tibble.", {
expect_equal(class(weather_df)[1],"tbl_df")
})

test_that("Test data should have 6 rows (observations).", {
  expect_equal(nrow(weather_df),6)
})

test_that("Dataframe should have 6 columns.", {
  expect_equal(ncol(weather_df),6)
})

# To be added


# def test_column_datatypes():
#   assert (
#     weather_df.datetime.dtype == "<M8[ns]"
#   ), "Data type of datetime column is incorrect (should be'<M8[ns]')."
# assert (
#   weather_df.air_temp.dtype == "float64"
# ), "Data type of air_temp column is incorrect (should be 'float64')."
# assert (
#   weather_df.atm_press.dtype == "float64"
# ), "Data type of atm_press column is incorrect (should be 'float64')."
# assert (
#   weather_df.wind_spd.dtype == "float64"
# ), "Data type of wind_spd column is incorrect (should be 'float64')."
# assert (
#   weather_df.wind_dir.dtype == "float64"
# ), "Data type of wind_dir column is incorrect (should be 'float64')."
#
#
# def test_station_number_coding():
#   assert (
#     weather_df.stn.unique().shape[0] == 1
#   ), "There should only be one station number in the data table"
# assert (
#   weather_df.stn.unique()[0] == station_number
# ), "Station number should match entries values in stn column"
#
#
# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
