#' Get stations information
#'
#' Downloads and cleans the data of all stations available at
#' <ftp://ftp.ncei.noaa.gov/pub/data/noaa>.
#'
#' @param country character, optional
#'
#' @return data.frame
#' @export
#'
#' @examples
#' get_stations_info(country = "US")
get_stations_info <- function(country = "all") {

}

#' Get weather data
#'
#' Loads and cleans weather data for a given NOAA station ID and year.
#' Returns a dataframe containing a time series of air temperature,
#' atmospheric pressure, wind speed, and wind direction the NOAA FTP server at
#' <ftp.ncei.noaa.gov/pub/data/noaa/>.
#'
#' @param station_number character
#' @param year integer
#'
#' @return data.frame
#' @export
#'
#' @details
#' `station_number` is a combination of the USAF station ID and the NCDC
#' WBAN number in the form '(USAF ID)-(WBAN ID)'.  If a WBAN ID does not
#' exist, a value of '99999' should be used in its place.
#' * Example with WBAN ID - '911650-22536'
#' * Example without WBAN ID - '010015-99999'
#' Station numbers can be found in the dataframe returned by
#' `get_stations_info()` or through the NOAA's graphical tool at
#' <https://gis.ncdc.noaa.gov/maps/ncei/cdo/hourly>
#'
#' @examples
#' get_weather_data('911803-99999', 2015)
get_weather_data <- function(station_number, year) {

  # Exception handling
  testthat::test_that("Year must be entered as a number", {
    testthat::expect_equal(class(year), "numeric")
  })
  testthat::test_that("Station number must be entered as a string", {
    testthat::expect_equal(class(station_number), "character")
  })
  testthat::test_that(
    "Station number must be entered in form '911650-22536'.  See documentation for additional details.",
    {
      testthat::expect_equal(stringr::str_detect(station_number, "^[0-9]{6}[-][0-9]{5}$"),
                   TRUE)
    }
  )

  # Build file and path names
  filename <- paste0(station_number, "-" , toString(year), ".gz")
  full_path <-
    paste0("ftp.ncei.noaa.gov/pub/data/noaa/", toString(year), "/")
  file_url <- paste0(full_path, filename)

  # Download compressed weather station data
  if (RCurl::url.exists(file_url)) {
    compressed_data <- RCurl::getBinaryURL(file_url)
  }

  # Decompress weather data
  data <-
    unlist(strsplit(
      memDecompress(compressed_data, type = 'gzip', asChar = TRUE),
      '\n'
    ))

  # Create data frame for weather observations
  stn_year_df <-
    tibble::tibble(
      stn = character(),
      datetime = as.POSIXct(NA),
      air_temp = numeric(),
      atm_press = numeric(),
      wind_spd = numeric(),
      wind_dir = numeric()
    )

  # Populate weather observations data frame from raw data
  for (i in seq_along(data)) {
    stn_year_df <- tibble::add_row(
      stn_year_df,
      stn = station_number,
      datetime = lubridate::ymd_hm(as.numeric(substr(data[i], 16, 27))),
      air_temp = as.numeric(substr(data[i], 88, 92)),
      atm_press = as.numeric(substr(data[i], 100, 104)) / 10,
      wind_spd = as.numeric(substr(data[i], 66, 69)) / 10,
      wind_dir = as.numeric(substr(data[i], 61, 63))
    )
  }

  stn_year_df[stn_year_df == 999] <- NA
  stn_year_df[stn_year_df == 999.9] <- NA
  stn_year_df[stn_year_df == 9999.9] <- NA

  stn_year_df
}

#' Plot weather data
#'
#' Visualizes the weather station observations including air temperature,
#' atmospheric pressure, wind speed, and wind direction changing over time.
#'
#' @param obs_df data.frame
#' @param col_name factor
#' @param time_basis factor
#'
#' @return 'ggplot2'
#' @export
#'
#' @examples
#' plot_weather_data(obs_df, col_name = air_temp, time_basis = monthly)
plot_weather_data <- function(obs_df, col_name, time_basis) {

}
