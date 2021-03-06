#' Get stations information
#'
#' Downloads and cleans the data of all stations available at
#' 'ftp://ftp.ncei.noaa.gov/pub/data/noaa/'.
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
#' 'ftp.ncei.noaa.gov/pub/data/noaa/'.
#'
#' @param station_number character
#' @param year integer
#'
#' @return data.frame
#' @export
#'
#' @details
#' `station_number` is a combination of the USAF station ID and the NCDC
#' WBAN number in the form '<USAF ID>-<WBAN ID>'.  If a WBAN ID does not
#' exist, a value of '99999' should be used in its place.
#' * Example with WBAN ID - '911650-22536'
#' * Example without WBAN ID - '010015-99999'
#' Station numbers can be found in the dataframe returned by
#' `get_stations_info()` or through the NOAA's graphical tool at
#'  https://gis.ncdc.noaa.gov/maps/ncei/cdo/hourly
#'
#' @examples
#' get_weather_data('911650-22536', 2020)
get_weather_data <- function(station_number, year) {

}

#' Plot weather data
#'
#' Visualizes the weather station observations including air temperature,
#' atmospheric pressure, wind speed, and wind direction changing over time.
#'
#' @param observations_df data.frame
#' @param y_axis factor
#' @param time_basis factor
#'
#' @return 'ggplot2'
#' @export
#'
#' @examples
#' plot_weather_data(observations_df, y_axis = airtemp, time_basis = monthly)
plot_weather_data <- function(observations_df, y_axis, time_basis) {

}
