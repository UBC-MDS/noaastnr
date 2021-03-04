#' Get stations' information
#'
#' Downloads and cleans the data of all stations available at
#' 'ftp://ftp.ncei.noaa.gov/pub/data/noaa/'.
#'
#' @param country factor, optional
#' @param path factor, optional
#'
#' @return data.frame
#' @export
#'
#' @examples
#' get_stations_info(country = "US", path = "home/project/")
get_stations_info <- function(country = "US", path = NULL) {

}

#' Get weather data
#'
#' Loads and cleans weather data for a given NOAA station ID and year.
#' Returns a dataframe containing a time series of air temperature,
#' atmospheric pressure, wind speed, and wind direction. Also saves a copy of
#' the raw data file downloaded from the NOAA FTP server at
#' 'ftp.ncei.noaa.gov/pub/data/noaa/'.
#'
#' @param station_number factor
#' @param year integer
#' @param path factor, optional
#'
#' @return data.frame
#' @export
#'
#' @examples
#' get_weather_data('911650-22536', 2020)
get_weather_data <- function(station_number, year, path = NULL) {

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
