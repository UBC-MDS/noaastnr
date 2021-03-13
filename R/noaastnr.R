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
  if(!is.numeric(year)) {
    stop("Year must be entered as a number.")
  }
  if(!is.character(station_number)) {
    stop("Station number must be entered as a string.")
  }
  if(!stringr::str_detect(station_number, "^[0-9]{6}[-][0-9]{5}$")) {
    stop("Station number must be entered in form '911650-22536'.  See documentation for additional details.")
  }

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
#' weather_df <- get_weather_data("911650-22536", 2020)
#' plot_weather_data(obs_df = weather_df, col_name = "air_temp", time_basis = "monthly")
plot_weather_data <- function(obs_df, col_name, time_basis) {
  # Define global variables
  datetime <-
    air_temp <- atm_press <- wind_spd <- wind_dir <- month <- NULL
  # Test input types
  if (!is.data.frame(obs_df)) {
    stop("Weather data should be a dataFrame.")
  }
  if (!is.character(col_name)) {
    stop("Variable name must be entered as a factor.")
  }
  if (!is.character(time_basis)) {
    stop("Time basis must be entered as a factor.")
  }
  # Test edge cases
  testthat::test_that("Variable can only be one of air_temp, atm_press, wind_spd or wind_dir",
                      {
                        testthat::expect_true(col_name %in% c("air_temp", "atm_press", "wind_spd", "wind_dir"))
                      })
  testthat::test_that("Time basis can only be monthly or daily", {
    testthat::expect_true(time_basis %in% c("monthly", "daily"))
  })

  df <- obs_df
  df <- tidyr::drop_na(df)
  if (nrow(df) < 3) {
    stop("Dataset is not sufficient to visualize.")
  }
  year <-
    lubridate::year(lubridate::floor_date(df$datetime, "year")[1])

  if (time_basis == "monthly") {
    df <-
      dplyr::group_by(df, month = lubridate::floor_date(datetime, "month"))
    df <-
      dplyr::summarise(
        df,
        air_temp = mean(air_temp),
        atm_press = mean(atm_press),
        wind_spd = mean(wind_spd),
        wind_dir = mean(wind_dir)
      )

    if (nrow(df) < 3) {
      stop("Dataset is not sufficient to visualize.")
    }

    if (col_name == "air_temp") {
      title_text <- paste("Air Temperature for ", year)
      line <-
        ggplot2::ggplot(df, ggplot2::aes(x = as.Date(month), y = air_temp)) +
        ggplot2::geom_line(color = "orange") +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::ggtitle(title_text) +
        ggplot2::xlab("Month") +
        ggplot2::ylab("Air Temperature")
    } else if (col_name == "atm_press") {
      title_text <- paste("Atmospheric Pressure for ", year)
      line <-
        ggplot2::ggplot(df, ggplot2::aes(x = as.Date(month), y = atm_press)) +
        ggplot2::geom_line(color = "orange") +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::ggtitle(title_text) +
        ggplot2::xlab("Month") +
        ggplot2::ylab("Atmospheric Pressure")
    } else if (col_name == "wind_spd") {
      title_text <- paste("Wind Speed for ", year)
      line <-
        ggplot2::ggplot(df, ggplot2::aes(x = as.Date(month), y = wind_spd)) +
        ggplot2::geom_line(color = "orange") +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::ggtitle(title_text) +
        ggplot2::xlab("Month") +
        ggplot2::ylab("Wind Speed")
    } else {
      title_text <- paste("Wind Direction for ", year)
      line <-
        ggplot2::ggplot(df, ggplot2::aes(x = as.Date(month), y = wind_dir)) +
        ggplot2::geom_line(color = "orange") +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::ggtitle(title_text) +
        ggplot2::xlab("Month") +
        ggplot2::ylab("Wind Direction")
    }

  } else {
    df <-
      dplyr::group_by(df, month = lubridate::floor_date(datetime, "day"))
    df <-
      dplyr::summarise(
        df,
        air_temp = mean(air_temp),
        atm_press = mean(atm_press),
        wind_spd = mean(wind_spd),
        wind_dir = mean(wind_dir)
      )

    if (nrow(df) < 3) {
      stop("Dataset is not sufficient to visualize.")
    }

    if (col_name == "air_temp") {
      title_text <- paste("Air Temperature for ", year)
      line <-
        ggplot2::ggplot(df, ggplot2::aes(x = as.Date(date), y = air_temp)) +
        ggplot2::geom_line(color = "orange") +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::ggtitle(title_text) +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Air Temperature")
    } else if (col_name == "atm_press") {
      title_text <- paste("Atmospheric Pressure for ", year)
      line <-
        ggplot2::ggplot(df, ggplot2::aes(x = as.Date(date), y = atm_press)) +
        ggplot2::geom_line(color = "orange") +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::ggtitle(title_text) +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Atmospheric Pressure")
    } else if (col_name == "wind_spd") {
      title_text <- paste("Wind Speed for ", year)
      line <-
        ggplot2::ggplot(df, ggplot2::aes(x = as.Date(date), y = wind_spd)) +
        ggplot2::geom_line(color = "orange") +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::ggtitle(title_text) +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Wind Speed")
    } else {
      title_text <- paste("Wind Direction for ", year)
      line <-
        ggplot2::ggplot(df, ggplot2::aes(x = as.Date(date), y = wind_dir)) +
        ggplot2::geom_line(color = "orange") +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::ggtitle(title_text) +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Wind Direction")
    }
  }

  chart <- line +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 13),
      plot.title = ggplot2::element_text(size = 22),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black")
    )
  chart
}
