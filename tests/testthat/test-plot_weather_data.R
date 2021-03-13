station_number <-  "911650-22536"
year <- 2020
weather_df <- get_weather_data(station_number, year)

plot_at_m <-
  plot_weather_data(obs_df = weather_df,
                    col_name = "air_temp",
                    time_basis = "monthly")
plot_ap_m <-
  plot_weather_data(obs_df = weather_df,
                    col_name = "atm_press",
                    time_basis = "monthly")
plot_ws_m <-
  plot_weather_data(obs_df = weather_df,
                    col_name = "wind_spd",
                    time_basis = "monthly")
plot_wd_m <-
  plot_weather_data(obs_df = weather_df,
                    col_name = "wind_dir",
                    time_basis = "monthly")
plot_at_d <-
  plot_weather_data(obs_df = weather_df,
                    col_name = "air_temp",
                    time_basis = "daily")
plot_ap_d <-
  plot_weather_data(obs_df = weather_df,
                    col_name = "atm_press",
                    time_basis = "daily")
plot_ws_d <-
  plot_weather_data(obs_df = weather_df,
                    col_name = "wind_spd",
                    time_basis = "daily")
plot_wd_d <-
  plot_weather_data(obs_df = weather_df,
                    col_name = "wind_dir",
                    time_basis = "daily")

# Test mapped axis

test_that("Month should be mapped to the x axis", {
  expect_true("as.Date(month)" == rlang::get_expr(plot_at_m$mapping$x))
  expect_true("as.Date(month)" == rlang::get_expr(plot_ap_m$mapping$x))
  expect_true("as.Date(month)" == rlang::get_expr(plot_ws_m$mapping$x))
  expect_true("as.Date(month)" == rlang::get_expr(plot_wd_m$mapping$x))
})

test_that("Date should be mapped to the x axis", {
  expect_true("as.Date(date)" == rlang::get_expr(plot_at_d$mapping$x))
  expect_true("as.Date(date)" == rlang::get_expr(plot_ap_d$mapping$x))
  expect_true("as.Date(date)" == rlang::get_expr(plot_ws_d$mapping$x))
  expect_true("as.Date(date)" == rlang::get_expr(plot_wd_d$mapping$x))
})

test_that("air_temp should be mapped to the y axis", {
  expect_true("air_temp" == rlang::get_expr(plot_at_m$mapping$y))
  expect_true("air_temp" == rlang::get_expr(plot_at_d$mapping$y))
})

test_that("atm_press should be mapped to the y axis", {
  expect_true("atm_press" == rlang::get_expr(plot_ap_m$mapping$y))
  expect_true("atm_press" == rlang::get_expr(plot_ap_d$mapping$y))
})

test_that("wind_spd should be mapped to the y axis", {
  expect_true("wind_spd" == rlang::get_expr(plot_ws_m$mapping$y))
  expect_true("wind_spd" == rlang::get_expr(plot_ws_d$mapping$y))
})

test_that("wind_dir should be mapped to the y axis", {
  expect_true("wind_dir" == rlang::get_expr(plot_wd_m$mapping$y))
  expect_true("wind_dir" == rlang::get_expr(plot_wd_d$mapping$y))
})

# Test plot configurations

test_that("plot should use geom_line", {
  expect_true("GeomLine" %in% c(class(plot_at_m$layers[[1]]$geom)))
  expect_true("GeomLine" %in% c(class(plot_ap_m$layers[[1]]$geom)))
  expect_true("GeomLine" %in% c(class(plot_ws_m$layers[[1]]$geom)))
  expect_true("GeomLine" %in% c(class(plot_wd_m$layers[[1]]$geom)))
  expect_true("GeomLine" %in% c(class(plot_at_d$layers[[1]]$geom)))
  expect_true("GeomLine" %in% c(class(plot_ap_d$layers[[1]]$geom)))
  expect_true("GeomLine" %in% c(class(plot_ws_d$layers[[1]]$geom)))
  expect_true("GeomLine" %in% c(class(plot_wd_d$layers[[1]]$geom)))
})

# Test input data type

test_that("Test failed for checking input param type", {
  expect_error(
    plot_weather_data(
      obs_df = 1,
      col_name = "air_temp",
      time_basis = "monthly"
    ),
    "Weather data should be a dataFrame."
  )
  expect_error(
    plot_weather_data(
      obs_df = weather_df,
      col_name = 1,
      time_basis = "monthly"
    ),
    "Variable name must be entered as a factor."
  )
  expect_error(
    plot_weather_data(
      obs_df = weather_df,
      col_name = "air_temp",
      time_basis = 1
    ),
    "Time basis must be entered as a factor."
  )
})

# Test input value

test_that("Test failed for checking input values", {
  expect_error(
    plot_weather_data(
      obs_df = weather_df,
      col_name = "test",
      time_basis = "monthly"
    ),
    "Variable can only be one of air_temp, atm_press, wind_spd or wind_dir"
  )
  expect_error(
    plot_weather_data(
      obs_df = weather_df,
      col_name = "air_temp",
      time_basis = "test"
    ),
    "Time basis can only be monthly or daily"
  )
})

# Test input data set size

station_number <-  "714950-99999"
year <- 2004
weather_df_size_test <- get_weather_data(station_number, year)

test_that("Test failed for checking input data set size", {
  expect_error(
    plot_weather_data(
      obs_df = weather_df_size_test,
      col_name = "air_temp",
      time_basis = "monthly"
    ),
    "Dataset is not sufficient to visualize."
  )
  expect_error(
    plot_weather_data(
      obs_df = weather_df_size_test,
      col_name = "air_temp",
      time_basis = "daily"
    ),
    "Dataset is not sufficient to visualize."
  )
})
