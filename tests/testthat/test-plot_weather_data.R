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
