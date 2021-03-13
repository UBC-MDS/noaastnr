usaf_len <- 6
wban_len <- 5
country_len <- 2
state_len <- 2
latitude_len <- 7
longitude_len <- 8
elevation_len <- 7
n_col <- 11

stations_df <- get_stations_info(country="all")
row_id <-  sample(c(1: nrow(stations_df)), 1)
print(row_id)

test_that("Dataframe should have 11 columns.", {
  expect_equal(ncol(stations_df), n_col)
})

test_that("Datetime columns should be represented as date time objects.", {
  expect_equal(class(stations_df$start)[1],"POSIXct")
  expect_equal(class(stations_df$end)[2],"POSIXt")
})

test_that("Character columns should be represented as character objects.", {
  expect_equal(class(stations_df$usaf),"character")
  expect_equal(class(stations_df$wban),"character")
  expect_equal(class(stations_df$station_name),"character")
  expect_equal(class(stations_df$country),"character")
  expect_equal(class(stations_df$state),"character")
  expect_equal(class(stations_df$call),"character")
  expect_equal(class(stations_df$latitude),"character")
  expect_equal(class(stations_df$longitude),"character")
  expect_equal(class(stations_df$elevation),"character")

})


test_that("usaf should be of length 6", {
  check <- is.na(stations_df$usaf[row_id]) | is.null(stations_df$usaf[row_id]) | nchar(stations_df$usaf[row_id]== usaf_len)
  expect_equal(check, TRUE)
})

test_that("wban should be of lengxth 5", {
  check <- is.na(stations_df$wban[row_id]) | is.null(stations_df$wban[row_id]) | nchar(stations_df$wban[row_id]== wban_len)
  expect_equal(check, TRUE)
})

test_that("state should be of length 2 or 'NULL' ", {
  check <- is.na(stations_df$state[row_id]) | is.null(stations_df$state[row_id]) | nchar(stations_df$state[row_id]== state_len)
  expect_equal(check, TRUE)
})

test_that("country should be of length 2 or 'NULL' ", {
  check <- is.na(stations_df$country[row_id]) | is.null(stations_df$country[row_id]) | nchar(stations_df$country[row_id]==country_len)
  expect_equal(check, TRUE)
})

test_that("latitude should be of length 7 or 'NULL' ", {
  check <- is.na(stations_df$latitude[row_id]) | is.null(stations_df$latitude[row_id]) | nchar(stations_df$latitude[row_id]==latitude_len)
  expect_equal(check, TRUE)
})

test_that("longitude should be of length 8 or 'NULL' ", {
  check <- is.na(stations_df$longitude[row_id]) | is.null(stations_df$longitude[row_id]) | nchar(stations_df$longitude[row_id]==longitude_len)
  expect_equal(check, TRUE)
})

test_that("elevation should be of length 7 or 'NULL' ", {
  check <- is.na(stations_df$elevation[row_id]) | is.null(stations_df$elevation[row_id]) | nchar(stations_df$elevation[row_id]==elevation_len)
  expect_equal(check, TRUE)
})
