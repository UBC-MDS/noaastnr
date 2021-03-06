
<!-- README.md is generated from README.Rmd. Please edit that file -->

# noaastnr

<!-- badges: start -->

[![R build
status](https://github.com/UBC-MDS/noaastnr/workflows/R-CMD-check/badge.svg)](https://github.com/UBC-MDS/noaastnr/actions)
[![codecov](https://codecov.io/gh/UBC-MDS/noaastnr/branch/main/graph/badge.svg)](https://codecov.io/gh/UBC-MDS/noaastnr)
<!-- badges: end -->

The US National Oceanic and Atmospheric Administration (NOAA) collects
and provides access to weather data from land-based weather stations
within the US and around the world ([Land-Based Station
Data](https://www.ncdc.noaa.gov/data-access/land-based-station-data)).
One method for accessing these data is through a publically accessible
FTP site. This package allows users to easily download data from a given
station for a given year, extract several key weather parameters from
the raw data files, and visualize the variation in these parameters over
time. The weather parameters that are extracted with this package are:

-   Air Temperature (degrees Celsius)
-   Atmospheric Pressure (hectopascals)
-   Wind Speed (m/s)
-   Wind Direction (angular degrees)

## Installation

You can install the development version of this package from Github
with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/noaastnr")
```

## Features

-   `get_stations_info`:
    -   This function downloads and cleans the data of all stations
        available at <ftp://ftp.ncei.noaa.gov/pub/data/noaa/>
-   `get_weather_data`:
    -   This function loads and cleans weather data for a given NOAA
        station ID and year. It returns a dataframe containing a time
        series of air temperature, atmospheric pressure, wind speed, and
        wind direction.
-   `plot_weather_data`:
    -   This function visualizes the weather station observations
        including air temperature, atmospheric pressure, wind speed, and
        wind direction changing over time.

## USAGE

A more detailed information about the usage of the features can be found
at
[DOCUMENTATION](https://ubc-mds.github.io/noaastnr/articles/noaastnr.html)

## Dependancies

The list of the dependencies for this package can be viewed under
`Imports:` at
[DESCRIPTION](https://github.com/UBC-MDS/noaastnr/blob/main/DESCRIPTION)

## Related Packages

The package
[`rnoaa`](https://cran.r-project.org/web/packages/rnoaa/index.html)
provides extensive functionality for interfacing with the NOAA’s data
systems including their
[APIs](https://www.ncei.noaa.gov/support/access-data-service-api-user-documentation)
and [FTP](ftp://ftp.ncei.noaa.gov/) service. The `noaastnr` package is a
lightweight alternative to the `rnoaa` package which provides access to
a specific subset of NOAA data, namely, their Integrated Surface
Database (historical global hourly weather data) through their FTP
service.

## Code of Conduct

Please note that the noaastnr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Contributors

We welcome and recognize all contributions. You can see a list of
current contributors in the [contributors
tab](https://github.com/UBC-MDS/noaastnr/graphs/contributors).
