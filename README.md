
<!-- README.md is generated from README.Rmd. Please edit that file -->

# noaastnr

<!-- badges: start -->
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

You can install the released version of noaastnr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("noaastnr")
```

And the development version from [GitHub](https://github.com/) with:

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

## Related Packages

There are few packages in the R ecosystem like…

## Code of Conduct

Please note that the noaastnr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
