

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Challenger o-ring dataset
#'
#' A dataset containing information about the o-ring status of the flights
#' leading up to the Space Shuttle Challenger distaster.
#'
#' Sourced from a table in Tufte's "Visual and Statistical Thinking"
#'
#' @format A data.frame
#' \describe{
#'   \item{flight}{Flight number}
#'   \item{temp}{Launch temperature (Fahrenheit)}
#'   \item{erosion}{Number of o-ring erosion incidents}
#'   \item{blowby}{Number of o-ring blow-by incidents}
#'   \item{damage}{Damage severity index}
#'   \item{date}{Date of launch}
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"challenger"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Monthly Southern Sea Ice Area over the last 40 years
#'
#' From the 'National Snow and Ice Data Center' \url{https://nsidc.org/data/g02135}
#'
#' @format Matrix of sea ice area, monthly from 1978 to 2020.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"sea_ice_area"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Monthly total rainfall in Centennial Park, Sydney, Australia
#'
#' From the Australian Bureau of Meteorology
#'
#' @format data.frame with each row representing a year, and each column
#'         representing a month of that year
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"sydney_rain"
