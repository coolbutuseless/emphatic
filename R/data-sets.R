

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Challenger o-ring dataset
#'
#' A dataset containing information about the o-ring status of the flights
#' leading up to the Space Shuttle Challenger distaster.
#'
#' Sourced from a table in Tufte's "Visual and Statistical Thinking"
#'
#' @source \url{http://williamwolff.org/wp-content/uploads/2013/01/tufte-challenger-1997.pdf}
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
#'
#' Other references:
#'
#' \itemize{
#' \item{\url{http://williamwolff.org/wp-content/uploads/2013/01/tufte-challenger-1997.pdf}}
#' \item{\url{https://archive.ics.uci.edu/ml/datasets/Challenger+USA+Space+Shuttle+O-Ring}}
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"challenger"





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Monthly Southern Sea Ice Area over the last 40 years
#'
#' From the 'National Snow and Ice Data Center' \url{https://nsidc.org/data/g02135}
#'
#' @format Matrix of sea ice area, monthly from 1978 to 2020.
#'
#' @source  \url{ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/monthly/data/}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"sea_ice_area"
