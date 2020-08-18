#' Share Price Increase
#'
#' Time series of price increases over a 60 day period.
#'
#' @format A data frame with 965 rows and 3 variables:
#' \describe{
#'   \item{id}{unique id of the time series}
#'   \item{class}{whether or not the time series increased or not}
#'   \item{ts}{list column, containing a time series of length 60 for each row}
#'   ...
#' }
#' @source \url{http://www.timeseriesclassification.com/description.php?Dataset=SharePriceIncrease}
"prices"

#' Ethanol levels
#'
#' Time series of ethanol with varying levels of alcohol. Four classes.
#'
#' @format A data frame with 504 rows and 3 variables:
#' \describe{
#'   \item{id}{unique id of the time series}
#'   \item{class}{four classes: e35, e38, e40, e45}
#'   \item{ts}{list column, containing a time series of length 1751 for each row}
#'   ...
#' }
#' @source \url{http://www.timeseriesclassification.com/description.php?Dataset=EthanolLevel}
"ethanol"
