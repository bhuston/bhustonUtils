#' Dataset of global financial index returns
#'
#' Daily returns for a broad set of national equity and other
#' major financial indices, such as oil prices and option-implied
#' equity volatilities
#'
#' @format A dataframe with 4641 observations 38 variables
#'
#' \describe{
#'  \item{country X}{daily equity returns of the predominant national equity index of country X}
#'  \item{gsci}{daily return of the Goldman Sachs Commoditity Index (GSCI) }
#'  \item{brent.crude}{daily return of the nearest Brent Crude Oil futures contract}
#'  \item{msci.world}{daily return of the MSCI World equity index}
#'  \item{global.volatility}{daily percent change in a "global equity volatility index", which is a simple average of the VIX, VDAX, and Nikkei 225 volatility indices}
#'  }
#'
#'  @source \url{www.quandl.com}
"global_returns_data"
