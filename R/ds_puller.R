#'ds_puller
#'
#'A function for pulling Datastream data
#'
#'This function is a wrapper for the function ds() from
#'from the package Rdatastream.
#'
#' @param SECURITITES A vector of Datastream codes.
#' @param FIELDS A list of Datastream fields.
#' @param fromDATE Date from which to begin data pull.
#' @param toDATE Date to end data pull.
#' @param PERIOD Frequency of data, possible values are "D", "M", "Q," "Y".
#' @param REQUESTS Currently not used. Will eventually be used to pass custom Datastream API syntax to Datastream servers
#'
#' @examples
#' ds_puller( SECURITITES = c("TOTMKUS"), FIELDS = list("MV"), fromDATE = "2014-09-14", toDATE = "2015-01-31", PERIOD = "D")
#'
#' @return a list with two dataframes, named Data and Metadata
#'
#' @importFrom plyr join_all
#' @importFrom plyr ldply
#' @import RDatastream
#'
#' @export
ds_puller <- function( SECURITITES, FIELDS, fromDATE, toDATE, PERIOD, REQUESTS = NULL ) {

USER <- list( username = "DS:XIMF901", password = "MONETARY" ) # enter in Datastream log-in details here

# this function is just a wrapper for ds() and uses the same args

N <- length(SECURITITES) # number of list entries

data_only <- vector( "list", N ) # intialize object to store individual data series

meta_data_only <- vector( "list", N ) # intialize object to store individual data series

for (i in 1:N) {

Fi <- FIELDS[[i]]
if ( Fi %in% "P" ) { Fi <- NULL } # end if

if( !is.null(REQUESTS) ) { # case where custom request is passed

blah <- RDatastream::ds(
  user = USER,
  requests = REQUESTS )["Data",] # extracts just the data

} else {

blah <- RDatastream::ds(
  user = USER,
  securities = SECURITITES[i],
  fields = Fi,
  fromDate = as.Date( fromDATE ),
  toDate = as.Date( toDATE ),
  period = PERIOD)["Data",] # extracts just the data

} # end if/else

blah <- as.data.frame( blah, stringsAsFactors = F)

if ( is.null(Fi) ) { Fi <- "P" } # end if

keep1 <-  which( names(blah) %in% c("DATE", Fi) )
keep2 <-  which( !( names(blah) %in% c("DATE", Fi) ) )

data_only[[i]] <- blah[, keep1, drop = F ]

entity <- blah$DISPNAME[1] # extracts entity name

names(data_only[[i]])[-1] <- stringr::str_c( entity, " ", Fi ) %>% str_replace_all(pattern =  " ", replacement =  "_")

meta_data_only[[i]] <- blah[, keep2, drop = F ] %>% dplyr::distinct() # keep only unique meta data records

} # end loop

data_only <- plyr::join_all( dfs = data_only, by = "DATE", type = "full" ) # create a single dataframe of data

meta_data_only <- plyr::ldply( .data = meta_data_only, .fun = rbind ) # create a single dataframe unique metadata

ds_data <- list( Data = data_only, Metadata = meta_data_only ) # return both the data and metadata as a list

return(ds_data)

}
getwd()
