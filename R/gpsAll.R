#' Add/update all GPS points
#' 
#' Download or resume a download of the `gpsAll` table in an existing Motus
#' database. Batch activity refers to the number of hits detected during a given
#' batch. Batches with large numbers of hits may indicate interference and thus
#' unreliable hits.
#'
#' @param src src_sqlite object representing the database
#' @param resume Logical. Resume a download? Otherwise the `activity` table is
#'   removed and the download is started from the beginning.
#' 
#' @details This function is automatically run by the [tagme()] function with
#'   `resume = TRUE`. 
#'   
#'   If an `activity` table doesn't exist, it will be created prior to
#'   downloading. If there is an existing `activity` table, this will update the
#'   records.
#'
#' @examples
#' 
#' # download and access data from project 176 in sql format
#' # usename and password are both "motus.sample"
#' \dontrun{sql.motus <- tagme(176, new = TRUE, update = TRUE)}
#' 
#' # OR use example sql file included in `motus`
#' sql.motus <- tagme(176, update = FALSE, 
#'                    dir = system.file("extdata", package = "motus"))
#'   
#' # Access 'activity' table
#' library(dplyr)
#' a <- tbl(sql.motus, "activity")
#'   
#' # If interrupted and you want to resume
#' \dontrun{my_tags <- activity(sql.motus, resume = TRUE)}
#'
#' @export

gpsAll <- function(src, resume = FALSE) {
  
  pageInitial <- function(returnID, projectID) srvGPSForAll(gpsID = returnID)
  
  pageForward <- function(b, returnID, projectID) {
    srvGPSForAll(gpsID = returnID) 
  }
  
  pageDataByReturn(src, table = "gpsAll", resume = resume,
                   pageInitial, pageForward)
  
}