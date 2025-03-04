#' Add/update batch activity
#' 
#' Download or resume a download of the `activity` table in an existing Motus
#' database. Batch activity refers to the number of hits detected during a given
#' batch. Batches with large numbers of hits may indicate interference and thus
#' unreliable hits.
#'
#' @inheritParams args
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

activity <- function(src, resume = FALSE) {
  
  getBatches <- function(src) {
    dplyr::tbl(src, "batches") %>%
      dplyr::pull(.data$batchID)
  }
  
  pageInitial <- function(batchID, projectID) srvActivityForBatches(batchID = batchID)
  
  pageForward <- function(b, batchID, projectID) {
    # Page forward
    ant <- b$ant[nrow(b)]
    hourBin <- b$hourBin[nrow(b)]
    
    # Try again
    srvActivityForBatches(batchID = batchID, 
                          ant = ant, hourBin = hourBin) 
  }
  
  pageDataByBatch(src, table = "activity", resume = resume,
                  getBatches, pageInitial, pageForward)
  
}