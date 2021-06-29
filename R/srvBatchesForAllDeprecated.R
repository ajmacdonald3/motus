#' Fetch all deprecated batches
#'
#' @param batchID Numeric. Largest batchID already fetched. For pagination,
#'   starts with 0 (default).
#' @param verbose Logical. Whether or not to make a verbose query.
#'
#' @noRd

srvBatchesForAllDeprecated <- function(batchID = 0, verbose = FALSE) {
  srvQuery(API = motus_vars$API_BATCHES_FOR_ALL_DEPRECATED, 
           params = list(batchID = I(batchID)),
           verbose = verbose) %>%
    as.data.frame()
}
