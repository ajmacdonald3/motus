
pageDataByReturn <- function(src, table, resume = FALSE,
                             pageInitial, pageForward) {
  
  # Check tables and update to include table if necessary
  ensureDBTables(src, projRecv = get_projRecv(src))
  sql <- safeSQL(src)
  
  batchID <- 0
  # Check where to start
  if(resume) {
    # If updating, start with last batch downloaded (a bit of overlap)
    batchID <- sql(paste0("select ifnull(max(batchID), 0) from ", table))[[1]]  
  } else {
    # Otherwise remove all rows and start again
    DBI::dbExecute(src$con, paste0("DELETE FROM ", table))
  }
  
  # If length zero, then no batches to get data for
  data_name <- get_projRecv(src)
  
  if(is_proj(data_name)) {
    projectID <- data_name
  } else {
    projectID <- NULL
  }

  # Announce
  message(sprintf("%s: checking records for new ", table, " data"))
  
  added <- 0
  rounds <- 0
  
  # Get batch
  b <- pageInitial(batchID, projectID)
  
  repeat {
    
    # Progress messages
    msg <- sprintf("batchID %8d: ", batchID)
    
    # Save Previous batch
    dbInsertOrReplace(sql$con, table, b)
    message(msg, sprintf("got %6d %s records", nrow(b), table))
    added <- added + nrow(b)
    
    # Page forward
    batchID <- b$batchID[nrow(b)]
    b <- pageForward(b, batchID, projectID)
    if(nrow(b) == 0) break
    
    # If testing, break out after x batches
    rounds <- rounds + 1
    if(rounds >= getOption("motus.test.max") && is_testing()) break
  }
  
  message("Downloaded ", added, " ", table, " records")
  
  src
}