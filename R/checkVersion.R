#' Check database version
#' 
#' Verifies the version of the package against the `admInfo` table of a `.motus`
#' file. Those should match if the `updateMotusDb()` function has been properly
#' applied by the `tagme()` function.
#'
#' @inheritParams args
#' @export
#'

checkVersion <- function(src) {

  # Get current version info
  current_version <- max(sql_versions$date)
  message("Motus package database version: ", current_version)

  # Get current database info
  message("Your motus sqlite file: ", src@dbname)
  
  # If database has admin info table
  if (DBI::dbExistsTable(src, "admInfo")) {
    local_version <- dplyr::tbl(src, "admInfo") %>%
      dplyr::pull(.data$db_version) %>%
      as.POSIXct(., tz = "UTC")
    message("Your motus sqlite file version: ", local_version)
  
    if (local_version != current_version) {
      message("Your motus sqlite file version does not match the package. ",
              "Please refer to the Motus R Book (troubleshooting chapter).")
    }	else {
      message("Your motus sqlite file is up-to-date with the package.")
    }
  } else {
    message("The admInfo table has not yet been created in your motus sqlite ", 
            "file. Please refer to the Motus R Book (troubleshooting chapter).")
  }
}
