#' Get database file name
#' 
#' Get the full file name for a tag project or receiver database
#'
#' @param projRecv Project code or receiver id
#' @param dir character scalar: path to folder where databases are stored
#'   default: `getwd()`
#'
#' @return character scalar giving the full path to the database file for
#' the specified tag project or receiver.  Both DB types have file extensions
#' ".motus", and the base filename looks like either "project-X.motus" or
#' "SG-1234BBBK5678.motus" or "Lotek-12345.motus"
#'
#' @examples
#'
#' # get the path to the database for tag project 123
#' getDBFilename(123) 
#'
#' # get the path to the database for receiver SG-1234BBBK5678
#' getDBFilename("SG-1234BBBK5678")
#'
#' # get the path to the database for project 5 in folder '/home/me/mydbs'
#' getDBFilename(5, "/home/me/mydbs") 
#'
#' @seealso `tagme()`
#'
#' @noRd

getDBFilename <- function(projRecv, dir = getwd()) {
    if (is.numeric(projRecv)) {
        dbName <- glue::glue("project-{projRecv}.motus")
    } else {
        dbName <- glue::glue("{projRecv}.motus")
    }
    file.path(dir, dbName)
}
