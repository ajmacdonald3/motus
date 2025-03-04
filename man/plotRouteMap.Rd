% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotRouteMap.R
\name{plotRouteMap}
\alias{plotRouteMap}
\title{Map of tag routes and sites coloured by id}
\usage{
plotRouteMap(
  data,
  zoom = 3,
  lat = NULL,
  lon = NULL,
  maptype = "terrain",
  recvStart = NULL,
  recvEnd = NULL
)
}
\arguments{
\item{data}{a .motus sql file}

\item{zoom}{Integer.  Values between 3 and 21, 3 being continent level, 10
being city-scale}

\item{lat}{Numeric vector. Top and bottom latitude bounds. If NULL (default)
this is calculated from the data}

\item{lon}{Numeric vector. Left and right longitude bounds. If NULL
(default) this is calculated from the data}

\item{maptype}{Character. Map type to display, can be: "terrain" , "toner",
"watercolor", or any other option available to \code{ggmap::get_stamenmap()}}

\item{recvStart}{Character. Start date for date range of active receivers. If
\code{NULL} uses the full data range}

\item{recvEnd}{Character. End date for date range of active receivers. If
\code{NULL} uses the full data range}
}
\description{
Google map of routes of Motus tag detections coloured by ID.  User defines a
date range to show points for receivers that were operational at some point
during the date range.
}
\details{
By default this function uses Stamen maps
}
\examples{
\dontshow{if (requireNamespace("ggmap", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# You must use a .motus sql file, instructions to load using tagme() are
# below

# download and access data from project 176 in sql format
# usename and password are both "motus.sample"
\dontrun{sql.motus <- tagme(176, new = TRUE, update = TRUE)}


# OR use example sql file included in `motus`
sql.motus <- tagme(176, update = FALSE, 
                   dir = system.file("extdata", package = "motus"))

# Plot routemap of all detection data, with "terrain" maptype, and receivers
# active between 2016-01-01 and 2017-01-01
plotRouteMap(sql.motus, recvStart = "2016-01-01", recvEnd = "2016-12-31")
\dontshow{\}) # examplesIf}
}
