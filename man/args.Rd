% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aa_common_docs.R
\name{args}
\alias{args}
\title{Common arguments}
\arguments{
\item{projRecv}{Numeric project code from motus.org, \emph{or} character receiver
serial number.}

\item{src}{SQLite connection (result of \code{tagme(XXX)} or
\code{DBI::dbConnect(RSQLite::SQLite(), "XXX.motus")})}

\item{resume}{Logical. Resume a download? Otherwise the table is
removed and the download is started from the beginning.}

\item{batchID}{Numeric. Id of the batch in question}

\item{batchMsg}{Character. Message to share}

\item{projectID}{Numeric. Id of the Project in question}

\item{filterName}{Character. Unique name given to the filter}

\item{motusProjID}{Character. Optional project ID attached to the filter in
order to share with other users of the same project.}
}
\description{
Common arguments
}
\keyword{internal}
