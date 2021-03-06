% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/positionals.R
\name{positionals}
\alias{positionals}
\title{Positionals}
\usage{
positionals(AccData, start = 0, end = 1, length = NULL,
  max.points = 1e+06, ...)
}
\arguments{
\item{AccData}{object to plot, can be matrix or accdata object}

\item{start}{time at which to start (Default set at 0).}

\item{end}{time at which to end (Default set at 1).}

\item{length}{Length of interval.}

\item{max.points}{maximum number of data points to plot. Data will be downsampled to achieve this.}

\item{...}{Arguements passed to code{\link[GENEAread]{get.intervals}}}
}
\value{
There is no return to the console. As a side effect an rgl graphic is created.
}
\description{
Creates a positionals plot of the GENEAcitv .bin data.
}
\details{
From the raw data to create a representation of arm elevation and wrist rotation.
}
\examples{
\dontrun{
x = readRDS(system.file("extdata", "AccData.rds", package = "GENEAsphere"))
positionals(x)
}
}
