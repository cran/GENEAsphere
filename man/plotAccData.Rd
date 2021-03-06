% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotAccData.R
\name{plotAccData}
\alias{plotAccData}
\title{plotAccData}
\usage{
plotAccData(x, what = c("sd", "mean", "temperature", "light", "voltage"),
  draw = TRUE, resolution = 200, ...)
}
\arguments{
\item{x}{should be an AccData object.}

\item{what}{What variable to plot against time. Options are: \enumerate{
\item sd: Standard of movement given the resolutions
\item mean: Mean of movementgiven the resolutions
\item temperature
\item light
\item voltage}}

\item{draw}{if TRUE, plot a whole new plot. Otherwise, superimpose on to existing plot.}

\item{resolution}{Resolution of plot to create.}

\item{...}{resolution of underlying grid.}
}
\description{
Creates a plot of the Acc Data given a resolution.
}
\details{
From the raw data to create a representation of light, temperature and MAGSA (Mean Absolute Gravity Substituted Acceleration).

Creates a line plot at a certain resolution from the GENEAread AccData objects available.
}
\examples{
\dontrun{
## AccData = read.bin(datafile) # where data file is a GENEActiv .bin file.
## saveRDS(AccData , "AccData.rds")
x = readRDS(system.file("extdata", "AccData.rds", package = "GENEAsphere"))
plotAccData(x, what = ("sd"))
plotAccData(x, what = ("sd"))
plotAccData(x, what = ("mean"))
plotAccData(x, what = ("temperature"))
plotAccData(x, what = ("light"))
plotAccData(x, what = ("voltage"))
}
}
