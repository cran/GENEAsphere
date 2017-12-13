
#' From the raw data to create a representation of light, temperature and MAGSA (Mean Absolute Gravity Substituted Acceleration). 
#'
#'@title plotAccData
#'
#'@description  Creates a plot of the Acc Data given a resolution.
#'
#'@param x should be an Acc Data object.
#'@param what What variable to plot against time. Options are: \enumerate{
#'\item sd: Standard of movement given the resolutions
#'\item mean: Mean of movementgiven the resolutions
#'\item temperature
#'\item light
#'\item voltage}
#'@param draw if FALSE, plot a whole new plot. Otherwise, superimpose on to existing plot.
#'@param resolution Resolution of plot to create.
#'@param ... resolution of underlying grid.
#'
#' @importFrom GENEAread get.intervals epoch.apply convert.time
#'@export
#'@details Creates a line plot at a certain resolution from the GENEAread AccData objects available.
#'
#'@examples
#' ## AccData = read.bin(datafile) # where data file is a GENEActiv .bin file.
#' ## saveRDS(AccData , "AccData.rds")
#' x = readRDS(system.file("extdata", "AccData.rds", package = "GENEAsphere"))
#' plot.AccData(x, what = ("sd"))
#' plot.AccData(x, what = ("sd"))
#' plot.AccData(x, what = ("mean"))
#' plot.AccData(x, what = ("temperature"))
#' plot.AccData(x, what = ("light"))
#' plot.AccData(x, what = ("voltage"))


plotAccData <- function(x, what = c("sd", "mean", "temperature", "light", "voltage"), draw = TRUE, resolution = 200,...){
  what = match.arg(what)
  if (is.null(what)){
    epoch = floor(nrow(x)/resolution + 1)/x$freq
    if (what == "sd"){
      obj = epoch.apply(x, epoch, TRUE, FUN = function(t) sd(svm(t)))
    } else if (what == "mean"){
      obj = epoch.apply(x, epoch, TRUE, FUN = function(t) mean(svm(t)))
    } else if (what == "temperature"){
      obj = epoch.apply( x, epoch, incl.date = T, function(t) mean(t[,7]))
    } else if (what == "voltage"){
      obj = x[(1: floor(nrow(x) / epoch) * epoch), c(1, 7)]
      obj[2] = x$voltage[(1: floor(nrow(x) / epoch) * epoch)]
    } else {
      #current workaround for light?
      obj = epoch.apply(x, epoch, incl.date = T, FUN = function(t) max(t[,5]))
    }
    if (draw) plot(convert.time(obj[,1]) ,obj[,2] ,  type = "l", xlab = "Time", ylab = what, ...)
    return(invisible(obj))
  } else {
    plot(convert.time(x[,1]), epoch.apply(x, epoch, TRUE, FUN = function(t) mean(svm(t))), ...)
    return(NULL)
  }
}

