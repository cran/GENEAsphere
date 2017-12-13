#' A segmented GENEActiv. bin data file of running and walking activities. 
#' The data has not been classified, only the features that are used to classify the 
#' data have been made.
#' 
#' @format A CSV file containing frame with 18 rows and 32 variables:
#' \describe{
#'   \item{Serial.Number}{Serial Number of deivce}	
#'   \item{Start.Time}{UNIX timestamp of the start of the segment}
#'   \item{Segment.Start.Time}{Start of the segment in HH:MM:SS format}
#'   \item{Segment.Duration}{Duration of segment in seconds}	
#'   \item{UpDown.median}{Median arm elevation position}	
#'   \item{UpDown.mad}{Mean absolute deviation of arm position over the segment}
#'   \item{Degrees.median}{Median wrist rotation position}
#'   \item{Degrees.mad}{Mean absolute deviation of wrist rotation position over the segment}
#'   \item{UpDown.mean}{Mean arm elevation position over the segment}
#'   \item{UpDown.var}{Variance of arm elevation}
#'   \item{UpDown.sd}{Standard deviation of arm elevation}
#'   \item{Degrees.mean}{Mean wrist rotation position}
#'   \item{Degrees.var}{Variance of wrist rotation position}
#'   \item{Degrees.sd}{Standard deviation of wrist rotation position}
#'   \item{Magnitude.mean}{Mean of mean absolute gravity substitued acceleration over the segment
#'   \item{Light.mean}{Mean of light readings voer segment}
#'   \item{Light.max}{Max of light readings over segment}
#'   \item{Temp.mean}{Mean of temperature over segment}
#'   \item{Temp.sumdiff}{Sum of differences of temperature over segment}
#'   \item{Temp.meandiff}{Mean of differences of temperature over segment}
#'   \item{Temp.abssumdiff}{absolute Sum of differences of temperature over segment}
#'   \item{Temp.sddiff}{Standard deviation of differences of temperature over segment}
#'   \item{Step.GENEAcount}{Number of Steps counted over segment}
#'   \item{Step.sd}{Standard deviation of steps over segment}
#'   \item{Step.mean}{Mean number of steps over segment (Step count over segment duration)}
#'   \item{Principal.Frequency.median}{Median principal frequency over segment}
#'   \item{Principal.Frequency.mad}{Mean absolute Deviation of principal frequency over segment}	
#'   \item{Principal.Frequency.sumdiff}{Sum of differences of principal frequency over segment duration}
#'   \item{Principal.Frequency.meandiff}{Mean of differences of principal frequency over segment duration}
#'   \item{Principal.Frequency.abssumdiff}{Absolute Sum of differences of principal frequency over segment duration}
#'   \item{Principal.Frequency.sddiff}{Standard deviation of differences principal frequency over segmentduration}
#' }
#' 
