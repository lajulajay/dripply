#'  Anomaly Detection in Utilities Datasets
#'
#'  Detecting anomalies in univariate time series data from utitlity providers.
#'   
#'  @name unplugg_mad
#'  @param input_df Data frame with the following columns at a minimum
#'    TYPE: type of data: Ex 'Electric Usage'
#'    DATE: date of activity, i.e. each row entry
#'    START TIME: start time of each entry
#'    USAGE: usage for current activity, i.e. count
#'    UNITS: units of usage
#'    TIMESTAMP: timestamp for each usage entry
#'  @param periodicity String indicating periodicity with which to slice data
#'    options are 'second', 'minute', 'hour', 'day', 'week', 'month', 'year'
#'  @param window Optional number indcating how many periods to use in learning outliers. Default is 30
#'  @param threshold Optional number indicating upper threshold to use in classifying outliers. Default is 5
#'  
#'  Estimates median absolute deviations from the median
#'  Code is adapted from @whuber (Cross Validated forum) 
#'  
#'  Currently optimized for PGE data available through Opower
#' 

unplugg_mad <- function(input_df, periodicity, window=30, threshold=5) {
  
  # Check periodicity
  periodicity_options <- c('second', 'minute', 'hour', 'day', 'week', 'month', 'year')
  if(is.character(periodicity)) {periodicity <- tolower(periodicity)}
  else {
    stop('Periodicity must be string')
  }
  if(!(periodicity %in% periodicity_options)) {stop('Invalid periodicity')}
  
  # Format input according to periodicity
  dates <- round_date(input_df$timestamp, periodicity)
  usage <- input_df$usage
  input_agg <- aggregate(usage ~ dates, FUN=sum)
  
  m = median(input_agg$usage)
  ut <- median(input_agg$usage) + threshold * median(abs(input_agg$usage - m))
  
  library(zoo)
  z <- rollapply(zoo(y), window, ut, align="right")
  z <- c(rep(z[1], window-1), z) # Use z[1] throughout the initial period
  outliers <- y > z
  
  # Graph the data, show the ut() cutoffs, and mark the outliers:
  plot(x, y, type="l", lwd=2, col="#E00000", ylim=c(0, 20000))
  lines(x, z, col="Gray")
  points(x[outliers], y[outliers], pch=19)
  
}

ut <- function(x) {
  m = median(x); median(x) + threshold * median(abs(x - m))
}