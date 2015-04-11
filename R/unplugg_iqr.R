#'  Anomaly Detection in Utilities Datasets
#'
#'  Detecting anomalies in univariate time series data from utitlity providers.
#'   
#'  @name unplugg_iqr
#'  @param input_df Data frame with the following columns at a minimum
#'    TYPE: type of data: Ex 'Electric Usage'
#'    DATE: date of activity, i.e. each row entry
#'    START TIME: start time of each entry
#'    USAGE: usage for current activity, i.e. count
#'    UNITS: units of usage
#'    TIMESTAMP: timestamp for each usage entry
#'  @param periodicity String indicating periodicity with which to slice data
#'    options are 'second', 'minute', 'hour', 'day', 'week', 'month', 'year'
#'  
#'  Estimates and subtracts trends and seasonal components of time series... 
#'  Then finds outliers in the residuals: +/- 3IQR
#'  
#'  Currently optimized for PGE data available through Opower
#' 

unplugg_iqr <- function(input_df, periodicity) {
  
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
  
  x <- input_agg$usage
  x <- as.ts(x)
  if(frequency(x)>1)
    resid <- stl(x, s.window="periodic", robust=TRUE)$time.series[,3]
  else {
    tt <- 1:length(x)
    resid <- residuals(loess(x ~ tt))
  }
  
  resid.q <- quantile(resid, prob=c(0.25, 0.75))
  iqr <- diff(resid.q)
  limits <- resid.q + 1.5*iqr*c(-1,1)
  score <- abs(pmin((resid - limits[1])/iqr, 0) + pmax((resid - limits[2])/iqr, 0))
  
  plot(x,
        xlab=NA,
        ylab=paste(input_df$type[1], ' ', '(', input_df$units[1], ')'))
  x2 <- ts(rep(NA,length(x)))
  x2[score>0] <- x[score>0]
  tsp(x2) <- tsp(x)
  points(x2, pch=19, col="red")
  
}