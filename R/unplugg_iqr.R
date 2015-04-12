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
  
  y <- as.ts(input_agg$usage)
  y.frequency <- find.freq(y)
  if(frequency(y)>1)
    resid <- stl(y^.5, s.window="periodic", robust=TRUE)$time.series[,3]
  else {
    tt <- 1:length(y)
    resid <- residuals(loess(y^.5 ~ tt))
  }
  
  resid.q <- quantile(resid, prob=c(0.25, 0.75))
  iqr <- diff(resid.q)
  limits <- resid.q + 1.5*iqr*c(-1,1)
  score <- abs(pmin((resid - limits[1])/iqr, 0) + pmax((resid - limits[2])/iqr, 0))
  
  # Generate plot
  plot(y, xaxt='n', xlab=NA, ylab=paste(input_df$type[1], ' ', '(', input_df$units[1], ')'))))
  #axis.POSIXct(side=1, input_agg$dates, format='%Y-%m')  
  y2 <- ts(rep(NA,length(y)))
  y2[score>0] <- y[score>0]
  tsp(y2) <- tsp(y)
  points(y2, pch=19, col="red")
  
}

find.freq <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  return(period)
}