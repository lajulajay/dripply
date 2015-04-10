#'  Anomaly Detection in Utilities Datasets
#'
#'  Detecting anomalies in univariate time series data from utitlity providers.
#'   
#'  @name unplugg_shesd
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
#'  Uses Seasonal Hybrid ESD Test at https://github.com/twitter/AnomalyDetection 
#'  Currently optimized for PGE data available through Opower
#'  

unplugg_shesd <- function(input_df, periodicity) {
  
  # Check periodicity
  periodicity_options <- c('second', 'minute', 'hour', 'day', 'week', 'month', 'year')
  if(is.character(periodicity)) {periodicity <- tolower(periodicity)}
  else {
    stop('Periodicity must be string')
  }
  if(!(periodicity %in% periodicity_options)) {stop('Invalid periodicity')}
  
  # Install package(s) as necessary
  packages <- c('lubridate', 'devtools')
  for(package in packages){
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  } 

  # Treat AnomlyDetection package specially b/c it requires Github download
  if(!("AnomalyDetection" %in% loadedNamespaces())) {
    devtools::install_github("twitter/AnomalyDetection")
    library(AnomalyDetection)
  }
  
  # Format input content for AnomalyDetectionTs
  dates <- round_date(input_df$timestamp, periodicity)
  usage <- input_df$usage
  input_agg <- aggregate(usage ~ dates, FUN=sum)
  
  # Invoke AnomalyDetectionTs
  anoms = AnomalyDetectionTs(input_agg, direction='pos', alpha = 0.05, plot=T, 
                             ylabel=paste(input_df$type[1], ' ', '(', input_df$units[1], ')')) 
  anoms$plot
  
  

}