#'  Anomaly Detection in Utilities Datasets
#'
#'  Detecting anomalies in univariate time series data from utitlity providers.
#'  Input is a sanitized data frame with columns TYPE, DATE, TIME, USAGE, UNITS AND TIMESTAMP
#'  @name unplugg_shesd
#'  @param input_df Data frame with the following columns at a minimum
#'    TYPE: type of data: Ex 'Electric Usage'
#'    DATE: date of activity, i.e. each row entry
#'    START TIME: start time of each entry
#'    USAGE: usage for current activity, i.e. count
#'    UNITS: units of usage
#'    TIMESTAMP: timestamp for each usage entry
#'  Uses Seasonal Hybrid ESD Test at https://github.com/twitter/AnomalyDetection 
#'  Currently optimized for PGE data available through Opower
#'  

unplugg_shesd <- function(input_df) {
  
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
  anom_df <- data.frame(input_df$timestamp, input_df$usage)
  
  # Invoke AnomalyDetectionTs
  anoms = AnomalyDetectionTs(anom_df, direction='pos', alpha = 0.05, threshold='p95', 
                             longterm=T, piecewise_median_period_weeks = 2, plot=T, y_log=T,
                             ylabel=paste(input_df$type[1], ' ', '(', input_df$units[1], ')')) 
  anoms$plot
  return(anoms)
}