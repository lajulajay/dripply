#'  Anomaly Detection in Utilities Datasets
#'
#'  Detecting anomalies in univariate time series data from utitlity providers.
#'  Input is a data frame with columns TYPE, DATE, TIME, USAGE and UNITS
#'  @name unplugg_shesd
#'  @param input_df Data frame with the following columns at a minimum
#'    TYPE: type of data: Ex 'Electric Usage'
#'    DATE: date of activity, i.e. each row entry
#'    START TIME: start time of each entry
#'    USAGE: usage for current activity, i.e. count
#'    UNITS: units of usage
#'  Uses Seasonal Hybrid ESD Test at https://github.com/twitter/AnomalyDetection 
#'  Currently optimized for PGE data available through Opower
#'  

unplugg_shesd <- function(input_df) {
  
  # Check for and install required packages
  packages <- c('lubridate', 'devtools', 'AnomalyDetection')
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  # Ensure that all input columns are present
  if(!is.data.frame(input_df)) {
    stop('Input must be a data frame')
  } else {
    names(input_df) <- tolower(names(input_df))
    if(!("type" %in% colnames(input_df))) {stop('Input must include TYPE')}
    if(!("date" %in% colnames(input_df))) {stop('Input must include DATE')}
    if(!("start.time" %in% colnames(input_df))) {stop('Input must include START TIME')}
    if(!("usage" %in% colnames(input_df))) {stop('Input must include USAGE')}
    if(!("units" %in% colnames(input_df))) {stop('Input must include UNITS')}
  }
  
  # Ensure that input content is valid
  if(!(all(input_df$type==input_df$type[[1]]))) {stop('Input entries must be same type')}
  if(!(all(input_df$units==input_df$units[[1]]))) {stop('Input entries must be same units')}
  
  # Format input content for AnomalyDetectionTs
  input_df$timestamp = parse_date_time(paste(input_df$date, ' ', input_df$start.time), 'mdy hm')
  anom_df <- data.frame(input_df$timestamp, input_df$usage)
  
  # Invoke AnomalyDetectionTs
  anoms = AnomalyDetectionTs(anom_df, direction='pos', alpha = 0.01, threshold='med_max', 
                             longterm=F, piecewise_median_period_weeks = 2, plot=T, 
                             ylabel=paste(input_df$type[1], ' ', '(', input_df$units[1], ')')) 
  anoms$plot
  return(anoms)
}