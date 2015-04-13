#'  Visuzlization of Utilities Datasets
#'
#'  Visualizing univariate time series data from utitlity providers.
#'  Input is a santized data frame returned from unplugg_sanitize.R 
#'  @name unplugg_visualize
#'  @param input_df Data frame with the following columns at a minimum
#'    TYPE: type of data: Ex 'Electric Usage'
#'    DATE: date of activity, i.e. each row entry
#'    START TIME: start time of each entry
#'    USAGE: usage for current activity, i.e. count
#'    UNITS: units of usage
#'    TIMESTAMP: timestamp for each usage entry
#'     
#'  Currently optimized for PGE data available through Opower
#'

unplugg_visualize <- function(input_df) {
  
  # Install necessary package(s)
  library(lubridate)
  
  # need timestamps for most plots so check for it
  if(!("timestamp" %in% colnames(input_df))) 
    stop('Missing TIMESTAMPS')
  
  # if there are at least 6 months of data
  if(length(unique(months(input_df$timestamp))) >= 6) {
    
    # display monthly totals
    barplot(t(rowsum(input_df$usage, format(input_df$timestamp,"%Y-%m"))), las=2, 
            ylab=input_df$units[1])
    
    # display yearly calendar heatmap
    dates <- lubridate::round_date(input_df$timestamp, 'day')
    usage <- input_df$usage
    input_agg <- aggregate(usage ~ dates, FUN=sum)
    all_yrs <- unique(lubridate::year(input_agg$dates))
    num_yrs <- length(all_yrs)
    num_dys <- as.numeric(diff(range(input_agg$dates)))
    
    if(num_yrs <= 2 && num_dys <= 366)
      calendarFlow(input_agg$dates, input_agg$usage)
    else {  
        par(mfrow = c(ceiling(num_yrs/2),2), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0)) 
        for(yr in all_yrs) {
          input_sub <- subset(input_agg, format(dates,'%Y')==yr)
          if(as.numeric(diff(range(input_sub$dates))) >= 15) {
            calendarFlow(input_sub$dates, input_sub$usage)
          }
        }
    }
    
  }
  
}