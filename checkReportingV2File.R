# Author: Collin Mitchell
#   Date: Edited 2017/12/3

##-- Libraries
library( purrr )
library( stringr )
library( rvest )
library( tidyverse )


checkReportingV2File <- function(){
  #######============ FUNCTIONS ============#########
  
  # Function to splice out and return each
  # individual client's volume history
  collectClientData <- function( .x ){
    # splice out dates before converting to numbers
    dates <- .x$Date
    
    # get columns for splicing.
    cNames <- colnames( .x )[-1]
    
    # convert numbers
    numericData <-  .x[, map(.SD, str_replace_all, pattern = ",", replacement = ""), .SDcols = cNames ]
    numericData <- map( numericData, as.numeric) %>% as.data.table()
    
    # collect and return
    cbind( Dates = dates, numericData )
  }
  
  
  # check each dataset to ensure we don't
  # need to investigate
  checkForDeviaions <- function( client_data ){
    investigate <- rep(FALSE, 4)
    
    # collect historical data
    clientColumnSums <- colSums( client_data[,-1], na.rm = TRUE )
    
    # missing last update?
    updateMissed = client_data[1,-1] == 0 &  clientColumnSums > 0
    investigate <- investigate | updateMissed
    
    # check for volume deviation
    checkValues <- map(client_data[,-1], function(.x){
      meanValue <- mean( .x, na.rm = TRUE )
      
      meanValue <- ifelse( is.na( meanValue ), 0, meanValue)
      
      if( meanValue == 0) return( TRUE )
      # second sd dev. away
      spread <- meanValue + c(-1,1)*sd( .x, na.rm = TRUE )*2
      
      # get the slope of the line.
      deviation <- coef( line( .x))[2]
      
      # is the slope inside expected values?
      (deviation > spread[1] & deviation < spread[2])
    })
    
    # check whether it deviated
    didItDeviate <- !unlist(checkValues)
    investigate <- investigate | didItDeviate
    
    investigate <- as.vector( ifelse( investigate, 'Investigate', 'Safe'))
    
    names( investigate ) <- names( didItDeviate )
    
    
    # if all columns are within spread, then we're safe.
    return( investigate )
  }
  
  
  #######============ MAIN ============#######
  # select file
  filesOfInterest<- list.files(pattern = "\\.html")
  
  # read the file
  # text_lines <- readLines( filesOfInterest )
  htmlDoc <- read_html(filesOfInterest)
  
  # collect data
  htmlTables <- htmlDoc %>%
    html_nodes("table") %>% 
    map( html_table, fill = TRUE)
  
  clientNames <- htmlTables[[1]][,1]
  
  # convert to data.tables
  htmlTables <-  map( htmlTables, as.data.table )
  
  
  # select indexes where data exists
  indexes <- which( unlist(map( htmlTables, nrow)) != 30)
  
  # remove OCS data tables
  htmlTables <- htmlTables[ -indexes ]
  
  # finalize and clean data
  data <- map( htmlTables, collectClientData )
  names( data ) <- clientNames
  
  map( data, checkForDeviaions)
} # END checkReportingV2File