#      Author: Colling Mitchell
#    Purpose: Functions associated with completing and running tasks,

####
# Functions
##
 # General:
 # logParser()
 # printOutput()
 # conflictingArms()
 # checkCollaborationFile()

 # collect:
 # collectErrorIndexes()

 




####
# call:
#######
callConflictingArms <- function(){
  conflictingArms()
}

checkReportingV2File <- function(){
  checkReportingV2File()
}


####
# General Functions
#####
logparser <- function(){
  # Replace , for . so R can parse to Date Object
  replaceComaForPeriod<- function(df){
    df$Datetime<- gsub(',', replacement = '.', df$Datetime)
    
    return(df)
  } # END replaceComaForPeriod
  
  # function to brute force figure out which parser
  checkParserType <- function(fileList, patterns){
    useFlag <- sum( unlist(map( patterns, function(.p){
      unlist( map(fileList, str_detect, pattern = .p))
    }))) > 0
    return( useFlag)
  }
  
  generalFormat6Column <- c("list", "of", "target", "files")
  
  generalFormat5Column <- c("list", "of", "target", "files")
  
  generalFormat4Column <- c("list", "of", "target", "files")
  
  webUI7Column <- c("UI.log")
  
  fileList <- Filter(function(x) file_test("-f", x), list.files())
  
  useGeneral6Column <- checkParserType(fileList, generalFormat6Column)
  useGeneral5Column <- checkParserType(fileList, generalFormat5Column)
  useGeneral4Column <- checkParserType(fileList, generalFormat4Column)
  useWebUI7Column   <- checkParserType(fileList, webUI7Column)
  
  
  
  if( useGeneral6Column ){
    pythonScriptPath <- file.path("utils", "generalFormat6Column.py")
    columnNames <- c("Filename", 'Datetime', "Timezone", "Thread", "Info", "Data")
    columnTypes <- c("cccccc")
    skipFlag <- TRUE
  }
  
  
  if( useGeneral5Column ){
    pythonScriptPath <- file.path("utils", "generalType5Column.py")
    columnNames <- c("Filename", 'Datetime', "Timezone", "Info", "Data")
    columnTypes <- c("ccccc")
  }
  
  if( useGeneral4Column ){
    pythonScriptPath <- file.path("utils", "generalFormat4Column.py")
    columnNames <- c("Filename", 'Datetime', "Thread", "Info", "Data")
    columnTypes <- c("ccccc")
  }
  
  # WebUI
  if( useWebUI7Column ){
    pythonScriptPath <- file.path("utils", "webUI7Column.py")
    columnNames <- c("Filename", 'Datetime', "Timezone", "Thread", "Info", "Data")
    columnTypes <- c("cccccc")
  }
  
  # reader template
  # pythonScriptPath <- file.path("utils", <python_file> )
  # columnNames <- c("")
  # columnTypes <- c("")
  
  if( !exists( "pythonScriptPath" )) stop("This type of file is not defined.")
  if( !file.exists( pythonScriptPath )) stop("Python Script is missing")
  if( file.exists('output.csv')) file.remove('output.csv')
  system2("python", args = pythonScriptPath, wait = TRUE)
  
  
  
  data <- suppressMessages( suppressWarnings(
    data.table( read_delim("output.csv",
                           delim = '~',
                           col_names = columnNames,
                           col_types = columnTypes, 
                           progress = FALSE))
  ))
  
  # Correct the time format
  data <- replaceComaForPeriod( data )
  data$Datetime <-suppressWarnings( ymd_hms( data$Datetime ))
  
  # data <<- data
  # data %>% collectErrorIndexes %>% printOutput(data, .)
  
  return( data )
} # END logparser


# Write data to output file
# < [ vector, data.table, ?date ]
# > [ *.txt ]
# deps( data.table )
printOutput<- function( data, indexes ){
  # request name and open  for writing.
  filename <- 'output.txt'
  sink(filename, append = TRUE)
  
  # Headers
  cat( sprintf('%-21s%-45s%-150s', '[ Date ]', '[ File ]', '[ Data ]' ), '\n')
  
  # dump data
  for(iter in indexes){
    if( is.na( data$Data[ iter ] )) next
    cat( sprintf( '%-21s%-45s%-125s',
                  ifelse( is.na( data$Datetime[ iter ]), "", as.character( anytime( data$Datetime[ iter ] ))),
                  data$Filename[ iter ],
                  data$Data[ iter ]),
         "\n")
  }
  
  # close connection
  sink()
} # END printOutput


####
# collect:
######

# retrieve the indexes potentially containing errors.
collectErrorIndexes <- function( data ){
  indexes<- sort( c( which( data$Info == "ERROR" ), which( is.na( data$Info ))))
  return( indexes )
}




detectDuplicates<- function( filename = "REPORTING.csv", threshold = .85, output = FALSE ){
  ##### Functions  #####
  
  # create a token from a character list
  tokenCreate<-function( .x ){
    unlist( strsplit(.x , ' '))
  } # END tokenCreate
  
  # Create ratio of token overlap
  tokenIntersect<- function(tokenOne, tokenTwo){
    tokenLength<- length( tokenOne )
    length( intersect( tokenOne, tokenTwo )) / tokenLength
  }
  
  # Get a table of thresholds
  tokenDataTable<- function( tokenList ){
    tokenMatrix<- map( tokenList, function( token ){
      unlist(map( tokenList, tokenIntersect, tokenOne = token))})
    
    data.table( matrix( unlist( tokenMatrix ), nrow = length( tokenMatrix )))
  } # END tokenDataTable
  
  # Filter through important indexes
  selectionIndexes<- function( indexList, dataSlice ){
    
    # go through each sublist checking for overlapping tech assignments.
    collectedIndexes<- unlist( map( indexList, function(indexes){
      # if more than one person,
      grabIndexes<- length(unique( dataSlice[indexes, "Assigned To"])) > 1
      
      # then, add indexes to list
      if( grabIndexes ) indexes
    }))
    
    # return sort unique list of indexes
    unique(sort( collectedIndexes ))
    
  } # END selectionIndexes
  
  # Get data
  data<- fread( filename )
  
  # Filter and keep only the important columns
  data<- data %>%
    select(`Call ID`, `Assigned To`, Customer, Summary)
  
  # ignore list
  clientIgnoreList<- c()
  
  Results<- data.table( "Call ID" = character(0), "Assigned To" = character(0), "Customer" = character(0), "Summary" = character(0))
  
  # cycle through all unique customers
  for( client in unique( data$Customer)){
    
    # if client in ignore list, then ignore.
    if( client %in% clientIgnoreList) next
    
    # else, continue
    temp<- data %>% filter( Customer == client)
    
    # create list of tokens
    tokens<- map( temp$Summary, tokenCreate  )
    
    # Create table of thresholds.
    tokenIntersectTable<- tokenDataTable( tokens )
    
    # Collect indexes we're interested in.
    potentialIndexes<- map(tokenIntersectTable, function( x ){ which(x > threshold)})
    
    # get important indexes
    indexes<- selectionIndexes( potentialIndexes, temp )
    
    if( !is.null( indexes ) ) Results<- rbind( Results, temp[indexes, ])
  }
  
  write.csv( Results, file = 'Duplicates.csv', row.names = FALSE )
  if( output) Results
  
} # END detectDuplicates


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