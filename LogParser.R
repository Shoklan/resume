# Author: Collin Mitchell
#   Date: Edited on 2017/12/3

##-- Libraries
library( purrr )
library( stringr )
library( lubridate )

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