#  AUTHOR: COLLIN MITCHELL
#    DATE: 2016/03/18
# PURPOSE: Queue Curve

# Library Calls
library(dplyr)
library(ggplot2)
library(data.table)

### Variables
# -----------
path<- file.path('__data', 'QueueCurve')
load( file.path( path, "EmailAddresses.RData") )


### Functions
# ------------

# Collect data
## no-return: create data
collectData <- function(){
  
  # HOME DIR GOES HERE
  dataPart1 <- data.table(read.csv("calls1.csv")) # 1-10
  dataPart2 <- data.table(read.csv("calls2.csv")) # 11-20
  dataPart3 <- data.table(read.csv("calls3.csv")) # 21-31
  
  #data <- data.table(read.csv("calls.csv"))
  dataset = rbind(dataPart1, dataPart2, dataPart3)
  rm(dataPart1, dataPart2, dataPart3)
  return(dataset)
}

# Prepare data
## return cleaned dataset
prepareData <- function(dataset){
  dates = Sys.Date() + seq(-31, 0)
  dateLength <- length(dates)
  dateLength <<- dateLength
  dates      <<- dates
  
  # Convert to Date Object
  dataset$Opened   <- as.Date(dataset$Opened)
  dataset$Modified <- as.Date(dataset$Modified)
  
  # Convert Fixed to 1's or 0's
  fixed <- ifelse(dataset$Resolution == "FIXED", TRUE, FALSE)
  
  # Null the old column; add in new one.
  dataset[, Resolution := NULL]
  dataset$Fixed      <- fixed
  return(dataset)
}

# prepareMatrix
## return zero'd matrix
prepareMatrix <- function(dataset){
  fill <- rep(0, dateLength * length(dataset$Call.ID))
  returnMatrix <- matrix( fill, nrow(dataset), dateLength )
  return(returnMatrix)
}

# createMatrix
## return completed Matrix
createMatrix <- function(dataset, internalMatrix){
  for( iter in 1:length(dataset$Call.ID) ){
    # Initial data needed for both comparisons
    dateClosed <- dataset$Modified[iter]
    dateOpened <- dataset$Opened[iter]
    
    
    # if fixed, create subsection of 1's
    if(dataset$Fixed[iter]){
      insertRow <- ifelse( ((dates >= dateOpened) == TRUE) & ( (dates <= dateClosed) == TRUE), 1, 0)
      internalMatrix[iter, ] <- insertRow
    }
    # else 1's pivot on date opened
    else{
      dateOpened <- dataset$Opened[iter]
      insertRow <- ifelse(Sys.Date() - dateOpened < as.numeric(Sys.Date() - dates), 0, 1)
      internalMatrix[iter, ] <- insertRow
    }
  }
  
  return(internalMatrix)
}

# collapseMatrix
## return ticket counts as a vector
collapseMatrix <- function(internalMatrix){
  nColumns <- ncol(internalMatrix)
  dailyCount <- rep(0, nColumns)
  
  # collapse matrix
  for(iter in 1:nColumns){
    dailyCount[iter] <- sum(internalMatrix[,iter])
  }
  
  return(dailyCount)
}

# cycleCallOwners
## no-return
cycleCallOwners <- function(dataset){
  matrixFrame <- data.frame()
  nameVector <- vector()
  for( name in as.character(unique(dataset$Assigned.To)) ){
    subsetSlice <- filter(dataset, Assigned.To == name)
    referenceMatrix <- prepareMatrix(subsetSlice)
    expandedMatrix <- createMatrix(subsetSlice, referenceMatrix)
    matrixSlice <- collapseMatrix(expandedMatrix)
    matrixFrame <- rbind(matrixFrame, matrixSlice)
    nameVector <- c(nameVector, name)
  }
  
  matrixFrame <<- matrixFrame
  nameVector <<- nameVector
}



# createGraph
createGraph <- function(){
  for(iter in 1:nrow(matrixFrame)){
    if( !(nameVector[ iter ] %in% filterNames)) next
    # data
    username = paste(nameVector[iter], ".png", sep = "")
    
    png(file = username, width = 1000, height= 1000)
    nameplot <- qplot(x = dates, y = unlist(matrixFrame[iter,]),
                      geom = "line", main = nameVector[iter],
                      ylab = "Ticket Count", xlab = "Date") + 
                scale_x_date(date_labels = '%b %d', date_minor_breaks = '1 day')
    print(nameplot)
    dev.off()
  }
}


# main
#------------------------
## Start
data<- collectData()
data<- prepareData(data)

# Generate Matrix data
cycleCallOwners(data)

# Graph User's Data
createGraph()