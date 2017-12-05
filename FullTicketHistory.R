# Author: Collin Mitchell
# Date: Edited 2017/12/5

#== Libraries ==#
library( purrr )
library( data.table )
library( ggplot2 )
library( dplyr )


#== Functions ==#

# Tidy up and subset the data
prepareData <- function(dataset){
  
  dataset <- as_tibble( dataset )
  # Convert to Date Object
  dataset$Opened   <- as.Date(dataset$Opened)
  dataset$Modified <- as.Date(dataset$Modified)
  
  # Assess whether the ticket is still open or not.
  dataset <- mutate(dataset, Fixed = Status %in% c('RESOLVED', 'CLOSED'))
                  
  dataset <- select( dataset, Opened, Modified, Status, Fixed)
  return( dataset )
}

# Vectorize the inital matrix for data insertion.
prepareMatrix <- function(dataset){
  # how many days from beginning of the job to the end?
  days <- as.numeric( max( dataset$Modified) - min( dataset$Opened))+1
  
  # R is faster when you prebuild the container.
  fill <- rep(0, dim(dataset)[1] * (days + 1))
  initDateMatrix <- matrix( fill, nrow = nrow( dataset ), ncol = days )
  return( initDateMatrix )
}


#== Main ==#
# collect slices.
slice1 <- fread('tickets1.csv')
slice2 <- fread('tickets2.csv')
slice3 <- fread('tickets3.csv')

# merge slices into a single dataset.
data <- rbindlist(list( slice1, slice2, slice3))

# clean data
data <- prepareData(data)

# initalize.
initDateMatrix <- prepareMatrix( data )

# Find out how many days there are
# and create the date objects for comparisons.
days <- as.numeric( max( dataset$Modified ) - min( dataset$Opened ))
dateVector <- min( dataset$Opened ) + seq(0, days)

# vectorize full row to speed up.
insertRow <- rep(0, ncol( initDateMatrix )-1 )

# Insert each row into the matrix per ticket.
for( i in 1:nrow( data ) ){
  # if fixed, create subsection of 1's
  if( data$Fixed[i] )initDateMatrix[i, ] <- ifelse( ((dateVector >= data$Opened[i]) == TRUE) & ( (dateVector <= data$Modified[i]) == TRUE), 1, 0)
  
  #else 1's pivot on date opened
  else initDateMatrix[i, ] <- ifelse( dateVector < data$Opened[i], 0, 1)
}

# sum the columns where the columns are a unique day.
# append a day beyond to create a flat bottom'd graph.
ticketCounts <- c( colSums( initDateMatrix ), 0)
dateVector <-  c(dateVector, min(data$Opened) + days+1)

# generate plot
g <- ggplot( tibble( dateVector, ticketCounts), aes( x = dateVector, y = ticketCounts))
g <- g + geom_polygon(color = '#3182bd', fill = '#63AEE6', alpha = .8)
g <- g + ggtitle("TICKETS")
g

# custom theme for display.
g + theme( panel.background = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           #axis.title.y = element_text( hjust = 0.165, size = 7,  face = 'bold'),
           #axis.line = element_line(colour = 'grey', lineend = 'round', )
           plot.title = element_text(hjust = 0.0, face = 'bold', size = 10)
)
