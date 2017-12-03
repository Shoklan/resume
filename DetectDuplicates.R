# Author: Collin Mitchell
# Date: Edited 2017/12/4


get.detectDuplicates<- function( filename = "REPORTING.csv", threshold = .85, output = FALSE ){
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