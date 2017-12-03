#  Author: Colling Mitchell
# Purpose: functions associated with the Dashboard Info Center UI

###
# Functions
##
 # removeOldProjects()
 # seekProjects()
 # seekCompleted()
 # seekParent()
 # checkForCollision()
 # zipExtract()
 # createComplete()
 # createDownloadHandler()
 # callFunction()



# Delete Projects and Completed
# < [ integer ]
# > [ ]
# deps( )
removeOldProjects<- function(){
  
  # remove data from both directories
  removeParent( completeDirectory )
  removeParent( projectsDirectory )
 
} # END removeOldProjects


removeParent<- function( directory, days = 2 ){
  
  # Collect directories
  setwd(directory)
  directories<- list.dirs(full.names = FALSE)
  
  # If nothing in directories, skip
  if( length(directories) == 0 ) break
  
    
  tempLogical<-lapply(directories[-1], function(x){
    c(x, (as.Date(file.info( x )['mtime'][1,1], format = '%Y-%M-%D %T' ) + days) < Sys.Date()) 
  })
  
  lapply(tempLogical, function(x){
    if( x[2] ) unlink( x[1], recursive = TRUE)
  })
  
} # END removeParent

# Collect names of projects currently running
# < [  ]
# > [ vector ]
# deps( )
seekProjects<- function(){
  return( seekParent( projectsDirectory ))
}# END seekProjects


# Collect names of completed projects
# < [  ]
# > [ vector ]
# deps( )
seekCompleted<- function(){
  return( seekParent( completeDirectory ))
}# END seekCompleted

# Master seek function; does all the heavy lifting for seek* functions
# < [ character ]
# > [ vector ]
# deps( )
seekParent<- function(directory){
  completed<- sort(list.dirs(path = directory, full.names = FALSE))[-1]
  
  
  # If nothing in files, skip
  if( length(completed) == 0 ) return( NULL )
  
  
  #loop and collect dir folders
  returnList<- c()
  for(item in completed){
    returnList <- c(returnList, item)
  }
  
  return( returnList )
} # END seekParent


# Returns if the projectName already exists in the folder structure
# < [ character ]
# > [ bool ]
# deps( data.table )
checkForCollision<- function(projectName){
  
  # Collect working  projects
  compileExpression<- expression( list.dirs(path = projectsDirectory, full.names = FALSE) )
  maxIndex<- length( temp<- sort(eval(compileExpression)))
  runningProjects<- temp[2:maxIndex]
  
  # Collect completed projects
  compileExpression<- expression( list.dirs(path = completeDirectory, full.names = FALSE) )
  maxIndex<- length( temp<- sort(eval(compileExpression)))
  completedProjects<- temp[2:maxIndex]
  
  # return results
  if(projectName %in% c(runningProjects, completedProjects)) return( TRUE )
  else return( FALSE )
} # END checkForCollision


# Creates and extracts uploaded data to project directory then deletes zip file
# > [ character ]
# < [ bool ]
# deps( Windows, 7za.exe)
zipExtract<- function( name, pathToFile ){

  # create and move file
  dir.create( name )
  
  file.copy( pathToFile, paste0( name, '/', name, '.zip' ))

  filename<- paste0(name, '.zip')

  setwd( name )
  unzip(zipfile = filename)

  
  # delete archive
  file.remove(filename)
}


# Creates the folder to finished Projects
# > [ character ]
# < [ bool ]
# deps( Windows, 7za.exe)
createComplete<- function(name){
  # Creates the folder so that DownloadHandler knows Project is done.
  dir.create( file.path(completeDirectory, name))
}


# Create the download handler for each finished project in the UI
# > [ character ]
# < [ downloadHandler ]
# deps( Shiny, Shinydashboard, Windows )
createDownloadHandler<- function( projectName ){
  
  # Move to directory to create files
  if( dir.exists( file.path( completeDirectory , projectName) )) setwd( file.path( completeDirectory , projectName) )
  else{ return( 'Removed')}
  
  # set filename to save as
  filename = paste0( projectName, '.zip')
  
  # Create contents for download
  content = function(file){
    
    # Collect where file will go and delete it if it already exists
    completedFile<- file.path( completeDirectory, projectName, paste0( projectName, '.zip'))
    if(file.exists( completedFile )) file.remove( completedFile )
    
    # Point to directory of files to zip
    completedFiles<- file.path( projectsDirectory, projectName, '*')
    
    # point to 7za cmd file and call function to create
    zipExe<- path7za
    zip(zipfile = completedFile, files = completedFiles, flags = 'a', zip = zipExe)
    
    # Move completed file to where Shiny will ACTUALLY download it
    file.copy(completedFile, file)
    
  }
  # Tell shiny it's a zip file.
  contentType = "application/zip"
  
  return( downloadHandler( filename, content, contentType ))
}

# function to call the project the user wants done.
# > [ character ]
# < [ ]
# deps( )
callFunction<- function(project, name){
  
  # DO NOT CHANGE THIS FUNCTION SO HELP ME GOD I WILL
  # MURDER YOUR WHOLE FAMILY TREE FROM NOW UNTIL ETERNITY
  
  # set up stats
  #loginfo(paste0( 'Starting Function call: ', project))
  
  index<- grep( project, isolate( global$tasks ))
  count<- length( taskCache<-isolate( global$taskTranslate )[[ index ]] )
  
  do.call( what = isolate( global$tasks)[[ index ]], args = list() )
}