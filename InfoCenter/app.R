# Author: Collin Mitchell 
# Date: Edited 2017/12/3

##-- load dependencies
source('libraries.R')
source('basefunctions.R')
source('supportfunctions.R')

# check to see if log file aleady exists
if( file.exists('infoCenter.log')){
  file.copy('infoCenter.log', 'Logs')
  file.remove('infoCenter.log')
}

basicConfig()
addHandler(writeToFile, file = 'infoCenter.log')

ROOT<- getwd()
path7za<- file.path( ROOT, '7za', '7za.exe')
completeDirectory<- file.path( ROOT, 'Completed')
projectsDirectory<- file.path( ROOT, 'Projects')

global<- reactiveValues( completed = seekCompleted(), projects = seekProjects(),
           tasks = c('Log File Errors', 'Potential Duplicates', "Check Report V2"),
           taskTranslate = list( 'callLogParser', 'detectDuplicates', 'checkReportingV2File'))

###
# Logging section
###
loginfo(paste0( "Root directory: ", ROOT))
loginfo(paste0( "Completed Directory: ", completeDirectory))
loginfo(paste0( "Projects Directory: ", projectsDirectory))
####


# splitting up pieces for simplicity
header <- dashboardHeader(title = 'Info Center', titleWidth = 250)

# SIDEBAR
sidebar<- dashboardSidebar(
  sidebarMenu(
    menuItem('Potential Jobs', tabName = 'jobsPage', icon = icon('cloud-upload')),
    menuItem('Finished Jobs', tabName = 'finishedJobs', icon = icon('arrow-down')),
    tags$hr(),
    textInput('projectName', label = NULL, placeholder = 'Project Name'),
    fileInput('downloadFile', 'Upload Files:', accept = 'text/csv'),
    tags$hr(),
    actionButton('dataUpload', "Upload Data", style = "margin: 1px 1px 1px 50px;"),
    textOutput('confirmSubmit'), 
    tags$hr(),
    actionButton('updateInformation', "Update UI", style = "margin: 1px 1px 1px 56px;"),
    tags$hr()
  )
)

#BODY
body   <- dashboardBody(
  tags$head(tags$style(HTML('.tab-pane{ height: 1000 px !important;}'))),
  tabItems(
    tabItem(
      tabName = 'jobsPage',
      div(h2("Select a Job Type:")),
      HTML('<div class="shiny-input-radiogroup" id="var">'),
      lapply(1:length(isolate(global$tasks)), function(i){
        HTML(paste0('<div class="shiny-html-output col-sm-6" id="job', i, '"></div>', collapse = ''))
      }),
      HTML('</div>')
    ),
    tabItem(
      tabName = 'finishedJobs',
      # prepare for REACTIVE context on the server
      htmlOutput('downloadBoxes', class="col-sm-12")
    )
  )
)

# compile it all together
ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) {

  # Create Potential jobs
  lapply(1:length(isolate(global$tasks )), function(i){
    output[[ paste0('job', i) ]] <- assign( paste0('value', isolate(global$tasks[ i ] )),
                                            renderValueBox({ valueBox(
                                              HTML(sprintf('<input type="radio" name="var" id="%s" value="%s"/><span>%s</span><br />',
                                                           isolate(global$tasks[ i ]), isolate(global$tasks[ i ]), isolate(global$tasks[ i ] ))),
                                              isolate(global$tasks[ i ]), icon = icon('folder-open'), color = 'olive' )
                                            }))
  }) #END potential jobs
  
  output$downloadBoxes <- renderUI({
    if( !is.null( global$completed )){
      lapply(1:length( global$completed ), function(i){
        output [[ paste0( 'download', global$completed[ i ]) ]] <- assign( paste0( 'value', global$completed[ i ] ),
                                                                          createDownloadHandler( global$completed[ i ] ))
        output[[ paste0( 'completedJob',i ) ]] <- assign( paste0('value', global$completed[ i ] ),
                                                          renderValueBox({
                                                            div(class = "shiny-html-output col-sm-6", valueBox( downloadButton( paste0( 'download', global$completed[ i ] ), 'Download'),
                                                                global$completed[ i ], icon = icon('check'), color = 'blue' )
                                                  )}))
      })
    }
  })
  
  # If the user requests an update to the page
  observeEvent( input$updateInformation, {

    
    temp<-removeOldProjects()
    global$completed <- seekCompleted()
    output$confirmSubmit <- renderText('')

      
    output$downloadBoxes<- renderUI({
    if( !is.null( global$completed )){
      lapply(1:length( global$completed ), function(i){
        output[[ paste0( 'download', global$completed[ i ]) ]] <- assign( paste0( 'value', global$completed[ i ] ),
                                                                          createDownloadHandler( global$completed[ i ] ))
        output[[ paste0( 'completedJob',i ) ]] <- assign( paste0('value', global$completed[ i ] ),
                                                          renderValueBox({
                                                            div(class = "shiny-html-output col-sm-4",valueBox( downloadButton( paste0( 'download', global$completed[ i ] ), "Download"),
                                                                global$completed[ i ], icon = icon('check'), color = 'blue' )
                                                   )}))
        })
    }})
  })
  
  

  
  observeEvent(input$dataUpload, {
    # Check for collisions, input$var selected, sanitize inputs
    
    # initial project creation
    projectName<- input$projectName
    if( !checkForCollision( projectName )){
    
      setwd(projectsDirectory)
      zipExtract( projectName, input$downloadFile$datapath)
      output$confirmSubmit<- renderText("Submit Confirmed")
    
      ###
      # Logging Section
      ###
      loginfo(paste0( 'Project Name: ', projectName))
      loginfo(paste0( 'Current directory: ', getwd()))
      ###
    
      # Call function to run project
      callFunction(input$var, global$name )
  
      # project completed
      createComplete( projectName )
    }
    else{
      output$confirmSubmit<- renderText("Submit Rejected; Project Name already exists.")
    }
    
    ###
    # Logging Section
    ###
    loginfo(paste0( 'Project ', projectName, ' completed.'))
    ###
  })
  
  
  
}


shinyApp(ui, server, options = list( port = 5050, host= '0.0.0.0'))



