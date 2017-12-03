# Author: Collin Mitchell
# Date: 2017/11/23
# Purpose: To login and read current Reporting tickets followed
#          by calculating how much work was done with them.


##-- Libraries
library( rvest )
library( stringr )
library( tidyverse )

URL <- '######################' # anonomized.


# load private data
load( "__data/credentials.R")



##-- Functions

# Return a session object
getSession<- function( urlTarget ){
  sessionObject <- html_session( urlTarget )
  sessionObject
}


# save credentials to a usable object
saveCredentials <- function(session, username, password){
  # credentials
  user = username
  pass = password
  
  # extract sesson form data.
  sessionForm<- html_form( session )[[1]]
  
  # update the form information
  completeForm<- set_values( sessionForm, "_login" = user, "_password" = pass)
  completeForm
}


# pass credentials to login
navLogin <- function( loginSession, creds){
  loginSession %>%
    submit_form( creds )
}

# navigate to "Change Columns"
navChangeColumns <- function( session ){
    session %>%
    follow_link( 'Reporting Tickets') %>%
    follow_link( 'Change Columns')
}


# fill in the correct form data for headers
correctForm <- function( loginSession, creds){
  # correct forms
  tempForm <- navLogin( loginSession, creds ) %>%
    navChangeColumns() %>%
    html_form()
  
  
  filledForm <- set_values(tempForm[[1]],
                           'column_priority' = TRUE,
                           'column_customer_temp' = TRUE,
                           'column_opendate' = TRUE,
                           'column_changeddate' = TRUE,
                           'column_rep_platform' = TRUE,
                           'column_assigned_to' = TRUE,
                           'column_level_2' = TRUE,
                           'column_reporter' = TRUE,
                           'column_bug_status' = TRUE,
                           'column_product' = TRUE,
                           'column_timezone' = TRUE,
                           'column_short_short_desc' = TRUE)
}

# login, download, and return a response
# object containing the data
collectReportingData <- function(loginSession, creds, filledForm){
  temp <- navLogin( loginSession, creds ) %>%
    navChangeColumns() %>%
    submit_form( filledForm ) %>%
    follow_link('REPORTING TICKETS') %>%
    follow_link('Export as CSV')
}

collectTicketComments <- function(loginSession, creds, callID){
  
  # get the form itself
  tempForm <- navLogin(loginSession, creds) %>%
    html_form()
  
  # extract the call form data and fill it.
  callLookupForm <- tempForm[9][[1]]
  callCompleteForm <- set_values(callLookupForm, id = callID)
  
  # collect all private comments
  tempComments <- navLogin(loginSession, creds) %>%
    submit_form( callCompleteForm ) %>%
    html_nodes( ".bz_private")
  
  # generate regex pattern to filter attachment comments.
  ptn <- or1("Added: ") %R%
    one_or_more( DGT ) %R%
    "-" %R%
    one_or_more( DGT ) %R%
    "-" %R%
    one_or_more( DGT ) %R%
    SPC %R%
    one_or_more( DGT ) %R%
    ":" %R%
    one_or_more( DGT ) %R%
    " PDT"
  
  # check for which comments are attachments.
  attachmentIndexes <- unlist( map( tempComments, function(.x){
    .x %>% xml_text() %>%
      str_detect( ptn )
  }))
  
  # subset out real comments
  tempComments <- tempComments[ !attachmentIndexes ]
  
  # generate tibble of comment data.
  commentText <- data_frame( text = unlist( map( tempComments, xml_text, trim=TRUE) ))
}


# return a tibble with the call,owner,ratio info.
genCommentRatio <- function( loginSession, creds, targetCall){
  # collect ticket comments
  ticketComments <- collectTicketComments(loginSession, creds, targetCall)
  
  # find the ticket owner
  ticketOwnerEmail <- data[ which( data$CallID == targetCall),]$Owner
  
  # how many of thee comments are the owners?
  ratio <- round( sum( unlist( map( ticketComments, str_detect, pattern = ticketOwnerEmail))) / nrow( ticketComments), 2)
  
  # create a resulting tibble
  data_frame( CalldID = targetCall, Owner = ticketOwnerEmail, Ratio = ratio)
}



##-- MAIN

# colelect a session
loginSession <- getSession( URL )

# collect the credential'd session to login.
creds <- saveCredentials( loginSession, username, password)

#  if something is wrong with credential's file, then
# create and submit new form data
if( !exists( 'filledForm') ) filledForm <- correctForm(loginSession, creds)

# collect the ARM CSV data:
reportingResponseData <- collectReportingData(loginSession, creds, filledForm)

# convert to tibble:
data <- reportingResponseData$response$content %>%
  rawToChar() %>%
  read_csv(col_names = TRUE)

# remove proserve duders
indexes <- which( data$`Assigned To` == "##################" | data$`Assigned To` == '###################') # anonymized
data <- data[-indexes,]

# rename column names to be easier to interpret
data <- rename( data, CallID = `Call ID`,
                      Owner = `Assigned To`,
                      Level2 = `Owner Level 2`,
                      Timezone = `Support Zone`)

Results <- map( 1:nrow( data ), function(.i){
  suppressMessages( genCommentRatio(loginSession, creds, data[.i,]$CallID) )
}) %>% bind_rows()


## Beyond this point is data expoloration
# =======================================


data %>%
  group_by(Owner) %>%
  summarize( count = n(), median_modified = median( Modified )) %>%
  arrange( desc( count ))


data %>%
  group_by(Owner) %>%
  summarize( count = n(), min_modified = min( Modified )) %>%
  arrange( desc( count ))

# save(Results, file="__data/Results.RData")
load('__data/Results.RData') # 2017/11/25


Results %>%
  group_by(Owner) %>%
  summarise( count = n(),
             minRatio = min(Ratio),
             medianRatio = median(Ratio),
             maxRatio = max(Ratio)) %>%
  arrange( desc(medianRatio))