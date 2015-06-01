best <- function(state, outcome) {
  ## Read outcome data is assumed for this exercise
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[, 11] <- as.numeric(outcome[, 11])
  outcome[, 17] <- as.numeric(outcome[, 17])
  outcome[, 23] <- as.numeric(outcome[, 23])

  
  ## Set valid outcomes
  volist <- list()
  volist[["heart attack"]] <- 11
  volist[["heart failure"]] <- 17
  volist[["pneumonia"]] <- 23  
  
  ## Check that state and outcome are valid
  if ( !state %in% state.abb ) {
    stop('invalid state')
  }
  
  if ( !outcome %in% names(volist) ) {
    stop('invalid outcome')
  }
    
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  # get the data for that state
  soutcome <- outcomedata[outcomedata$State==state,]
  
  # get the row which contains min of the outcome column for the desired outcome
  minrow <- soutcome[which.min(unlist(soutcome[unlist(volist[outcome])])),]
#  minrow <- soutcome[which.min(unlist(soutcome[volist[[outcome]])),]
  
  # return the hospital name
  as.character(minrow["Hospital.Name"])
}

