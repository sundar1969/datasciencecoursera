rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  
  ## assign oc string for future use in sorting etc
  oc <- ""
  if (outcome == "heart attack") {
    oc <- as.character("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
  } else if (outcome == "heart failure") {
    oc <- as.character("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
  } else if (outcome == "pneumonia") {
    oc <- as.character("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  }
  
  ## Return hospital name in that state with the given rank  
  ## 30-day death rate
  
  # get the data for that state
  soutcome <- outcomedata[outcomedata$State==state,]
  row <- list()
    
  #get rows which have data for desired outcome, with na removed
  soutcomereduced <- soutcome[c("Hospital.Name",oc)]
  nacleansed <- soutcomereduced[soutcomereduced[oc] != "Not Available",]
    
  # assign n correctly
  n <- 0
  if (num == "best") {
    n <- 1
  }
  else if (num == "worst") {
    n <- nrow(nacleansed)
  }
  else {
    n = as.numeric(num)
  }
  
  #message("n is assigned ", n)
  #message("number of rows available is ", nrow(nacleansed))
  
  if (nrow(nacleansed) >= n) {
    #message("Well behaved n and nacleansed")
    
    # defer sorted to last possible stage so we don't do it needlessly.
    # sort by outcome and by hospital name for tie breaking
    hsorted <- nacleansed[order(as.numeric(nacleansed[,oc]), nacleansed[,"Hospital.Name"]),]
    #message("Number of sorted hospitals is ", nrow(hsorted) )
    
    row <- hsorted[n,]
    #message("row is ", row)
    
    # return the hospital name
    as.character(row["Hospital.Name"])
  } else { # Not enough rows to compute the rank required
    return(NA)
  }
}  