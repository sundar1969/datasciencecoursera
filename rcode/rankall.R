rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Set valid outcomes
  volist <- list()
  volist[["heart attack"]] <- 11
  volist[["heart failure"]] <- 17
  volist[["pneumonia"]] <- 23
  
  ## Check that state and outcome are valid
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
  
  # Find all the unique states in the dataframe
  # df[!duplicated(df), ]
  thestates <- outcomedata["State"][!duplicated(outcomedata["State"]),]
  #thestates <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL")  

  # Initialize a DataFrame with 2 cols and as many rows as there are unique states in the input dataframe
  df <- data.frame(hospital=character(length(thestates)), 
                   state=character(2),
                   stringsAsFactors=FALSE)
    
  ## For each state, find the hospital of the given rank
  i <- 0
  for (s in thestates) {
    i <- i+1
    # get the data for that state
    soutcome <- outcomedata[outcomedata$State==s,]

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
  
    # Add the state name
    df$state[i] <- s
    if (nrow(nacleansed) >= n) {
      # sort by outcome and by hospital name for tie breaking
      hsorted <- nacleansed[order(as.numeric(nacleansed[,oc]), nacleansed[,"Hospital.Name"]),]      
      row <- hsorted[n,]
      hn <- as.character(row["Hospital.Name"])      
      
      # Add the hospital name
      df$hospital[i] <- hn
    } else { # Not enough rows to compute the rank required
      hn <- as.character("<NA>")
      df$hospital[i] <- hn
    }
  }
  
  ## Return a data frame with the hospital names and the  
  ## (abbreviated) state name
  df[order(as.character(df[,"state"])),]
}