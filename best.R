best <- function(state, outcome) {
  ## Read outcome data
  outcome2 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  statelist <- split(outcome2, outcome2$State)
  
  mystatesdata <- statelist[[state]]
  
  ## if statelist contains data, we are valid. 
  
  if (is.null(statelist[[state]]) )
  {
    ##print("invalid state")
    stop("invalid state")
  }
  
  
  
  ## outcomes 
  ## 17 - "heart failure"
  ## 11 - "heart attack"
  ## 23 - "pneumonia"
  mylist = c("heart failure","heart attack","pneumonia")
  
  if ( !(outcome %in% mylist) )
  {
    ##print("invalid outcome")
    stop("invalid outcome")
  }

  if (outcome == "heart failure" )
  {
    #print("outcome is pneumonia")
    ##outcome[, 11] <- as.numeric(outcome[, 11])
    mystatesdata[,17] <- as.numeric(mystatesdata[,17])
    mystatesdata <- mystatesdata[order(mystatesdata[,17]),]
  }

  if (outcome == "heart attack" )
  {
    #print("outcome is pneumonia")
    ##outcome[, 11] <- as.numeric(outcome[, 11])
    mystatesdata[,11] <- as.numeric(mystatesdata[,11])
    mystatesdata <- mystatesdata[order(mystatesdata[,11]),]
  }
  
    
  if (outcome == "pneumonia" )
  {
    #print("outcome is pneumonia")
    ##outcome[, 11] <- as.numeric(outcome[, 11])
    mystatesdata[,23] <- as.numeric(mystatesdata[,23])
    mystatesdata <- mystatesdata[order(mystatesdata[,23]),]
  }

  ## Return hospital name in that state with lowest 30-day death
  ## rate
 
  #head(mystatesdata) 
  
  
  temp <- mystatesdata[1,]  
  c <- as.vector( temp[,2] )
  c
  
}