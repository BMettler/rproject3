rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  
  outcome2 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Return hospital name in that state with the given rank
  mylist = c("heart failure","heart attack","pneumonia")
  
  if ( !(outcome %in% mylist) )
  {
    ##print("invalid outcome")
    stop("invalid outcome")
  }

  number = num
  death = outcome
  
  ## Check that state and outcome are valid
  statelist <- split(outcome2, outcome2$State)
  
  #apply(statelist, , ...)
  #mystatesdata <- statelist[[state]]
  

  
  getmystuff <- function(dataframe)
  {
    if ( number == "best")
    {
      number = 1;
    }
    if (number == "worst")
    {
      #print ("worst")
      number = nrow(dataframe)
      #print(num)
    }
    
    
    if (death == "heart failure" )
    {
      #print("outcome is pneumonia")
      ##outcome[, 11] <- as.numeric(outcome[, 11])
      dataframe[,17] <- as.numeric(dataframe[,17])
      dataframe <- dataframe[order(dataframe[,17],dataframe[,2]),]
      #dataframe <- dataframe[complete.cases(dataframe[,c(2,17)]),] 
    }
    
    if (death == "heart attack" )
    {
      #print("outcome is pneumonia")
      ##outcome[, 11] <- as.numeric(outcome[, 11])
      dataframe[,11] <- as.numeric(dataframe[,11])
      dataframe <- dataframe[order(dataframe[,11],dataframe[,2]),]
      #dataframe <- dataframe[complete.cases(dataframe[,c(2,11)]),] 
    }
    
    
    if (death == "pneumonia" )
    {
      #print("outcome is pneumonia")
      ##outcome[, 11] <- as.numeric(outcome[, 11])
      dataframe[,23] <- as.numeric(dataframe[,23])
      dataframe <- dataframe[order(dataframe[,23],dataframe[,2]),]
      #dataframe <- dataframe[complete.cases(dataframe[,c(2,23)]),] #- will only select rows with complete data in columns 2, 3, and 5
    }
    
    
    temp <- dataframe[number,]  
    temp2 <- dataframe[1,]
    c <- c( temp[,2], temp2[,7])
    c
    
  }
  
  
  foo <- data.frame(do.call(rbind, lapply(statelist,getmystuff)))
  #foo <- data.frame(lapply(statelist,getmystuff))
  
  names(foo) <-c("hospital","state")
  foo
  
  
  
  #so we have a list of states.. for each state, find outcome, and rank/num
    # For each state, find the hospital of the given rank

  
  
    ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}