rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
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
    mystatesdata <- mystatesdata[order(mystatesdata[,17],mystatesdata[,2]),]
    mystatesdata <- mystatesdata[complete.cases(mystatesdata[,c(2,17)]),] 
  }
  
  if (outcome == "heart attack" )
  {
    #print("outcome is pneumonia")
    ##outcome[, 11] <- as.numeric(outcome[, 11])
    mystatesdata[,11] <- as.numeric(mystatesdata[,11])
    mystatesdata <- mystatesdata[order(mystatesdata[,11],mystatesdata[,2]),]
    mystatesdata <- mystatesdata[complete.cases(mystatesdata[,c(2,11)]),] 
  }
  
  
  if (outcome == "pneumonia" )
  {
    #print("outcome is pneumonia")
    ##outcome[, 11] <- as.numeric(outcome[, 11])
    mystatesdata[,23] <- as.numeric(mystatesdata[,23])
    mystatesdata <- mystatesdata[order(mystatesdata[,23],mystatesdata[,2]),]
    mystatesdata <- mystatesdata[complete.cases(mystatesdata[,c(2,23)]),] #- will only select rows with complete data in columns 2, 3, and 5
      }
  
  
  ## got to check that num isnt bigger than mystatesdata
  if ( num == "best")
  {
    num = 1;
  }
  if (num == "worst")
  {
    #print ("worst")
    num = nrow(mystatesdata)
    #print(num)
  }

#  if (num > nrow(mystatesdata))
#  {
#    c <- NULL
#    stop(c)
#  }
    
  temp <- mystatesdata[num,]  
  c <- as.vector( temp[,2] )
  c
  
#mystatesdata    

  
}