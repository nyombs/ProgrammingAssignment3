rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available" )
  
  ## Check that state and outcome are valid
  if((outcome != "heart attack") &&  (outcome != "heart failure") && (outcome != "pneumonia"))
  {
    stop("invalid outcome")
  }
  ##num can be "best", "worst" or a number
  if (num != "best" && num != "worst"){
    if (!is.numeric(num)) stop("invalid rank")
  }
  
  if(nrow(subset(outcomeData, State == state)) == 0)
  {
    stop("invalid state")
  }
  desc <- FALSE #default value for sorting is in descending = FALSE (less mortality is good :) ) 
  if(num == "best")  { desc <- FALSE  }
  if(num == "worst")  { desc <- TRUE  }

  index <- 0
  hospitalNameIndex <- 2
  if(outcome == "heart attack") index <- 11
  if(outcome == "heart failure") index <- 17
  if(outcome == "pneumonia") index <- 23
  outcomeData <- subset(outcomeData, State == state)
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcomeData <- subset(outcomeData, State == state)
  m <- as.numeric(outcomeData[,index]) ## medical issues to monitor
  names <- outcomeData[, hospitalNameIndex]
  result <- outcomeData[order(m, names, decreasing = desc, na.last = NA), ] 
  print(is.numeric(num))
  if(is.numeric(num))
  {
    rank = as.numeric(num)
    count = as.numeric(nrow(result))
    print(class(count))
    print (class(rank))
    value <- rank - count == 0
    print(class(value))
    if(value == TRUE)  { return NA } 
    else 
    {
      return (result[rank, 2])
    }
  }
  else
  {
    return(result[1,2])
  }
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
x <- 1:4
if(nrow(x) - 6 == TRUE) return NA else { return 1}
