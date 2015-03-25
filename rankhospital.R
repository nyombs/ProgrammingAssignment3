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
  #print(is.numeric(num))
  if(!is.numeric(num))
  {
    return(result[1,2])
  }
  if(nrow(result) > num)
  {
    return (result[num, 2])
  }
  if(nrow(result) < num)
  {
    spital <- NA
    #print(paste(spital))
    return(spital)
  }
}

# rankhospital("TX", "heart failure", 4)
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)
# 
# x <- 1:4
# if(nrow(x) - 6 == TRUE) return NA else { return 1}
