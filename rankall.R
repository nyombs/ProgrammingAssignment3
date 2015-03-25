rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available" )
  output <-  setNames(data.frame(matrix(nrow=length(num), ncol=2)), c("hospital", "state"))
  ## Check that state and outcome are valid
  if((outcome != "heart attack") &&  (outcome != "heart failure") && (outcome != "pneumonia"))
  {
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  
}