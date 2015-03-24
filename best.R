best <- function(state = character(2), outcome) {
  ## Read outcome data  
  ## Check that state and outcome are valid
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available" )
    if((outcome != "heart attack") &&  (outcome != "heart failure") && (outcome != "pneumonia"))
    {
      stop("invalid outcome")
    }
    if(nrow(subset(outcomeData, State == state)) == 0)
    {
      stop("invalid state")
    }
    index <- 0
    hospitalNameIndex <- 2
    if(outcome == "heart attack") index <- 11
    if(outcome == "heart failure") index <- 17
    if(outcome == "pneumonia") index <- 23
  outcomeData <- subset(outcomeData, State == state)
  m <- as.numeric(outcomeData[,index]) ## medical issues to monitor
  names <- outcomeData[, hospitalNameIndex]
  result <- outcomeData[order(m, names, na.last = NA), ] 
 # print(nrow(result))
  return(result[1,2])
}


# best("TX", "heart attack")
# 
# best("TX", "heart failure")
# 
# best("MD", "heart attack")
# 
# best("MD", "pneumonia")
# 
# best("BB", "heart attack")
# 
# best("NY", "hert attack")
# 
# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available" )
# outcome <- subset(outcome, State == "TX")
# max(outcome[, 17], na.rm = TRUE)
# subset(outcome[outcome[, 17] == max(outcome[, 17], na.rm = TRUE),], select = c(2, 17))
