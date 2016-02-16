rankall <- function(outcome, num = "best") {
  ## Read outcome data
  dataOutcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings= "Not Available",stringsAsFactors=FALSE)
  ## Check that state and outcome are valid
  states <- unique(dataOutcome$State)
  validOutcomes <- c("heart attack","heart failure","pneumonia")
  if(!outcome %in% validOutcomes) stop("invalid outcome")
  colNbOutcome = c(11,17,23)
  names(colNbOutcome) <- validOutcomes
  outcomeColIndex <- colNbOutcome[outcome]
  ## For each state, find the hospital of the given rank
  findHospitalName <- function(stateData) {
    #Order the data by outcome first and then by hospital name
    index <- order(as.numeric(stateData[,outcomeColIndex]),stateData[,2])
    stateData <- stateData[index,]
    stateData[,outcomeColIndex] <- as.numeric(stateData[,outcomeColIndex])
    stateData <- stateData[!is.na(stateData[,outcomeColIndex]),]
    
    if(num=="best") {num <- 1}
    else if(num=="worst") {num <- nrow(stateData)}
    stateData[num,2]
  }
  stateList <- split(dataOutcome,dataOutcome$State)
  stateList <- sapply(stateList, findHospitalName )
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital=stateList,state=names(stateList))

}
