best <- function(state, outcome) {
  ## Read outcome data
  dataOutcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validStates <- unique(dataOutcome$State)
  validOutcomes <- c("heart attack","heart failure","pneumonia")
  if(!state %in% validStates) stop("invalid state")
  if(!outcome %in% validOutcomes) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate
  colNbOutcome = c(11,17,23)
  names(colNbOutcome) <- validOutcomes
  stateData = dataOutcome[dataOutcome$State==state,]
  selectedOutcome = as.numeric(stateData[,colNbOutcome[outcome]])
  minOutCOme = min(selectedOutcome,na.rm = TRUE)
  best = stateData[selectedOutcome==minOutCOme,2]
  sort(best)[1]
  
  
}
