rankhospital <- function(state, outcome, num = 1) {
  ## Read outcome data
  
  outcome_temp <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
  
  fields <- c("Hospital.Name", 
              "State", 
              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
              "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  outcome_data <- outcome_temp[,fields]
  
  fieldNames <- c("hospital name",
                  "state",
                  "heart attack",
                  "heart failure",
                  "pneumonia")
  
  names(outcome_data) <- fieldNames
  
  ## Check that state and outcome are valid
  
  ## opcional states <- unique(outcome_data$State)
  ## if (all(state != states)){
  
  if (all(state != outcome_data$state)){
    stop("invalid state")
  }
  
  if (all(outcome != fieldNames[3:5])) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  outcome_state <- outcome_data[outcome_data$state==state,]
  
  na_filter <- !is.na(outcome_state[,which(fieldNames==outcome)])
  
  outcome_state <- outcome_state[na_filter,]
  
  sort <- order(outcome_state[,which(fieldNames==outcome)],outcome_state[,1]) 
  
  outcome_sorted <- outcome_state[sort,]
  
  if (num == "best") num <-1
  
  if (num == "worst") num <- nrow(outcome_sorted)
  
  if (num > nrow(outcome_sorted)) return(NA)
  
  x <- as.character(outcome_sorted[num,1])
  
  return(x)
}