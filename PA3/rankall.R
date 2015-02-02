rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  outcome_temp <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
  
  fields <- c("Hospital.Name", 
              "State", 
              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
              "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )

  fieldNames <- c("hospital",
                  "state",
                  "heart attack",
                  "heart failure",
                  "pneumonia")
  
  outcome_data <- outcome_temp[,fields]
  
  names(outcome_data) <- fieldNames
  
  ## Check that state and outcome are valid
  
  if (all(outcome != fieldNames[3:5])) {
    stop("invalid outcome")
  }
  
# LOOP - STATE -----------------------------------------------------------------

  states <- as.character(unique(outcome_data[,2]))
  states <- as.character(states[order(states)])

  for (character in states) {
    i <- 1
    outcome_state <- outcome_data[outcome_data$state==character,]
    n <- num
    if (n == "best") n <- 1
    if (n == "worst") n <- nrow(outcome_state)
    if (all(character != unique(outcome_state$state))) i <- NA
    if (n > nrow(outcome_state)) i <- NA
    
    sort <- order(outcome_state[,which(fieldNames==outcome)],outcome_state[,1])
    
    if(!exists("outcome_sorted")) {
      if (!is.na(i)) {
        outcome_sorted <- outcome_state[sort,c(1,2)]
        outcome_sorted <- outcome_sorted[n,]  
      } else {
        hospital <- NA
        state <- character
        outcome_sorted <- data.frame(hospital, state)
      }
    } else {
      if  (!is.na(i)) {
        outcome_sorted_temp <- outcome_state[sort,c(1,2)]
        outcome_sorted_temp <- outcome_sorted_temp[n,]
      } else {
        hospital <- NA
        state <- character
        outcome_sorted_temp <- data.frame(hospital, state)
      }
      outcome_sorted <- rbind(outcome_sorted, outcome_sorted_temp)
      rm("outcome_sorted_temp")
    }
    rm(outcome_state)
  }

  x <- data.frame(outcome_sorted[,1:2])
  rownames(x) <- states

  return(x)
}