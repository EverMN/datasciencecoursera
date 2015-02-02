outcome <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", colClasses = "character")
head(outcome)
outcome_names <- names(outcome)
temp <- outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
temp_na <- is.na(temp)
mean(temp_na)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

state <- "TX"
any(state == outcome_data$State)
all(state != outcome_data$State)
all(state != states)


# D U M M Y S ------------------------------------------------------------------
outcome <- "pneumonia"
num <- "worst"
state <- "NJ"
character <- "NJ"
