best <- function(state, outcome) {
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #check if state is correct
  if (!(state %in% unique(outcome_data$State))) {
    stop("invalid state")
  }
  #check if outcome passed as an argument is correct. if yes, substract hospital name and outcome value
  if (outcome == "heart attack") {
    state_outcome_data <- outcome_data[outcome_data$State == state,c(2, 11)]
  } else if (outcome == "heart failure") {
    state_outcome_data <- outcome_data[outcome_data$State == state,c(2, 17)]
  } else if (outcome == "pneumonia") {
    state_outcome_data <- outcome_data[outcome_data$State == state,c(2, 23)]
  } else {
    stop("invalid outcome")
  }
  #remove non - numeric values
  state_outcome_data <- state_outcome_data[!is.na(suppressWarnings(as.numeric(state_outcome_data[,2]))),]
  #return row where second column value is minimal
  state_outcome_data[as.numeric(state_outcome_data[,2]) == min(as.numeric(state_outcome_data[,2])),1]
}