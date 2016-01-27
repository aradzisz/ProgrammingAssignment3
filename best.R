best <- function(state, outcome) {
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!(state %in% unique(outcome_data$State))) {
    stop("invalid state")
  }
  if (outcome == "heart attack") {
    state_outcome_data <- outcome_data[outcome_data$State == state,c(2, 11)]
  } else if (outcome == "heart failure") {
    state_outcome_data <- outcome_data[outcome_data$State == state,c(2, 17)]
  } else if (outcome == "pneumonia") {
    state_outcome_data <- outcome_data[outcome_data$State == state,c(2, 23)]
  } else {
    stop("invalid outcome")
  }
  state_outcome_data[state_outcome_data[,2] == min(state_outcome_data[,2], na.rm = TRUE),1]
}