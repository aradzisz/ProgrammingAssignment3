rankhospital <- function(state, outcome, num = "best") {
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #check if state is correct
  if (!(state %in% unique(outcome_data$State))) {
    stop("invalid state")
  }
  #check if outcome passed as an argument is correct. if yes, subset hospital name and outcome value
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
  if (num != "best" && 
      num != "worst" && 
      as.numeric(num) > nrow(state_outcome_data)) {
    return(NA)
  }
  #sort data properly
  if (num != "worst") {
    state_outcome_data <- state_outcome_data[order(as.numeric(state_outcome_data[,2]), state_outcome_data[,1], decreasing = FALSE),]
    if (num == "best") {
      hospital <- state_outcome_data[1,1]
    } 
    else {
      hospital <- state_outcome_data[num,1]
    }
    }
  else if (num == "worst") {
    state_outcome_data <- state_outcome_data[order(as.numeric(state_outcome_data[,2]), rev(state_outcome_data[,1]), decreasing = TRUE),]
    hospital <- state_outcome_data[1,1]
  }
  hospital
}
