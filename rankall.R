rankall <- function(outcome, num = "best") {
  outcome_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  #check if outcome passed as an argument is correct. if yes, subset hospital name, state and outcome value
  if (outcome == "heart attack") {
    outcome_data <- outcome_data[,c(2, 7, 11)]
  } else if (outcome == "heart failure") {
    
    outcome_data <- outcome_data[,c(2, 7, 17)]
  } else if (outcome == "pneumonia") {
    outcome_data <- outcome_data[,c(2, 7, 23)]
  } else {
    stop("invalid outcome")
  }
  
  #remove NAs
  outcome_data <- outcome_data[!is.na(outcome_data[,3]),]
  names(outcome_data) <- c("hospital", "state", "outcome")
  #sort data properly
  sorted_outcome_data <- outcome_data[order(outcome_data$state, as.numeric(outcome_data$outcome), outcome_data$hospital),]
  sorted_outcome_data <- split(sorted_outcome_data, sorted_outcome_data$state)
  #create vector with hospital names
  hospital_vector <- unlist(lapply(sorted_outcome_data, function(x) {
    if (num == "best") {
      x[[1]][1]
      }
    else if (num == "worst") {
      x[[1]][nrow(x[1])]
      }
    else {
      x[[1]][num]
    }
  }
    )
  )
  #create vector with states
  state_vector <- unlist(lapply(names(sorted_outcome_data), function(x) x))
  data.frame(hospital = hospital_vector, state =state_vector)
}