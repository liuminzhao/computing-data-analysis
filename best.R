best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!state %in% data$State) {
    stop("invalid state")
  }
  data <- data[data$State==state,]
  if (outcome == 'heart attack') {
    death <- as.numeric(data[,11])
  } else if (outcome == 'heart failure') {
    death <- as.numeric(data[,17])
  } else if (outcome == 'pneumonia') {
    death <- as.numeric(data[,23])
  } else {
    stop("invalid outcome")
  }
  

  index <- which(death == min(death, na.rm=T))

  return(data$Hospital.Name[index])
}

