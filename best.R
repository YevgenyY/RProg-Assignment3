best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # prepare data, format as numeric
  tmp <- data_num<-sapply(data[, c(11, 17, 23)], as.numeric)
  data_num<-cbind(data[,c(2,7)], tmp)
  
  # order alphabetically by Hospital name
  data_ord <- data_num[order(data_num[,1]),]
    
  ## Check that state and outcome are valid
  
  # Check outcome string
  if (outcome == "heart attack")
    colindex <- 3
  else if (outcome == "heart failure")
    colindex <- 4
  else if (outcome == "pneumonia")
    colindex <- 5
  else 
    stop("invalid outcome\n")
  
  # Check state
  if (! state %in% data_num[,2])
    stop("invalid state")

  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data_state <- data_ord[data_ord$State==state,]
  
  result <- data_state[ which.min(data_state[,colindex]), ]
  
  result[1]
  
}
