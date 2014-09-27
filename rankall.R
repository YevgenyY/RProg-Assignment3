rankall <- function(outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  # data_state <- data_ord[data_ord$State==state,]
  list_state <- split(data_ord, data_ord$State)
  state_names <- names(list_state)
  
  result <- matrix(0, nrow=0, ncol=2)
  for (i in 1:length(list_state))
  {
    tmp <- list_state[[i]]
    if (num == "best")
      result <- rbind(result, tmp[ which.min(tmp[,colindex]), c("Hospital.Name", "State")])  
    else if (num == "worst")
      result <- rbind(result, tmp[ which.max(tmp[,colindex]), c("Hospital.Name", "State")])  
    else
    {
      #tmp2 <- rbind(result, tmp[ order(tmp[,colindex]), c("Hospital.Name", "State")])  
      #tmp <- data_state[order(data_state[, colindex]), c("Hospital.Name", "State")]
      tmp2 <- tmp[ order(tmp[,colindex]), c("Hospital.Name", "State")]
      result <- rbind(result,tmp2[num,])
    }
  }
  names(result) <- c("hospital", "state")
  result    
}
