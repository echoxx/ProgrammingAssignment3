#WILL NEED TO REMOVE "INPUT_STATE" for final answer
rankall <- function( input_state, metric, num) {
  source <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  
  #Renames columns of interest (easier to work with)
  names(source)[c(2,7,11,17,23)] <- c("hospital", "state", "heart_attack", "heart_failure", "pneumonia")
  
  #Checks if valid state
  if ( all( input_state %in% source$state) == FALSE)  {
    print("invalid state")
    stop()
  } 
  
  #Checks if valid metric
  if ( metric %in% names(source)  == FALSE)  {
    print("invalid metric")
    stop()
  } 
  
  column_index <- numeric()
  if (metric == "heart_attack") {
    column_index <- 11
  } else if (metric == "heart_failure") {
    column_index <- 17
  } else if (metric == "pneumonia") {
    column_index <- 23
  } else { 
    print("invalid outcome")
    stop()
  }
  
  #Creates simplified df named "redux"
  redux <- data.frame(source$hospital, source$state, source[,column_index], stringsAsFactors = FALSE)
  names(redux) <- c("hospital", "state", "outcome")
  redux <- redux[complete.cases(redux),] #removes NAs
  
  #BELOW, WORK ONLY WITH "REDUX" NOT "SOURCE"
  splitstate <- split(redux, redux$state)  #returns a list of data frames
  
  #Maybe just use a for loop
  orderstateFN <- function(df) {
    for (i in seq_along(splitstate)) {
      orderOutcomeAlpha <- order( splitstate[[i]][,3] , splitstate[[i]][,1] )
      orderedstates <- splitstate[[i]][orderOutcomeAlpha,]
      
      if (i == 1) {
        results <- orderedstates[1,]
      } else {
        results <- rbind(results,orderedstates[1,])
      }
    }
    results
  }
    #Function that outputs 1st hospital in each state --> need to change to ranking
  
  

  
  
}