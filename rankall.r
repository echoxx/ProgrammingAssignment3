rankall <- function( metric, position) {
  source <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  
  #Renames columns of interest (easier to work with)
  names(source)[c(2,7,11,17,23)] <- c("hospital", "state", "heart_attack", "heart_failure", "pneumonia")
  
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
  
  #Generates data frame with appropriate rank
  for (i in seq_along(splitstate)) {
    orderOutcomeAlpha <- order( splitstate[[i]][,3] , splitstate[[i]][,1] )
    orderedstates <- splitstate[[i]][orderOutcomeAlpha,]
    
    #Sets "num" to rank, "best" or "worst"
    num <- numeric()
    if (position == "best") {
      num <- 1
    } else if (position == "worst") {
      num <- length( splitstate[[i]][,3])
    } else {
      num <- position
    }
    
    #Collates hospital ranks by state  
    if (i == 1) {
      results <- orderedstates[num,]
    } else {
      results <- rbind(results,orderedstates[num,])
    }
  }
return(results) 
}