rankhospital <- function(input_state, metric, num) {
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
    
    #Chooses column based on metric request
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
    inputstate <- splitstate[[input_state]] ##need to use double brackets to return df instead of list
    
    #creates vector with correct metric order
    ## orders 1st by metric in col 3, then by alphabetical in col 1
    orderinputstate_vector <- order(inputstate[,3], inputstate[,1]) 
    orderinputstate <- inputstate[orderinputstate_vector, ]
    
    #Returns NA if "num" greater than # of hospitals in list
    if (!is.character(num) && num >= length(orderinputstate[,1])) {
      return(NA)
      stop()
    } else if (num == "best") {
      return(orderinputstate[1,1])
    } else if (num == "worst") {
      statecount <- length(orderinputstate[,1])
      return(orderinputstate[statecount,1])
    } else {
      return(orderinputstate[num,1])
    }
  
}