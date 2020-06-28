best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate

  data <- read.csv('outcome-of-care-measures.csv')
  
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    sprintf("error in best(%s,%s): invalid outcome", state,outcome)
  }
  
  else if(!state %in% data[,'State']){
    sprintf("error in best(%s,%s): invalid state", state,outcome)
  }
  
  else {
    if(outcome == 'heart attack'){
      df <- data[,c(2,7,11)]
      
    }
    else if(outcome == 'heart failure'){
      df <- data[,c(2,7,17)]
      
    }
    else {
      df <- data[,c(2,7,23)]
      
    }
    
    df <- df[df['State']==state,]
    df <- df[!df[3]=='Not Available',]
    df[,3] <- as.numeric(df[,3])
    l_min <- lapply(df[3], min)
    hospital_name <- df[df[3]==l_min,]
    return(hospital_name$Hospital.Name)
    
    
  }
  
}
  
