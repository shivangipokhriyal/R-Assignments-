rankhospital <- function(state, outcome, num="best" ) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  data <- read.csv('outcome-of-care-measures.csv')
  test<- c("best","worst",1:4706)
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    sprintf("error in best(%s,%s): invalid outcome", state,outcome)
  }
  
  else if(!state %in% data[,'State']){
    sprintf("error in best(%s,%s): invalid state", state,outcome)
  }
  
  else if(!num %in% as.character(test)) {
    print("NA")
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
    arr_list <- df[order(df[,3]),3]
    
    if(num=="best"){
      arr <- arr_list[1]
    }
    else if(num=="worst"){
      arr <- arr_list[length(arr_list)]
    }
    else {
      arr <- arr_list[as.numeric(num)]
      
    }
    
    hospital_name <- df[df[,3]==arr,]
    hospital_name <- hospital_name[order(hospital_name[,1]),1]
    return(hospital_name[1,1])
  
  
  
  
  }
  

}