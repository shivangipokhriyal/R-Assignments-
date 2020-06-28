
pollutantmean <- function(directory="D:/Shivangi (WORK)/R (Coursera files)/specdata" , pollutant, id=1:332 ) {
    
    y <- paste(directory, "/" ,formatC(id, width=3, flag='0'), ".csv", sep="")

    for (i in seq_along(y)) {
    df<-data.frame(read.csv(y[i]))
    ## print(df)  
    }
    
    if(pollutant=='sulfate'){
    s<- df[,2]
    bads<- is.na(s)
    gs<-s[!bads]
    mean(gs)
    ## paste("Mean of Sulphate on selected monitors is:", mean(gs), sep=" ")
    } 
    else if (pollutant=='nitrate') {
    n<-df[,3]
    gn<-  n[!is.na(n)]
    mean(gn)
    paste("Mean of Nitrate on selected monitors is:", mean(gn), sep=" ")
    }
    else {
    ## print("The pollutant you have entered is  not available here.")
    }
}