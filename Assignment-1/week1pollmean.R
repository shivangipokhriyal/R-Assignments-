
pollutantmean <- function(directory="D:/Shivangi (WORK)/R (Coursera files)/specdata", pollutant, id=2 ) {
   
    data = numeric()
    for (i in id) {

        newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
            ".csv", sep = ""))

        data = c(data, newRead[[pollutant]])
    }
    return(mean(data, na.rm = TRUE))
}