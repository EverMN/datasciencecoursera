pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## id interval validation
  if(min(id)<1 | max(id)>332) {
    return("Please select a valid ID")
  }
  ## pollutant validation
  if(length(pollutant)!=1 | (pollutant!="sulfate"&pollutant!="nitrate")) {
    return("Please review pollutant. Use sulfate or nitrate")
  }
  ## Reading only DESIRED files, not all the files in the directory
  for(integer in id) {
    ## Creating file paths
    file <- file.path(getwd(),
                      directory,
                      paste(
                        formatC(integer, 
                                width = 3, 
                                format = "d", 
                                flag = "0"
                                ),
                        ".csv",
                        sep=""),
                      fsep = .Platform$file.sep
                      )
    ## Getting data
    data <- read.csv(file)
    if (exists("x")){
      x <- c(x, data[,pollutant])   
    } else {
      x <- data[,pollutant]  
    }
  }
  ## Getting x mean (opt. nas <- is.na(x); x <- x[!nas]; mean(x))
  result <- mean(x, na.rm = TRUE)
  return(result)
}
