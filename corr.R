corr <- function(directory, threshold = 0) {
  ## Numeric vector of lenght 0
  z <- numeric()
  ## Getting file routes
  files <- list.files(file.path(getwd(),
                                directory,
                                fsep = .Platform$file.sep
                                )
                      )
  
  for (file in files) {
    ## Getting ALL the files in the selected directory
    data_temp <- read.csv(file.path(getwd(),
                                    directory,
                                    file,
                                    fsep = .Platform$file.sep
                                    )
    )  
    file_id <- file
    ## Filtering complete observations
    nas_field1 <- is.na(data_temp[,1])
    nas_field2 <- is.na(data_temp[,2])
    nas_field3 <- is.na(data_temp[,3])
    nas_comb <- 0 == (nas_field1+
                        nas_field2+
                        nas_field3)
    ## Threshold validation and vector of corr.
    if (sum(nas_comb) > threshold) {
      data_temp <- data_temp[nas_comb,] 
        z <- c(z, cor(data_temp$sulfate, data_temp$nitrate))
    }
  }
  return(z)
}