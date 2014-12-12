complete <- function(directory, id = 1:322) {
  ## id interval validationo
  if(min(id)<1 | max(id)>332) {
    return("Please select a valid ID")
  }
  ## Reading only the DESIRED files, not all the files in the directory
  for(integer in id) {
    ## Creando ruta de archivo -> file
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
    data_temp <- read.csv(file)  
    file_id <- integer
    ## Building complete obs filter
    nas_field1 <- is.na(data_temp[,1])
    nas_field2 <- is.na(data_temp[,2])
    nas_field3 <- is.na(data_temp[,3])
    nas_comb <- 0 == (nas_field1+
                   nas_field2+
                   nas_field3)
    nobs <- sum(nas_comb)
    ## Building data frame
    if (exists("df_nobs")) {
      tmp_df_nobs <- data.frame(file_id, 
                                nobs)
      df_nobs <- rbind(df_nobs, 
                       tmp_df_nobs)
      rm(tmp_df_nobs)
    } else {
      df_nobs <- data.frame(file_id, 
                            nobs)
    }
  }
  colnames(df_nobs) <- c("id", "nobs")
  return(df_nobs)
}