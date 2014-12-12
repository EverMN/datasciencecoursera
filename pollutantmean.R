pollutantmean <- function(directory, pollutant, id = 1:322) {
    ## Valido que los IDs estén dentro del intervalo
  if(min(id)<1 | max(id)>322) {
    print("Please select a valid ID")
  }
   ## Valido pollutant
  if(length(pollutant)!=1 | (pollutant!="sulfate"&pollutant!="nitrate")) {
    print("Please review pollutant. Use sulfate or nitrate")
  }
  ## Creo x con NA
  x <- NA
  ## Inicio ciclo de lectura de archivos
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
    ## Obteniendo datos
    data <- read.csv(file)
    x <- c(x, data[,pollutant]) 
  }
  ## Calculando la media nas <- is.na(x); x <- x[!nas]
  result <- mean(x, na.rm = TRUE)
  return(result)
}
