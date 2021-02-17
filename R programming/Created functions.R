### Creación de función
pollutantmean <- function(directory = "specdata", pollutant, id) {
  setwd(paste("C:/Users/niesc/Downloads/rprog_data_specdata/", directory, sep = ""))
  for(i in id) {
    if(i < 1 | i > 332){
      print("Error, inserte el/ los monitor(es) a usar entre un rango de 0 a 332")
    }else if(i >= 1 & i < 10){
      print(paste("00", i, ".csv", sep = ""))
    }else if(i >= 10 & i <100){
      print(paste("0", i, ".csv", sep = ""))
    }else {
      print(paste(i, ".csv", sep = ""))
    }
  }
}

pollutantmean(id = 1:333)
