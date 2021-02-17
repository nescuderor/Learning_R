###First assignment
##First part
pollutantmean <- function(directory = "specdata", pollutant, id) {
  #Stablishing the main directory
  setwd(paste("C:/Users/niesc/Downloads/rprog_data_specdata/", directory, sep = ""))
  
  #Local variables
  M <- data.frame()
  
  #Mean procedure
  if(length(id) >= 1 & length(id) < 333){
    for(i in id) {
      if(i < 1 | i > 332){
        print("Error, inserte el/ los monitor(es) a usar entre un rango de 1 a 332")
      }else if(i >= 1 & i < 10){
        x <- read.csv2(paste("00", i, ".csv", sep = ""), header = TRUE, sep = ",", dec = ".")
        M <- rbind(M, x)
        
      }else if(i >= 10 & i <100){
        x <- read.csv2(paste("0", i, ".csv", sep = ""), header = TRUE, sep = ",", dec = ".")
        M <- rbind(M, x)
        
      }else {
        x <- read.csv2(paste(i, ".csv", sep = ""), header = TRUE, sep = ",", dec = ".")
        M <- rbind(M, x)
        
      }
    }
  } else {
    print("Error, inserte el/ los monitor(es) a usar entre un rango de 1 a 332")
  }
  print(mean(M[, pollutant], na.rm = T))
}

pollutantmean("specdata", "nitrate", 1:332)



