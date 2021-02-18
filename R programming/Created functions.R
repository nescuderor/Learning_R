### Creación de función
##Primera -> Determinación de promedios tras el análisis de tablas en diferentes archivos
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

##Segunda -> Función de conteo de observaciones en diferentes archivos
complete <- function(directory = "specdata", id = 1:332) {
  #Stablishing the main directory
  setwd(paste("C:/Users/niesc/Downloads/rprog_data_specdata/", directory, sep = ""))
  
  #Local variables
  A <- data.frame()
  
  #Filling procedure
  if(length(id) >= 1 & length(id) < 333){
    for(i in id) {
      if(i < 1 | i > 332){
        print("Error, inserte el/ los monitor(es) a usar entre un rango de 1 a 332")
      }else if(i >= 1 & i < 10){
        x <- read.csv2(paste("00", i, ".csv", sep = ""), header = TRUE, sep = ",", dec = ".")
        x <- sum(complete.cases(x))
        A <- rbind(A, cbind(i, x) )
        
      }else if(i >= 10 & i <100){
        x <- read.csv2(paste("0", i, ".csv", sep = ""), header = TRUE, sep = ",", dec = ".")
        x <- sum(complete.cases(x))
        A <- rbind(A, cbind(i, x) )
        
      }else {
        x <- read.csv2(paste(i, ".csv", sep = ""), header = TRUE, sep = ",", dec = ".")
        x <- sum(complete.cases(x))
        A <- rbind(A, cbind(i, x) )
        
      }
    }
  } else {
    print("Error, inserte el/ los monitor(es) a usar entre un rango de 1 a 332")
  }
  colnames(A) <- c("id", "nobs")
  print(A)
}

##Tercera -> Determinación de correlaciones ante tablas que superen un umbral de observaciones
corr <- function(directory = "specdata", threshold = 0) {
  #Stablishing the main directory
  setwd(paste("C:/Users/niesc/Downloads/rprog_data_specdata/", directory, sep = ""))
  
  #Local variables
  Y <- vector(mode = "double")
  
  #Correlation vector
  for(i in 1:332) {
    if(i >= 1 & i < 10){
      x <- read.csv2(paste("00", i, ".csv", sep = ""), header = TRUE, sep = ",", dec = ".")
      x <- x[complete.cases(x) == TRUE, ]
      if(nrow(x) > threshold){
        Y <- append(Y, cor(x$sulfate, x$nitrate))
      }
      
    }else if(i >= 10 & i <100){
      x <- read.csv2(paste("0", i, ".csv", sep = ""), header = TRUE, sep = ",", dec = ".")
      x <- x[complete.cases(x) == TRUE, ]
      if(nrow(x) > threshold){
        Y <- append(Y, cor(x$sulfate, x$nitrate))
      }
      
    }else {
      x <- read.csv2(paste(i, ".csv", sep = ""), header = TRUE, sep = ",", dec = ".")
      x <- x[complete.cases(x) == TRUE, ]
      if(nrow(x) > threshold){
        Y <- append(Y, cor(x$sulfate, x$nitrate))
      }
    }
  }
  print(Y)
}