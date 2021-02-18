##############################First assignment##################################

################################################################################
##First part
pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
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

################################################################################
##Second part
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

complete("specdata", 3)

################################################################################
##Third part
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

cr <- corr("specdata", 150)
head(cr)
summary(cr)

###Assignment testing
#Polluntant mean function
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

#Complete function
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

#Corr function
cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
print(out)
