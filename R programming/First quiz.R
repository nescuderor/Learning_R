###Solution
##Getting the names of each column
x01 <- read.csv2("hw1_data.csv", header=T,sep = ",") #Before this, we have to set where is the file using the commands getwd(), setwd() and dir().
names(x01)
# View(x01)

##Extracting the first two rows
head(x01, n = 2L)
x01[1:2,]

##Getting the dimensions
dim(x01)
nrow(x01)

##Extracting the last two rows
tail(x01, n = 2L)

##Extracting an specific value
x01$Ozone[47]

##Calculating the missing values
sum(is.na(x01$Ozone))

##Calculating the mean without missing values
x02 <- x01$Ozone[is.na(x01$Ozone) == FALSE]
mean(x02)

##Subsetting and establishing personalized operations
#First exercise
#First option
x03 <- x01[(x01$Ozone > 31) & (x01$Temp > 90), ]
x03 <- x03[complete.cases(x03) == T, ]
mean(x03$Solar.R)

#Alternative option
x04 <- x01[((x01$Ozone > 31) & (is.na(x01$Ozone) == F)) & ((x01$Temp > 90) & (is.na(x01$Temp) == F)), ]
mean(x04$Solar.R)

#Second exercise
mean(x01$Temp[(x01$Month == 6) & ((is.na(x01$Month) == F) & (is.na(x01$Temp) == F))])

##Determining the maximum value of a time line
x05 <- x01[(x01$Month == 5),]
max(x05$Ozone, na.rm = TRUE)