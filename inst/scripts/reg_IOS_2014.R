library(readstata13)
library(ggplot2)


#Loading cleaned dataset and without missing values

dat <- read.dta13("D:/AFR_Database/SSAPOV-Harmonization/Muhsine/Maputo/IOS_survey_2014_forreg.dta")

class(dat)

View(dat)

#Keeping only variables of interest -> drop hid  

data <- subset(dat,select=c("ageyrs","ocusec","industrycat10","whours","socialsec"))


#How does R dummify
contrasts(data$ocusec)
contrasts(data$industrycat10)

#Splitting the data into two
result <- dat[1:46000,]
test <- dat[46001:46769,]

#Model
model <- glm(socialsec ~.,family=binomial(link='logit'),dat=result)

