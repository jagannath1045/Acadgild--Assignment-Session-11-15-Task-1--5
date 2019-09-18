library(readr)
Crimes<-read.csv()
View(Crimes)
names(Crimes) <- c("Case", "Number", "Date", "Block", "IUCR", "Primary Type",
                   "Description", "Location Desc", "Arrest", "Domestic", "Beat", "District", "Ward",
                   "Community Area","FBI Code", "X Coordinate", "Y Coordinate", "Year", "Updated On",
                   "Latitude", "Longitude", "Location")
head(Crimes)
str(Crimes)
library(dplyr)
Crimes <- na.omit(Crimes)
names(Crimes)
c <- cor(Crimes[c(11,12,13,14,18,20,21)])
c
psych::cor.plot(c)
x <- as.data.frame(table(Crimes$Description))
x[order(x$Freq, decreasing = T)[1:3],]
correlation <- cor(Crimes[c(11,12,13,14,18,20,21)])
correlation
psych::cor.plot(correlation)
covariance <- cov(Crimes[c(11,12,13,14,18,20,21)])
covariance
psych::cor.plot(covariance)
