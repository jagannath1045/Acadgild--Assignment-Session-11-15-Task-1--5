library(data.table)
library(foreach)
library(dplyr) library(readr)
blogData_train <- read_csv("blogData_train.csv")
View(blogData_train)
test_filenames = list.files(pattern = "blogData_test")
# load and combine dataset
train = fread("blogData_train.csv")
fbtest = foreach(i = 1:length(test_filenames), .combine = rbind) %do% {
  + temp = fread(test_filenames[i], header = F)
  # Assign variable names to the train and test data set
  colnames(blogData_train) <-c("plikes","checkin","talking","category","d5","d6","d7","d8","d9","d10","d11
","d12",
      +
        "d13","d14","d15","d16","d17","d18","d19","d20","d21","d22","d23","d24","d25"
      ,"d26",
      +"d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","post
shre",
      +
        "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basem
on",
      +
        "basetue","basewed","basethu","basefri","basesat","target")
dim(blogData_train)  > dim(fbtest)
View(blogData_train)  > View(fbtest)
str(blogData_train)  > str(fbtest)
train <- blogData_train; test <- fbtest
head(train); head(test)
# making the data tidy by constructing single collumn for post publish day
train$pubday<- ifelse(train$sun ==1, 1, ifelse(train$mon ==1, 2,
                                                     ifelse(train$tue ==1, 3,
                                                            +
                                                              ifelse(train$wed ==1, 4, ifelse(train$thu ==1, 5, ifelse(train$fri ==1, 6,
                                                                                                                       +
                                                                                                                         ifelse(train$sat ==1, 7, NA)))))))
# making the data tidy by constructing single collumn for base day
train$baseday<- ifelse(train$basesun ==1, 1, ifelse(train$basemon ==1, 2,
                                                          ifelse(train$basetue ==1, 3,
                                                                 +
                                                                   ifelse(train$basewed ==1, 4, ifelse(train$basethu ==1, 5,
                                                                                                       +
                                                                                                         ifelse(train$basefri ==1, 6, ifelse(train$basesat ==1, 7, NA)))))))
  
        
  + } 
"d27","d28","d29","cc1","cc2","cc3","cc4","cc5","basetime","postlength","post
shre",
+
  "postpromo","Hhrs","sun","mon","tue","wed","thu","fri","sat","basesun","basem
on",
+
  "basetue","basewed","basethu","basefri","basesat","target")
> dim(blogData_train)  > dim(fbtest)
> View(blogData_train)  > View(fbtest)
> str(blogData_train)  > str(fbtest)
> train <- blogData_train; test <- fbtest
> head(train); head(test)
> # making the data tidy by constructing single collumn for post publish day
  > train$pubday<- ifelse(train$sun ==1, 1, ifelse(train$mon ==1, 2,
                                                   ifelse(train$tue ==1, 3,
                                                          +
                                                            ifelse(train$wed ==1, 4, ifelse(train$thu ==1, 5, ifelse(train$fri ==1, 6,
                                                                                                                     +
                                                                                                                       ifelse(train$sat ==1, 7, NA)))))))
  # making the data tidy by constructing single collumn for base day
  train$baseday<- ifelse(train$basesun ==1, 1, ifelse(train$basemon ==1, 2,
                                                          ifelse(train$basetue ==1, 3,
                                                                 +
                                                                   ifelse(train$basewed ==1, 4, ifelse(train$basethu ==1, 5,
                                                                                                       +
                                                                                                         ifelse(train$basefri ==1, 6, ifelse(train$basesat ==1, 7, NA)))))))
  # Total comments are strongly correlated to correlated with cc3(comments in last 48 to
  last 24 hours relative to base date/time)
df <- train
melt_df <- melt(df)
library(ggplot2)
# Distribution of all the Variables - Histogram
ggplot(melt_df, aes(x=value, fill = variable))+
  geom_histogram(bins=10, color = "Blue")+
  facet_wrap(~variable, scales = 'free_x')
df <- log(train[1:39])
par(mfrow=c(1,1))

c. Visualize the dataset and make inferences from that.

barplot(table(train$target, train$pubday), col = heat.colors(7),
        xlab = "Weekday", ylab = "Number of comments",
        main = "Number of comments Vs. Weekday")

library(car)
# number of comments vs Post Likes
scatterplot(train$plikes, train$target , col = "Blue",
            xlab = "Page Likes", ylab = "Number of comments",
            main = "Number of comments Vs. Pagelikes",
            xlim = c(0,10000000), ylim = c(0,400))
abline(lm(plikes~target, data = train), col = "red")

# Number of comments Vs Post length
scatterplot(train$postlength, train$target , col = "Red",
            xlab = "Post Length", ylab = "Number of comments",
            main = "Number of comments Vs. Psot Length",
            ylim = c(0,400), xlim = c(0,5000))
abline(lm(postlength~target, data = train), col= "blue")
hist(train$target, breaks = 1000, xlim = c(0,10) ) 
d. Perform any 3 hypothesis tests using columns of your choice, make conclusions.

1. # Ho: Mean difference bet comments across the publish day is not significant
day <- aov(target~pubday, data = train)
summary(day)
2. # Ho: Difference between Mean comments within cc2 and cc4 is not significant
cc2 <- t.test(x=train$cc2, y=train$cc4, paired = FALSE, alternative = "two.sided", mu=0)
cc2 
3. # Ho: Difference between Mean comments within cc1 and cc3 is not significant
cc3 <- t.test(x=train$cc1, y=train$cc3, paired = FALSE, alternative = "two.sided", mu=0)
cc3

f. Fine tune the model and represent important features
final_model <- lm(target ~ talking + d5 + d7 + d8 + d10 + d11 +
                    d12 + d13 + d16 + d17 + d19 + d20 + d22 + d23 +
                    cc1 + cc2 + cc3 + cc4 + basetime + postshre + Hhrs, data = train)
summary(final_model)
prediction <- predict(final_model, test)
predicted <- data.frame(cbind(actuals = test$target, prediction = prediction))
predicted$prediction <- ifelse(prediction<0, 0, round(prediction,0))
cor(predicted)
View(predicted)
g. Interpret the summary of the linear model.
summary(final_model)

h. Report the test accuracy vs. the training accuracy
# test accuracy
round(accuracy(predicted$prediction,predicted$actuals),3)
prediction <- predict(final_model, test)
predicted <- data.frame(cbind(actuals = test$target, prediction = prediction))
predicted$prediction <- ifelse(prediction<0, 0, round(prediction,0))
min_max_accuracy <- mean(apply(predicted, 1, min) / apply(predicted, 1, max))
# training accuracy
round(accuracy(predicted$prediction,predicted$actuals),3)
prediction <- predict(final_model, train)
predicted <- data.frame(cbind(actuals = train$target, prediction = prediction))
predicted$prediction <- ifelse(prediction<0, 0, round(prediction, 0))
min_max_accuracy <- mean(apply(predicted, 1, min) / apply(predicted, 1, max))


