# task2 - ANOVA option1 assignment session 11-15- 
yeast <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.data', stringsAsFactors = FALSE) 
l <- readLines('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.names') 
l<-l[(grep('^7', l) + 1):(grep('^8', l) - 1)]
l <- l[grep('\\d\\..*:', l)] 

names(yeast) <- make.names(c(sub('.*\\d\\.\\s+(.*):.*', '\\1', l), 'class'))
View(yeast)


#a. What are the assumptions of ANOVA, test it out? 

#Assumptions of ANOVA:

#1. The data are Quantitative in nature and  are Normally Distributed.
plot(data,2) # the data is not normally distibuted, its a right skewed

# Extract the residuals
aov_residuals <- residuals(object = data)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals ) # p valu is less than 0.5

#output
#Shapiro-Wilk normality test

data<-aov_residuals

#2. samples are drawn from the population randomly and independently.

library(car)
Anova(data,type = "III") # two way anaova uing car package

#3.homogeneity of variance, variances of the population 
# from which samples have been drawn are equal 
plot(data,1) # points 1288, 115 and 1080 are outliers



library(ggplot2)
ggplot(yeast, aes(x =yeast$class, y = yeast$nuc)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("class variable") +
  ylab("nuc variable")



data<-lm(nuc~class, data=yeast)
data<-lm(yeast$nuc~yeast$class, data=yeast)
summary(data)
t.test(yeast$nuc,yeast$yeastclass)
anova(data)
res.ano <- aov(nuc~class, data= yeast)
summary(res.ano)

#output

#             Df Sum Sq Mean Sq F value Pr(>F)    


#b. Why ANOVA test? Is there any other way to answer the above question?

# to compare several population means at the same time.

kruskal.test(yeast$class, yeast$nuc) # when the data is not homoscadasticity , 
# then we use non parametric test of Kruskal_Wallis test

# output

# As an Alternative we can use Wilcox test 


pairwise.wilcox.test(yeast$nuc,yeast$class,p.adjust.method = "BH")

#output
