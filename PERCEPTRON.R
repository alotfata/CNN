install.packages("MASS")
library(MASS)
head(Boston)
str(Boston)
library(Amelia)
missmap(Boston)
head(Boston)

data<-Boston

#very important preprocessing in neural netowrk- > normalizing (min-max normalizing or z)-prefering z

#first try min-max normalizing

#counting max and min value of each columns - 

maxs<- apply(data, 2, max)
maxs
mins<- apply (data, 2, min)
mins

# using scale function to scale numeric matrix
sacleddata<- scale(data, center=mins, scale=maxs-mins)
# this is the matrix and use as dataframe to convert it to dataframe

scales<- as.data.frame(sacleddata)

head(scales)

#design neural model

library (caTools)
split<-sample.split(scales$medv, SplitRatio = 0.7)
train<-subset(scales, split==TRUE)
test<-subset(scales, split==FALSE)

install.packages('neuralnet')
library(neuralnet)
# In r we have to manually input features, below code can ease our work
n<-names(train)
f<-as.formula(paste("medv~ ", paste(n[!n %in% "medv"], collapse="+")))
# output [medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + 
#tax + ptratio + black + lstat]

#train neural now
#selected hidden neurons of first layer and second layer 
nn<- neuralnet(f, data=train, hidden=c(5, 3), linear.output = TRUE)
plot(nn, main="prediction")

# we donot use labelled column (target)
predicted<- compute(nn,test[1:13])

# In the previous stage- we normalized data to get true prediction we should undo previous stages 

trueprediction<- predicted$net.result * (max(data$medv)-min(data$medv))+min(data$medv)
#to compare true prediction with actual values, it is essnetial to convert test dataset to true format (before normalizing)

truetest<- (test$medv)* (max(data$medv)-min(data$medv))+min(data$medv)

# now calculating MSE  (mean square error)

MSE<- sum((truetest-trueprediction)^2)/nrow(test)
MSE

# to plot error- we make datafarme
errordf<-data.frame(truetest, trueprediction)

head(errordf)

library (ggplot2)

ggplot(errordf, aes(x= truetest, y=trueprediction ))+geom_point()+geom_smooth()



