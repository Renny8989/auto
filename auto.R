### symboling prediction

#loading data
library(readr)
dat = read.csv2("auto1.csv")
library(randomForest)

dat$normalized.losses=as.character(dat$normalized.losses)
dat$normalized.losses=as.integer(dat$normalized.losses)
dat$symboling=as.factor(dat$symboling)
dat$wheel.base=as.character(dat$wheel.base)
dat$wheel.base=as.numeric(dat$wheel.base)
dat$length=as.character(dat$length)
dat$length=as.numeric(dat$length)
dat$width=as.character(dat$width)
dat$width=as.numeric(dat$width)
dat$height=as.character(dat$height)
dat$height=as.numeric(dat$height)
dat$bore=as.character(dat$bore)
dat$bore=as.numeric(dat$bore)
dat$stroke=as.character(dat$stroke)
dat$stroke=as.numeric(dat$stroke)
dat$compression.ratio=as.character(dat$compression.ratio)
dat$compression.ratio=as.numeric(dat$compression.ratio)
dat$horsepower=as.character(dat$horsepower)
dat$horsepower=as.numeric(dat$horsepower)
dat$peak.rpm=as.character(dat$peak.rpm)
dat$peak.rpm=as.numeric(dat$peak.rpm)
dat$price=as.character(dat$price)
dat$price=as.numeric(dat$price)

# splitting train and test data
smp_size <- floor(0.7 * nrow(dat))
set.seed(12)
train_ind <- sample(seq_len(nrow(dat)), size = smp_size)
train <- dat[train_ind, ]
test <- dat[-train_ind, ]

# model training and prediction 
rf=randomForest(symboling~., data = train, ntree=70, na.action = na.omit)
test.ctree = predict(rf, newdata = test, type = "class")
test.ctree

# model evaluation 
library(caret)
library(e1071)
a=confusionMatrix(data=test.ctree, reference = test$symboling)
a

#variable importance 
varImpPlot(rf,sort = T, main = "Variable Importance", importance=TRUE)

# oob error
# y is number of trees
plot(rf$err.rate[,1])
