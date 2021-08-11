#setting up and adding the library needed 
library("doParallel")
library("randomForest")
library("e1071")
library("caret")
library("doParallel")


setwd("C:\Users\LENOVO\Documents\datasciencecoursera\assignement_ml")

set.seed(1603)

#Downloading the data 

training_f<- 'pml-training.csv'
testing_f <- 'pml-testing.csv'
trainingUrl <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
testingUrl <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

download.file(trainingUrl,training_f)
download.file(testingUrl,testing_f)

#cleaning_data from NA
#training dataset

training.df <- read.csv(training_f,na.strings = c("NA","","#DIV/0!"))
training.df <-training.df[,colSums(is.na(training.df))==0]
dim(training.df)

#testing dataset 
testing.df <- read.csv(testing_f,na.strings = c("NA","","#DIV/0!"))
testing.df <-testing.df[,colSums(is.na(testing.df))==0]
dim(testing.df)

#reduce the number of variables that would impact the ^prediction
Training.df <- training.df[,-c(1,7)]
Testing.df <- testing.df[,-c(1,7)]
dim(Training.df)
dim(Testing.df)

Training.nzv<-nzv(Training.df[,-ncol(Training.df)],saveMetrics=TRUE)
rownames(Training.nzv)
dim(Training.nzv)[1]

inTrain     <- createDataPartition(Training.df$classe, p = 0.6, list = FALSE)
inTraining  <- Training.df[inTrain,]
inTest      <- Training.df[-inTrain,]
dim(inTraining);dim(inTest)



#ML Model with cross validation"
mlFilename <- "ml.RData"
if (!file.exists(mlFilename)) {
  
  
  ncores <- makeCluster(detectCores() - 1)
  registerDoParallel(cores=ncores)
  getDoParWorkers() # 3    
  
  # use Random Forest method with Cross Validation, 4 folds
  ml <- train(classe ~ .
                   , data = inTraining
                   , method = "rf"
                   , metric = "Accuracy"  # categorical outcome variable 
                   , preProcess=c("center", "scale") # attempt to improve accuracy by normalising
                   , trControl=trainControl(method = "cv"
                                            , number = 4 # folds of the training data
                                            , p= 0.60
                                            , allowParallel = TRUE 
                   )
  )
  
  save(ml, file = "ml.RData")
 
  stopCluster(ncores)
} else {
  # Use cached model  
  load(file = mlFilename, verbose = TRUE)
}


print(ml,digits=4)
#prediction
predTest <- predict(ml,newdata = inTest)

ml$finalModel
varImp(ml)

#quiz answers
print(predict(ml, newdata=Testing.df))

