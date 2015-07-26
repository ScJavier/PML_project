setwd("C:/Users/Javier/Google Drive/Coursera/Data Science Specialization/08 - Practical Machine Learning/Coursera_PML-PA_project")

if(!file.exists("../pml-training.csv"))
{
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","pml-training.csv")
}

if(!file.exists("../pml-testing.csv"))
{
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","pml-testing.csv")
}

training<-read.csv("../pml-training.csv")

library(caret)

stuff<-nearZeroVar(training[,-160])
training<-training[,-stuff]

na.obs<-apply(is.na(training), 2, sum)
pct.na.obs<-na.obs/dim(training)[1]*100
keepvar<-pct.na.obs<20
training<-training[,keepvar]

set.seed(147) # For reproducibility
subt_index <- createDataPartition(training$classe, p = 0.60, list = FALSE)
subtraining <- training[subt_index, ]
validating <- training[-subt_index, ]

# Model 1. Boosting classification trees

tune.gbm=data.frame(n.trees=50,
                    interaction.depth=4,
                    shrinkage=0.1,
                    n.minobsinnode=100)

model_1<-train(classe~., data = subtraining, method = "gbm",
               tuneGrid=tune.gbm,
               trControl=trainControl(method="boot",number=10))

confusionMatrix(predict(model_1,validating),validating$classe)

# Model 2. Random Forest
model_2<-train(classe~., data = subtraining, method = "rf",
               tuneGrid=data.frame(mtry=15),
               trControl=trainControl(method="boot",number=10))

confusionMatrix(predict(model_2,validating),validating$classe)



