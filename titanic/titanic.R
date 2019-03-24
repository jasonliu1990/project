library(DMwR)
library(dplyr)
# 導入 data
train <- read.csv('C:/Users/Jason/Desktop/all/train.csv')
test <- read.csv('C:/Users/Jason/Desktop/all/test.csv')
full <- bind_rows(train,test)

str(full)
summary(full)

full$Survived <- as.factor(full$Survived)
full$Pclass <- as.factor(full$Pclass)
full$SibSp <- as.factor(full$SibSp)
full$Parch <- as.factor(full$Parch)
full$fam <- as.factor(c(full$SibSp) + c(full$Parch))

full$Title <- gsub("(.*, )|(\\..*)","",full$Name)
rare_title <- c('Dona','the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev','Jonkheer')
full$Title[full$Title == 'Lady']        <- 'Miss'
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs'
full$Title[full$Title == 'Sir']         <- 'Mr'
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
full$Title <- as.factor(full$Title)

install.packages("rpart")
library(rpart)
age.model <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + fam, 
                   data=full[!is.na(full$Age), ], method='anova')
full$Age[is.na(full$Age)] <- predict(age.model, full[is.na(full$Age), ])
sapply(full,function(x) sum(is.na(x)))

full$Fare[1044] <- 0

library('partykit')
full$Pclass <- factor(full$Pclass)
full$fam <- factor(full$fam)
full$Embarked <- factor(full$Embarked)
set.seed(100)
model <- cforest(Survived ~ Pclass + Title + Sex + Age + SibSp + 
                   Parch + fam  + Fare +  
                   Embarked, data =full[1:891,], 
                   ntree=2000, mtry=3)

predict.result <- predict(model, full[(1+nrow(train)):(nrow(full)), ], OOB=TRUE, type = "response")
