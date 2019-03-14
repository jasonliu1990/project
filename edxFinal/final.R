library(mice)
library(randomForest)
library(data.table)
# input data

train <- fread("c:/Users/jason/Desktop/final/train_values_oAriVNN.csv")
test <- fread("c:/Users/jason/Desktop/final/test_values.csv")
summary(train)
label <- fread("c:/Users/jason/Desktop/final/train_labels.csv")[,-1]
options(sicpen=999)

train <- cbind(train,label)
summary(train)
summary(label)

# 轉類別
train$rucc <- as.numeric(train$rucc)
train$urban_influence <- as.numeric(train$urban_influence)
train$economic_typology <- as.numeric(train$economic_typology)
train$year <- as.numeric(train$year)

test$rucc <- as.numeric(test$rucc)
test$urban_influence <- as.numeric(test$urban_influence)
test$economic_typology <- as.numeric(test$economic_typology)
test$year <- as.numeric(test$year)

# 刪掉不需要的欄位
test <- test[,-c(1:4,21:23)]
train <- train[,-c(1:4,21:23)]
str(test)
# mice補值

mice.data <- mice(train,
                  m = 1,           # 產生一個被填補好的資料表
                  maxit = 50,      # max iteration
                  method = "rf", # 使用CART決策樹，進行遺漏值預測
                  seed = 102)      # set.seed()，令抽樣每次都一樣


complete(mice.data, 1) # 1st data


# 拿第二個資料，作為後續分析的資料
train <- complete(mice.data, 1)
summary(train)

# test
mice.test <- mice(test,
                  m = 1,           # 產生一個被填補好的資料表
                  maxit = 50,      # max iteration
                  method = "rf", # 使用CART決策樹，進行遺漏值預測
                  seed = 102)      # set.seed()，令抽樣每次都一樣


complete(mice.test, 1) # 1st data


# 拿第一個資料，作為後續分析的資料
test <- complete(mice.test, 1)
summary(test)


# 轉換欄位數值
train[,c(12:20,24:33,41:46)] <- 
  train[,c(12:20,24:33,41:46)] * train[,5]

train[,35] <- train[,35] * train[,5] / 100000
train[,36] <- train[,36] * train[,5] / 100000
train[,37] <- train[,37] * train[,5] / 100000
train[,47] <- train[,47] * train[,5] / 1000
train[,48] <- train[,48] * train[,5] / 1000

test[,c(12:20,24:33,41:46)] <- 
  test[,c(12:20,24:33,41:46)] * test[,5]

test[,35] <- test[,35] * test[,5] / 100000
test[,36] <- test[,36] * test[,5] / 100000
test[,37] <- test[,37] * test[,5] / 100000
test[,47] <- test[,47] * test[,5] / 1000
test[,48] <- test[,48] * test[,5] / 1000

train <- train[,-1]
train <- train[,-2]

test <- test[,-1]
test <- test[,-2]


# 隨機森林

set.seed(103)
RF_Model <- randomForest(evictions ~ ., data=train , ntree=300)

RF.Prediction <- predict(RF_Model, test)
RF.Prediction <- round(RF.Prediction)
RF.Prediction.Output <- as.data.frame(RF.Prediction)
write.csv(RF.Prediction.Output,file = "c:/Users/jason/Desktop/final/rfRf1.csv")
write.csv(train, file = "c:/users/Jason/Desktop/final/train_new.csv")
write.csv(test, file = "c:/users/Jason/Desktop/final/test_new.csv")

