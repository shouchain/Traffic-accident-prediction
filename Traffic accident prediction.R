#Load the data
dataset <- read.csv("Accidents.csv", header=T,fileEncoding="Big5")
#第9643項資料全部都是空的
dataset <- dataset[-9643,]
dataset
#更動“事故類別名稱“的敘事方法
table(dataset$事故類別名稱)
dataset$事故類別名稱 <- gsub('A1',1, dataset$事故類別名稱)
dataset$事故類別名稱 <- gsub('A2',1, dataset$事故類別名稱)
dataset$事故類別名稱 <- gsub('A3',0, dataset$事故類別名稱)
dataset$事故類別名稱 = as.integer(dataset$事故類別名稱)
table(dataset$事故類別名稱)

#刪除意義重複的特徵
dataset <- subset(dataset, select = c(-編號,-光線代碼, -路面狀況.路面狀態代碼,-保護裝備代碼,-飲酒情形代碼,-車種代碼,-天候代碼,-當事者性別名稱))

#處理空白格
dataset$天候名稱 <- gsub(' ', NA , dataset$天候名稱)
dataset$光線名稱 <- gsub(' ', NA , dataset$光線名稱)
dataset$車種名稱 <- gsub(' ', NA , dataset$車種名稱)
dataset$保護裝備名稱 <- gsub(' ', NA , dataset$保護裝備名稱)
dataset$飲酒情形名稱 <- gsub(' ', NA , dataset$飲酒情形名稱)

#把缺失過多的特徵刪除
dataset <- subset(dataset, select = c(-飲酒情形名稱))
dataset <- subset(dataset, select = c(-保護裝備名稱))
dataset <- subset(dataset, select = c(-光線名稱))

#處理過後，確認所有缺失值都以填補
dataset <- dataset[complete.cases(dataset), ]
print(sapply(dataset, function(df){sum(is.na(df)/nrow(dataset))}))

#類別變數設定為虛擬變數
for(unique_value in unique(dataset$天候名稱)){
  dataset[paste("Wether", unique_value, sep = ".")] <- ifelse(dataset$天候名稱== unique_value, 1, 0)
}
for(unique_value in unique(dataset$路面狀況名稱)){
  dataset[paste("Roadtype", unique_value, sep = ".")] <- ifelse(dataset$路面狀況名稱== unique_value, 1, 0)
}
for(unique_value in unique(dataset$當事者性別代碼)){
  dataset[paste("Gender", unique_value, sep = ".")] <- ifelse(dataset$當事者性別代碼== unique_value, 1, 0)
}
for(unique_value in unique(dataset$車種名稱)){
  dataset[paste("Transportation", unique_value, sep = ".")] <- ifelse(dataset$車種名稱== unique_value, 1, 0)
}
for(unique_value in unique(dataset$發生星期)){
  dataset[paste("DayOfWeek", unique_value, sep = ".")] <- ifelse(dataset$發生星期== unique_value, 1, 0)
}
dataset <- subset(dataset, select = c(-發生星期,-天候名稱,-路面狀況名稱, -當事者性別代碼,-車種名稱))

#模型設置
model <- glm(training$事故類別名稱 ~ ., family=binomial, data=training, control=list(maxit=20))
p2 <- predict(model, testing, type="response")

#觀察結果
predict_results <- ifelse(p2 > 0.5, 1, 0)
Accuracy <- mean(predict_results == testing$事故類別名稱)

table(testing$事故類別名稱, predict_results)