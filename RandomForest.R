require(caret)
require(e1071)

DF1 <- read.csv("~/GitHub/AirportDelayAnalysis/Casefile-Data/Martijn_GeNIe_Exp.csv")
DF2 <- read.csv("~/GitHub/AirportDelayAnalysis/Casefile-Data/DelayData.csv")
names(DF1) <- c("Wind", "Direction", "Delay")
Model1 <- train(Delay ~ Direction + Delay, 
                data = DF1, 
                method = "rf", 
                tuneLength = 9,
                preProcess = c("center", "scale", "YeoJohnson", "knnImpute"),
                trControl = trainControl(method = "oob")
)

Model2 <- train(Delay ~ Direction + Delay, 
                data = DF2, 
                method = "rf", 
                tuneLength = 9,
                preProcess = c("center", "scale", "YeoJohnson", "knnImpute"),
                trControl = trainControl(method = "oob")
)
print(Model1$results)
print(Model2$results)

print(confusionMatrix(predict(Model2, DF2), DF2$Delay))
