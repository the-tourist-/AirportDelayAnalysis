require(caret)
require(e1071)
require(magrittr)
require(car)

GetBestTune <- function(Model) {
  if (class(Model)=="caretStack")
    Model$ens_model$results[rownames(Model$ens_model$bestTune), ]
  else
    Model$results[rownames(Model$bestTune), ]
}

GetOutOfSamplePredictions <- function(Model) {
  BestTune <- GetBestTune(Model)
  Parameters <- names(BestTune)[!(names(BestTune) %in% c(Model$perfNames, paste0(Model$perfNames, "SD")))]
  Predictions <- Model$pred[eval(parse(text=paste(paste0("Model$pred$", Parameters, " == BestTune$", Parameters), collapse=" & "))), ]
  Predictions <- data.frame()
  for (Fold in names(Model$control$indexOut))
    Predictions <- rbind(Predictions, unique(BestTunePredictions[BestTunePredictions$Resample==Fold, ]))
  Predictions$pred[order(Predictions$rowIndex)]
} 

DF1 <- read.csv("~/GitHub/AirportDelayAnalysis/Casefile-Data/Martijn_GeNIe_Exp.csv")
DF2 <- read.csv("~/GitHub/AirportDelayAnalysis/Casefile-Data/DelayData.csv")
DF <- cbind(DF1, DF2)
DF[,2] <- NULL
names(DF) <- c("Wind.Avg", "Delay.Min", "Wind", "Direction", "Delay")
DF <- with(DF, data.frame(Wind.Avg, Wind, Direction, Delay.Min, Delay))
DF$DegreesOffNW <- 22.5*c(6, 5, 7, 2, 4, 3, 1, 0, 6, 8, 7, 5, 4, 2, 1, 3)[as.integer(DF1$Direction)]
DF$DegreesOffNE <- 22.5*c(2, 1, 3, 2, 0, 1, 3, 4, 6, 4, 5, 7, 8, 6, 5, 7)[as.integer(DF1$Direction)]

PreProcModel <- preProcess(DF[, c("Delay.Min", "Wind.Avg", "DegreesOffNW", "DegreesOffNE")], c("center", "scale", "knnImpute"))
DFPP <- predict(PreProcModel, DF[, c("Delay.Min", "Wind.Avg", "DegreesOffNW", "DegreesOffNE")])
DFPP$Wind.Interaction <- DFPP$Wind.Avg * DFPP$DegreesOffNW
PreProcModel2 <- preProcess(DFPP, c("YeoJohnson"))
DFPP <- predict(PreProcModel2, DFPP)
DFPP <- as.data.frame(sapply(names(DFPP), function(Column)pnorm(DFPP[, Column])))

scatterplotMatrix(~Delay.Min + Wind.Avg + DegreesOffNW + DegreesOffNE, data=DF)
scatterplotMatrix(~Delay.Min + Wind.Avg + DegreesOffNW + DegreesOffNE + Wind.Interaction, data=DFPP)
cor(as.matrix(DFPP))

DFPP$Delay <- DF$Delay

Model1 <- train(Delay.Min ~ Wind.Avg * DegreesOffNW + Wind.Avg * DegreesOffNE, 
                data = DFPP, 
                method = "rf", 
                tuneLength=5,
                trControl = trainControl(method = "cv", savePredictions=T)
)

Model2 <- train(DF$Delay ~ Wind.Avg * DegreesOffNW + Wind.Avg * DegreesOffNE, 
                data = DFPP, 
                method = "rf", 
                tuneLength=5,
                trControl = trainControl(method = "cv", savePredictions=T)
)

Model3 <- train(Delay.Min ~ Wind.Avg * DegreesOffNW + Wind.Avg * DegreesOffNE, 
                data = DFPP, 
                method = "svmRadial",
                tuneLength=5,
                trControl = trainControl(method = "cv", savePredictions=T)
)

Model4 <- train(DF$Delay ~ Wind.Avg * DegreesOffNW + Wind.Avg * DegreesOffNE, 
                data = DFPP, 
                method = "C5.0",
                tuneLength=5,
                trControl = trainControl(method = "cv", savePredictions=T)
)

Model5 <- train(Delay.Min ~ Wind.Avg * DegreesOffNW + Wind.Avg * DegreesOffNE, 
                data = DFPP, 
                method = "glm",
                trControl = trainControl(method = "cv", savePredictions=T)
)

print(Model1$results)
print(Model2$results)
print(Model3$results)
print(Model4$results)
print(Model5$results)

Model <- train(DF$Delay ~ Wind.Avg * DegreesOffNW + Wind.Avg * DegreesOffNE, 
               data = DFPP, 
               method = "rf", 
               tuneLength=5,
               trControl = trainControl(method = "cv", savePredictions=T)
)
BestTune <- GetBestTune(Model)
Parameters <- names(BestTune)[!(names(BestTune) %in% c(Model$perfNames, paste0(Model$perfNames, "SD")))]
Predictions <- Model$pred[eval(parse(text=paste(paste0("Model$pred$", Parameters, " == BestTune$", Parameters), collapse=" & "))), ]

print(confusionMatrix(Predictions$pred, Predictions$obs))
