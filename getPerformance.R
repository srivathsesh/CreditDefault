getPerformance <- function(trainX,validX,trainY,validY,model, positive = "yes", rowNames = c("train","valid"),cutoff = 0.5, title = "", modelname =""){
  
  # mertrics to be used
  # 1. Accuracy, 2. Sensitivity, 3. Specificty 4. NIV, AUC
  
  library(caret)
  library(magrittr)
  library(dplyr)
  library(pROC)
  
  #browser()
  if(class(model) == "randomForest"|class(model) == "OneR"){
    
    if(class(model) == "randomForest"){
      trainPred <- predict(model,trainX,type = "response")
      validPred <- predict(model,validX,type = "response")
    } else{
      trainPred <- predict(model,trainX,type = "class")
      validPred <- predict(model,validX,type = "class")
    }
    
    train.roc <- roc(response = trainY, predictor = predict(model,trainX,type = "prob")[,"yes"])
    valid.roc <- roc(response = validY, predictor = predict(model,validX,type = "prob")[,"yes"])
    
  } else if(class(model) == "train"){
    
    trainPred <- predict(model,trainX)
    validPred <- predict(model,validX)
    train.roc <- roc(response = trainY, predictor = predict(model,trainX,type = "prob")[,"yes"])
    valid.roc <- roc(response = validY, predictor = predict(model,validX,type = "prob")[,"yes"])
    
  }else{
    trainPred <- as.factor(ifelse(predict(model,trainX) > cutoff, "yes", "no"))
    validPred <- as.factor(ifelse(predict(model,validX) > cutoff, "yes","no"))
    
    train.roc <- roc(response = trainY, predictor = predict(model,trainX))
    valid.roc <- roc(response = validY, predictor = predict(model,validX))
  }
  
  
  
  trainConfusion <- confusionMatrix(trainPred,trainY, positive = positive)
  testConfusion <- confusionMatrix(validPred,validY,positive = positive)
  
  PerformanceMeasures <- rbind(trainConfusion$byClass, testConfusion$byClass)
  rownames(PerformanceMeasures) <- rowNames
  
  PerformanceMeasures <- data.frame(PerformanceMeasures) %>% 
    mutate(auc = c(auc(train.roc),auc(valid.roc)),
           Model = modelname)
  
  
  rownames(PerformanceMeasures) <- rowNames
 
  plot(train.roc,legacy.axes = T, asp = NA, col = "blue",main = title)
  plot(valid.roc,legacy.axes = T, asp = NA, col = "red",add = T)
  legend("bottomright",legend = c(paste0("Train; AUC:",round(PerformanceMeasures$auc[1],2)), paste0("validation; AUC:",round(PerformanceMeasures$auc[2],2))), col = c("blue", "red"), lty = 1,cex = 0.6)
  
  #p <- recordPlot()
  #plot.new()
  
  return(list(Perf = PerformanceMeasures,
              Plots = recordPlot(),
              conftbltrain = trainConfusion$table,
              conftblvalid = testConfusion$table))
  
  
}
  