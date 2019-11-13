# model 1
getPerformance(x,xvalid,y,yvalid,rpartFit,title = "rpart decision tree - complexity cost tuning",trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1],validBILL = creditdata$BILL_AMT1[creditdata$data.group==3]) -> rpartstats

getPerformance(x,xtest,y,ytest,rpartFit,title = "rpart decision tree - complexity cost tuning",trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1],validBILL = creditdata$BILL_AMT1[creditdata$data.group==2]) -> rparttest

# model 2

 getPerformance(x,xvalid,y,yvalid,rpartFit2,title = "rpart decision tree - depth of tree tuning",trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1],validBILL = creditdata$BILL_AMT1[creditdata$data.group==3]) -> rpartstats2
 
getPerformance(x,xtest,y,ytest,rpartFit2,title = "rpart decision tree - depth of tree tuning",trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1],validBILL = creditdata$BILL_AMT1[creditdata$data.group==2]) -> rparttest2

# model 3
getPerformance(xOneR,validOneR,xOneR$DEFAULT,validOneR$DEFAULT,baselinemod_later,title = "OneR Decision tree single predictor",validBILL = creditdata$BILL_AMT1[creditdata$data.group==3],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1]) -> baselinemodstats

getPerformance(xOneR,testOneR,xOneR$DEFAULT,testOneR$DEFAULT,baselinemod_later,title = "OneR Decision tree single predictor",validBILL = creditdata$BILL_AMT1[creditdata$data.group==2],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1]) -> baselinemodstatsTest


# model 4
perf.logit <- getPerformance(trainProc[,-which(names(trainProc) %in% 'DEFAULT')],validProc[,-which(names(trainProc) %in% 'DEFAULT')],y_trainProc,validY,logitmdl2$finalModel, title = "Simple Logistic regression",,validBILL = creditdata$BILL_AMT1[creditdata$data.group==3],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1])

perf.logittest <- getPerformance(trainProc[,-which(names(trainProc) %in% 'DEFAULT')],testProc[,-which(names(trainProc) %in% 'DEFAULT')],y_trainProc,testY,logitmdl2$finalModel, title = "Simple Logistic regression",,validBILL = creditdata$BILL_AMT1[creditdata$data.group==2],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1])

# model 5

 glmLogit.perf <- getPerformance(trainProc[,-which(names(trainProc) %in% 'DEFAULT')],validProc[,-which(names(validProc) %in% 'DEFAULT')],y_trainProc,validY,glmLogit$finalModel, title = "Stepwise AIC logistic regression",validBILL = creditdata$BILL_AMT1[creditdata$data.group==3],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1])

 glmLogit.perf.test <- getPerformance(trainProc[,-which(names(trainProc) %in% 'DEFAULT')],testProc[,-which(names(testProc) %in% 'DEFAULT')],y_trainProc,testY,glmLogit$finalModel, title = "Stepwise AIC logistic regression",validBILL = creditdata$BILL_AMT1[creditdata$data.group==2],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1])
 
 
 # model 6
 
 glmnetLogit.perf <- getPerformance(trainProc[,-which(names(trainProc) %in% 'DEFAULT')] %>% data.matrix(),validProc[,-which(names(validProc) %in% 'DEFAULT')] %>% data.matrix(),y_trainProc,validY,glmnetLogit, title = "Lasso regression",validBILL = creditdata$BILL_AMT1[creditdata$data.group==3],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1])
 
 glmnetLogit.perf.test <- getPerformance(trainProc[,-which(names(trainProc) %in% 'DEFAULT')] %>% data.matrix(),testProc[,-which(names(testProc) %in% 'DEFAULT')] %>% data.matrix(),y_trainProc,testY,glmnetLogit, title = "Lasso regression",validBILL = creditdata$BILL_AMT1[creditdata$data.group==2],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1])
 
 
 # model 7
 
 perf.rf <- getPerformance(trainProc[,-which(names(trainProc) %in% 'DEFAULT')],validProc[,-which(names(validProc) %in% 'DEFAULT')],y_trainProc,validY,rf$finalModel, cutoff = 0.4,title = "Random forest",,validBILL = creditdata$BILL_AMT1[creditdata$data.group==3],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1])
 
 perf.rf.test <-getPerformance(trainProc[,-which(names(trainProc) %in% 'DEFAULT')],testProc[,-which(names(testProc) %in% 'DEFAULT')],y_trainProc,testY,rf, cutoff = 0.4,title = "Random forest",,validBILL = creditdata$BILL_AMT1[creditdata$data.group==2],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1])
 
 # model 8
 
 xgb.Perf <- getPerformance(trainProcMat[,-which(colnames(trainProcMat) %in% 'DEFAULT')],validProcMat[,-which(colnames(validProcMat) %in% 'DEFAULT')],y_trainProc,validY,xgb_model$finalModel, title = "xgBoost performance",validBILL = creditdata$BILL_AMT1[creditdata$data.group==3],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1])
 
 xgb.Perf.test <- getPerformance(trainProcMat[,-which(colnames(trainProcMat) %in% 'DEFAULT')],testProcMat[,-which(colnames(testProcMat) %in% 'DEFAULT')],y_trainProc,testY,xgb_model$finalModel, title = "xgBoost performance",validBILL = creditdata$BILL_AMT1[creditdata$data.group==2],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1])
 
 # model 9
 getPerformance(trainProc[,which(names(trainProc) %in% Preds)],validProc[,which(names(validProc) %in% Preds)],y_trainProc,validY,logitSelectedPred$finalModel,title = "Logit model with selected features",validBILL = creditdata$BILL_AMT1[creditdata$data.group==3],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1]) -> logitSelectedPredPerf
 
 getPerformance(trainProc[,which(names(trainProc) %in% Preds)],testProc[,which(names(testProc) %in% Preds)],y_trainProc,testY,logitSelectedPred$finalModel,title = "Logit model with selected features",validBILL = creditdata$BILL_AMT1[creditdata$data.group==2],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1]) -> logitSelectedPredPerf.test
 
 # model 10
 
 getPerformance(df[,PredsLogit],df_valid[,PredsLogit],y_trainProc,validY,logitSelectedPred2$finalModel, title = "Logit refit",validBILL = creditdata$BILL_AMT1[creditdata$data.group==3],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1]) -> logitSelectedPredPerf2
 
 getPerformance(df[,PredsLogit],df_test[,PredsLogit],y_trainProc,testY,logitSelectedPred2$finalModel, title = "Logit refit",validBILL = creditdata$BILL_AMT1[creditdata$data.group==2],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1]) -> logitSelectedPredPerf2.test
 
 # model 11
 
 getPerformance(df[,-which(names(df) %in% 'DEFAULT')],df_valid[,-which(names(df_valid) %in% 'DEFAULT')],y_trainProc,validY,nbSelectedPred, title = "Naive Bayes model Performance",validBILL = creditdata$BILL_AMT1[creditdata$data.group==3],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1]) -> nbPerf
 
 getPerformance(df[,-which(names(df) %in% 'DEFAULT')],df_test[,-which(names(df_test) %in% 'DEFAULT')],y_trainProc,testY,nbSelectedPred, title = "Naive Bayes model Performance",validBILL = creditdata$BILL_AMT1[creditdata$data.group==2],trainBILL = creditdata$BILL_AMT1[creditdata$data.group==1]) -> nbPerf.test
 
 
 trainEnsemble <- data.frame(
   mdl1 = rpartstats$trainPredProb,
   mdl2 = rpartstats2$trainPredProb,
   mdl3 = baselinemodstats$trainPredProb,
   mdl4 = perf.logit$trainPredProb,
   mdl5 = glmLogit.perf$trainPredProb,
   mdl6 = glmnetLogit.perf$trainPredProb,
   mdl7 = perf.rf$trainPredProb,
   mdl8 = xgb.Perf$trainPredProb,
   mdl9 = logitSelectedPredPerf$trainPredProb,
   mdl10 = logitSelectedPredPerf2$trainPredProb,
   mdl11 = nbPerf$trainPredProb,
   mdl12 = y_train_pred_prob2
   
 ) %>% 
   mutate(mdl1Class = ifelse(mdl1 > 0.5, 1, 0),
          mdl2Class = ifelse(mdl2 > 0.5, 1, 0),
          mdl3Class = ifelse(mdl3 > 0.5, 1, 0),
          mdl4Class = ifelse(mdl4 > 0.5, 1, 0),
          mdl5Class = ifelse(mdl5 > 0.5, 1, 0),
          mdl6Class = ifelse(mdl6 > 0.5, 1, 0),
          mdl7Class = ifelse(mdl7 > 0.5, 1, 0),
          mdl8Class = ifelse(mdl8 > 0.5, 1, 0),
          mdl9Class = ifelse(mdl9 > 0.5, 1, 0),
          mdl10Class = ifelse(mdl10 > 0.5, 1, 0),
          mdl11Class = ifelse(mdl11 > 0.5, 1, 0),
          mdl12Class = ifelse(mdl12 > 0.5, 1, 0)) %>% 
   mutate(Votes = rowSums(select(.,matches("mdl[0-9]+Class")))) %>% 
   mutate(class = as.factor(ifelse(Votes > 6, "yes","no"))) %>% 
   mutate(VAR = ifelse(class == "yes",1,0)*ifelse(!!trainBILL <= 0, 0, !!trainBILL))
 
 validEnsemble <- data.frame(
   mdl1 = rpartstats$validPredProb,
   mdl2 = rpartstats2$validPredProb,
   mdl3 = baselinemodstats$validPredProb,
   mdl4 = perf.logit$validPredProb,
   mdl5 = glmLogit.perf$validPredProb,
   mdl6 = glmnetLogit.perf$validPredProb,
   mdl7 = perf.rf$validPredProb,
   mdl8 = xgb.Perf$validPredProb,
   mdl9 = logitSelectedPredPerf$validPredProb,
   mdl10 = logitSelectedPredPerf2$validPredProb,
   mdl11 = nbPerf$validPredProb,
   mdl12 = y_valid_pred_prob2
   ) %>% 
   mutate(mdl1Class = ifelse(mdl1 > 0.5, 1, 0),
          mdl2Class = ifelse(mdl2 > 0.5, 1, 0),
          mdl3Class = ifelse(mdl3 > 0.5, 1, 0),
          mdl4Class = ifelse(mdl4 > 0.5, 1, 0),
          mdl5Class = ifelse(mdl5 > 0.5, 1, 0),
          mdl6Class = ifelse(mdl6 > 0.5, 1, 0),
          mdl7Class = ifelse(mdl7 > 0.5, 1, 0),
          mdl8Class = ifelse(mdl8 > 0.5, 1, 0),
          mdl9Class = ifelse(mdl9 > 0.5, 1, 0),
          mdl10Class = ifelse(mdl10 > 0.5, 1, 0),
          mdl11Class = ifelse(mdl11 > 0.5, 1, 0),
          mdl12Class = ifelse(mdl12 > 0.5, 1, 0)) %>% 
   mutate(Votes = rowSums(select(.,matches("mdl[0-9]+Class")))) %>% 
   mutate(class = as.factor(ifelse(Votes > 6, "yes","no"))) %>% 
   mutate(VAR = ifelse(class == "yes",1,0)*ifelse(!!validBILL <= 0, 0, !!validBILL))

 testEnsemble <- data.frame(
   mdl1 = rparttest$validPredProb,
   mdl2 = rparttest2$validPredProb,
   mdl3 = baselinemodstatsTest$validPredProb,
   mdl4 = perf.logittest$validPredProb,
   mdl5 = glmLogit.perf.test$validPredProb,
   mdl6 = glmnetLogit.perf.test$validPredProb,
   mdl7 = perf.rf.test$validPredProb,
   mdl8 = xgb.Perf.test$validPredProb,
   mdl9 = logitSelectedPredPerf.test$validPredProb,
   mdl10 = logitSelectedPredPerf2.test$validPredProb,
   mdl11 = nbPerf.test$validPredProb,
   mdl12 = y_test_pred_prob2
 ) %>% 
   mutate(mdl1Class = ifelse(mdl1 > 0.5, 1, 0),
          mdl2Class = ifelse(mdl2 > 0.5, 1, 0),
          mdl3Class = ifelse(mdl3 > 0.5, 1, 0),
          mdl4Class = ifelse(mdl4 > 0.5, 1, 0),
          mdl5Class = ifelse(mdl5 > 0.5, 1, 0),
          mdl6Class = ifelse(mdl6 > 0.5, 1, 0),
          mdl7Class = ifelse(mdl7 > 0.5, 1, 0),
          mdl8Class = ifelse(mdl8 > 0.5, 1, 0),
          mdl9Class = ifelse(mdl9 > 0.5, 1, 0),
          mdl10Class = ifelse(mdl10 > 0.5, 1, 0),
          mdl11Class = ifelse(mdl11 > 0.5, 1, 0),
          mdl12Class = ifelse(mdl12 > 0.5, 1, 0))  %>% 
   mutate(Votes = rowSums(select(.,matches("mdl[0-9]+Class")))) %>% 
   mutate(class = as.factor(ifelse(Votes > 6, "yes","no"))) %>% 
   mutate(VAR = ifelse(class == "yes",1,0)*ifelse(!!testBILL <= 0, 0, !!testBILL))
 
confusionMatrix(trainEnsemble$class,estimates_keras_tbl2$truth, positive = "yes")

recoveredVAR_train_Ensemble <- sum(trainEnsemble$VAR)/ActualVAR_train
recoveredVAR_valid_Ensemble <- sum(validEnsemble$VAR)/ActualVAR_valid
recoveredVAR_test_Ensemble <- sum(testEnsemble$VAR)/ActualVAR_test
