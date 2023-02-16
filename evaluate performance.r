
AUCFunzGLM <- function(x,TrainValue = 0.8, TestValue = 0.2) {
  library(groupdata2) #to divde data set
  library(ROCR) #for the metric
  
  set.seed(999)
  inds <- partition(x, p = c(train = 0.8, test = 0.2))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  
  #Favourability
  Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  Prob <-prediction(Pred$pres_P, validation[,1], label.ordering = NULL)
  
  #ROC
  ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
  
  #AUC
  AUCperfProb <-performance(Prob, measure="auc")
  AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
  AUCperfProb <-round(AUCperfProb,4)
  
  return(AUCperfProb)
}
###Example trial
#AUC_GLM_s1.t1<- AUCFunzGLM(modSpecies.s1t1)


AUCFunzRF <- function(x,TrainValue = 0.8, TestValue = 0.2) {
  library(groupdata2)
  library(ROCR)
  
  set.seed(999)
  inds <- partition(x, p = c(train = 0.8, test = 0.2))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  
  #Favourability
  
  Model<-ranger(training$pres ~., data= training, importance='impurity') 
  Pred<- predict(
    Model,
    data = validation[2:ncol(validation)],
    predict.all = FALSE,
    num.trees = Model$num.trees)
  
  Prob <-prediction(Pred$predictions, validation[,1], label.ordering = NULL)
  
  #ROC
  ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
  
  #AUC
  AUCperfProb <-performance(Prob, measure="auc")
  AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
  AUCperfProb <-round(AUCperfProb,4)
  
  return(AUCperfProb)
}
###Example trail
#AUC_RF_s1.t1<- AUCFunzRF(modSpecies.s1t1)

AUCFunzGAM <- function(x,TrainValue = 0.8, TestValue = 0.2) {
  library(groupdata2)
  library(ROCR)
  
  set.seed(999)
  inds <- partition(x, p = c(train = 0.8, test = 0.2))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  sp_cols <- 1
  pred_cols <- 2:ncol(training)
  names(training)[sp_cols]
  names(training)[pred_cols]
  #Favourability
  
  form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
  Model <- gam(form_gam, family = binomial, data = training)
  prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
  df.prediction <- data.frame(Pred=prediction)
  Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
  
  #ROC
  ROCperfProb<-performance(Prob, measure="tpr", x.measure="fpr")
  
  #AUC
  AUCperfProb <-performance(Prob, measure="auc")
  AUCperfProb <-unlist(slot(AUCperfProb, "y.values"))
  AUCperfProb <-round(AUCperfProb,4)
  
  return(AUCperfProb)
}
###Exampe trial
#AUC_GAM_s1.t1<- AUCFunzGAM(modSpecies.s1t1)

##################TSS metric################################################

TSSFunzGLM <- function(x, TrainValue = 0.8, TestValue = 0.2) {
  library(groupdata2)
  library(ROCR)
  
  set.seed(999)
  inds <- partition(x, p = c(train = 0.8, test = 0.2))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  
  #Favourability
  Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE, 
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,Favourability = FALSE)
  df.prediction<- data.frame(Pred$pres_P)
  
  data_thres<- data.frame(ID=1:nrow(test),test[,1], df.prediction)
  thres <- PresenceAbsence::optimal.thresholds(DATA= data_thres, opt.methods = 'MaxSens+Spec') #--- if we must select the optimal thresholds, note: if we use this, we must delete the thres in the function input
  cmx<- cmx(data_thres, threshold=thres[,2])
  
  TSSperfProb<- PresenceAbsence::sensitivity(cmx, st.dev=F) + 
    PresenceAbsence::specificity(cmx, st.dev=F) - 1
  
  TSSperfProb <-round(TSSperfProb,4)
  
  return(TSSperfProb)
}
#TSS_GLM_s1.t1<- TSSFunzGLM(modSpecies.s1t1)

TSSFunzRF <- function(x,TrainValue = 0.8, TestValue = 0.2) {
  library(groupdata2)
  library(ROCR)
  
  set.seed(999)
  inds <- partition(x, p = c(train = 0.8, test = 0.2))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  
  #Favourability
  
  Model<-ranger(training$pres ~., data= training, importance='impurity') 
  Pred<- predict(
    Model,
    data = validation,
    predict.all = FALSE,
    num.trees = Model$num.trees)
  df.prediction<- data.frame(Pred$predictions)
  Prob <-prediction(Pred$predictions, validation[,1], label.ordering = NULL)
  
  data_thres<- data.frame(ID=1:nrow(test),test[,1], df.prediction)
  thres <- PresenceAbsence::optimal.thresholds(DATA= data_thres, opt.methods = 'MaxSens+Spec')
  cmx<- cmx(data_thres, threshold=thres[,2])
  
  TSSperfProb<- PresenceAbsence::sensitivity(cmx, st.dev=F) + 
    PresenceAbsence::specificity(cmx, st.dev=F) - 1
  
  TSSperfProb <-round(TSSperfProb,4)
  
  return(TSSperfProb)
}


TSSFunzGAM <- function(x,TrainValue = 0.8, TestValue = 0.2) {
  library(groupdata2)
  library(ROCR)
  
  set.seed(999)
  inds <- partition(x, p = c(train = 0.8, test = 0.2))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  sp_cols <- 1
  pred_cols <- 2:4
  names(training)[sp_cols]
  names(training)[pred_cols]
  #Favourability
  
  form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
  Model <- gam(form_gam, family = binomial, data = training)
  prediction <- predict(Model, newdata = validation, type = "response")
  df.prediction <- data.frame(Pred=prediction)
  Prob <-prediction(df.prediction$Pred, validation[,1], label.ordering = NULL)
  data_thres<- data.frame(ID=1:nrow(test),test[,1], df.prediction)
  thres <- PresenceAbsence::optimal.thresholds(DATA= data_thres, opt.methods = 'MaxSens+Spec')
  cmx<- cmx(data_thres, threshold=thres[,2])
  
  TSSperfProb<- PresenceAbsence::sensitivity(cmx, st.dev=F) + 
    PresenceAbsence::specificity(cmx, st.dev=F) - 1
  
  
  TSSperfProb <-round(TSSperfProb,4)
  
  return(TSSperfProb)
}


##################BOYCE###################################################


BoyceGLM <- function(x){
  library(ecospat)
  
  set.seed(999)
  inds <- partition(x, p = c(train = 0.8, test = 0.2))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  
  
  Model<-multGLM(training, sp.cols = 1, var.cols=2:ncol(training), family = "binomial",
                 step = FALSE, FDR = FALSE, trim = FALSE, Y.prediction = FALSE,
                 P.prediction = TRUE, Favourability = TRUE)
  Pred<- getPreds(validation[2:ncol(validation)], models=Model$models, id.col = NULL, Y = FALSE, P = TRUE,
                  Favourability = FALSE)
  
  
  b <- data.frame(cbind(pred=Pred$pres_P, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceRF <- function(x){
  library(ecospat)
  
  set.seed(999)
  inds <- partition(x, p = c(train = 0.8, test = 0.2))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  
  Model<-ranger(training$pres ~., data= training, importance='impurity')
  Pred<- predict(
    Model,
    data = validation[2:ncol(validation)],
    predict.all = FALSE,
    num.trees = Model$num.trees)
  
  Pred <- data.frame(predictions= Pred[["predictions"]])
  Pred$predictions[ Pred$predictions == 1] <- 1 - 2.2e-16
  
  b <- data.frame(cbind(pred=Pred$predictions, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

BoyceGAM <- function(x){
  library(ecospat)
  
  set.seed(999)
  inds <- partition(x, p = c(train = 0.8, test = 0.2))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  sp_cols <- 1
  pred_cols <- 2:4
  names(training)[sp_cols]
  names(training)[pred_cols]
  
  
  form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
  Model <- gam(form_gam, family = binomial, data = training)
  prediction <- predict(Model, newdata = validation[2:ncol(validation)], type = "response")
  df.prediction <- data.frame(Pred=prediction)
  
  b <- data.frame(cbind(pred=df.prediction$Pred, pres=validation$pres))
  obs <- (b$pred
          [which(b$pres==1)])
  
  boyce <- ecospat.boyce(fit=b$pred, obs=obs, nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
  return(boyce)
}

#####Create table of results
auc.gam.s1.t1<- AUCFunzGAM(modSpecies.s1t1)
auc.gam.s1.t2<- AUCFunzGAM(modSpecies.s1t2)
auc.gam.s1.t3<- AUCFunzGAM(modSpecies.s1t3)

auc.gam.s1.T1<- AUCFunzGAM(modSpecies.s1T1)
auc.gam.s1.T2<- AUCFunzGAM(modSpecies.s1T2)
auc.gam.s1.T3<- AUCFunzGAM(modSpecies.s1T3)



auc.glm.s1.t1<- AUCFunzGLM(modSpecies.s1t1)
auc.glm.s1.t2<- AUCFunzGLM(modSpecies.s1t2)
auc.glm.s1.t3<- AUCFunzGLM(modSpecies.s1t3)

auc.glm.s1.T1<- AUCFunzGLM(modSpecies.s1T1)
auc.glm.s1.T2<- AUCFunzGLM(modSpecies.s1T2)
auc.glm.s1.T3<- AUCFunzGLM(modSpecies.s1T3)



auc.rf.s1.t1<- AUCFunzRF(modSpecies.s1t1)
auc.rf.s1.t2<- AUCFunzRF(modSpecies.s1t2)
auc.rf.s1.t3<- AUCFunzRF(modSpecies.s1t3)

auc.rf.s1.T1<- AUCFunzRF(modSpecies.s1T1)
auc.rf.s1.T2<- AUCFunzRF(modSpecies.s1T2)
auc.rf.s1.T3<- AUCFunzRF(modSpecies.s1T3)



tss.glm.s1.t1<- TSSFunzGLM(modSpecies.s1t1)
tss.glm.s1.t2<- TSSFunzGLM(modSpecies.s1t2)
tss.glm.s1.t3<- TSSFunzGLM(modSpecies.s1t3)

tss.glm.s1.T1<- TSSFunzGLM(modSpecies.s1T1)
tss.glm.s1.T2<- TSSFunzGLM(modSpecies.s1T2)
tss.glm.s1.T3<- TSSFunzGLM(modSpecies.s1T3)



tss.gam.s1.t1<- TSSFunzGAM(modSpecies.s1t1)
tss.gam.s1.t2<- TSSFunzGAM(modSpecies.s1t2)
tss.gam.s1.t3<- TSSFunzGAM(modSpecies.s1t3)

tss.gam.s1.T1<- TSSFunzGAM(modSpecies.s1T1)
tss.gam.s1.T2<- TSSFunzGAM(modSpecies.s1T2)
tss.gam.s1.T3<- TSSFunzGAM(modSpecies.s1T3)



tss.rf.s1.t1<- TSSFunzRF(modSpecies.s1t1)
tss.rf.s1.t2<- TSSFunzRF(modSpecies.s1t2)
tss.rf.s1.t3<- TSSFunzRF(modSpecies.s1t3)

tss.rf.s1.T1<- TSSFunzRF(modSpecies.s1T1)
tss.rf.s1.T2<- TSSFunzRF(modSpecies.s1T2)
tss.rf.s1.T3<- TSSFunzRF(modSpecies.s1T3)



boyce.glm.s1.t1<- BoyceGLM(modSpecies.s1t1)
boyce.glm.s1.t2<- BoyceGLM(modSpecies.s1t2)
boyce.glm.s1.t3<- BoyceGLM(modSpecies.s1t3)

boyce.glm.s1.T1<- BoyceGLM(modSpecies.s1T1)
boyce.glm.s1.T2<- BoyceGLM(modSpecies.s1T2)
boyce.glm.s1.T3<- BoyceGLM(modSpecies.s1T3)


boyce.gam.s1.t1<- BoyceGAM(modSpecies.s1t1)
boyce.gam.s1.t2<- BoyceGAM(modSpecies.s1t2)
boyce.gam.s1.t3<- BoyceGAM(modSpecies.s1t3)

boyce.gam.s1.T1<- BoyceGAM(modSpecies.s1T1)
boyce.gam.s1.T2<- BoyceGAM(modSpecies.s1T2)
boyce.gam.s1.T3<- BoyceGAM(modSpecies.s1T3)


boyce.rf.s1.t1<- BoyceRF(modSpecies.s1t1)
boyce.rf.s1.t2<- BoyceRF(modSpecies.s1t2)
boyce.rf.s1.t3<- BoyceRF(modSpecies.s1t3)

boyce.rf.s1.T1<- BoyceRF(modSpecies.s1T1)
boyce.rf.s1.T2<- BoyceRF(modSpecies.s1T2)
boyce.rf.s1.T3<- BoyceRF(modSpecies.s1T3)


gam<- c(auc.gam.s1.t1,auc.gam.s1.T1, auc.gam.s1.t2, auc.gam.s1.T2, auc.gam.s1.t3, auc.gam.s1.T3,
          tss.gam.s1.t1, tss.gam.s1.T1, tss.gam.s1.t2, tss.gam.s1.T2, tss.gam.s1.t3, tss.gam.s1.T3,
        boyce.gam.s1.t1, boyce.gam.s1.T1, boyce.gam.s1.t2, boyce.gam.s1.T2, boyce.gam.s1.t3, boyce.gam.s1.T3)

glm<- c(auc.glm.s1.t1,auc.glm.s1.T1, auc.glm.s1.t2, auc.glm.s1.T2, auc.glm.s1.t3, auc.glm.s1.T3,
        tss.glm.s1.t1, tss.glm.s1.T1, tss.glm.s1.t2, tss.glm.s1.T2, tss.glm.s1.t3, tss.glm.s1.T3,
        boyce.glm.s1.t1, boyce.glm.s1.T1, boyce.glm.s1.t2, boyce.glm.s1.T2, boyce.glm.s1.t3, boyce.glm.s1.T3)

rf<- c(auc.rf.s1.t1,auc.rf.s1.T1, auc.rf.s1.t2, auc.rf.s1.T2, auc.rf.s1.t3, auc.rf.s1.T3,
        tss.rf.s1.t1, tss.rf.s1.T1, tss.rf.s1.t2, tss.rf.s1.T2, tss.rf.s1.t3, tss.rf.s1.T3,
        boyce.rf.s1.t1, boyce.rf.s1.T1, boyce.rf.s1.t2, boyce.rf.s1.T2, boyce.rf.s1.t3, boyce.rf.s1.T3)
metric<- c('AUC s1_t1','AUC s1_T1', 'AUC s1_t2','AUC s1_T2', 'AUC s1_t3', 'AUC s1_T3', 'TSS s1_t1', 'TSS s1_T1', 'TSS s1_t2', 'TSS s1_T2', 'TSS s1_t3', 'TSS s1_T3', 'Boyce s1_t1', 'Boyce s1_T1', 'Boyce s1_t2', 'Boyce s1_T2', 'Boyce s1_t3', 'Boyce s1_T3')
tab<- data.frame(metric,gam, glm, rf)

