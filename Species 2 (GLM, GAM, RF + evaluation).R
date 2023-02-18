para_s2<- formatFunctions(temp=c(fun='dnorm', mean = 24.37, sd = 0.42),
                          preci=c(fun='dnorm', mean =397.88, sd=374.32),
                          elev= c(fun='dnorm', mean=177, sd=25))

s2<- generateSpFromFun(raster.stack = envi[[c("temp", "preci", "elev")]],parameters = para_s2, plot = TRUE)
s2_pa <- convertToPA(s2, prob.method="logistic",alpha = -0.05, beta = "random", species.prevalence= 0.05)

s2_suit<-plotSuitabilityToProba(s2_pa)
s2_res<-plotResponse(s2_pa)

#T=sum, overlap
s2_T1 <- sampleOccurrences(s2_pa,n = 62,type = "presence-absence", extract.probability = TRUE,plot = T)
s2_T2 <- sampleOccurrences(s2_pa,n = 347,type = "presence-absence", extract.probability = TRUE,plot = F)
s2_T3 <- sampleOccurrences(s2_pa,n = 516,type = "presence-absence", extract.probability = TRUE,plot = F, replacement = T)
#t=single range, not overlap
s2_t1 <- sampleOccurrences(s2_pa,n = 59,type = "presence-absence", extract.probability = TRUE,plot = F)
s2_t2 <- sampleOccurrences(s2_pa,n = 244,type = "presence-absence", extract.probability = TRUE,plot = F)
s2_t3 <- sampleOccurrences(s2_pa,n = 213,type = "presence-absence", extract.probability = TRUE,plot = F, replacement = T)

########GLM########
####non overlapping time scale (t)
##s2_t1
#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s2t1=s2_t1$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s2t1)<-~x+y
crs(PresAbs.s2t1)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s2t1<- raster::extract(envi, PresAbs.s2t1, df=T)

values.s2t1<- values.s2t1[, -1] # first column not need it

modSpecies.s2t1<-data.frame(pres= PresAbs.s2t1$Observed, values.s2t1[1:ncol(values.s2t1)])

preds<- envi
## Favourability and Probability
glm.s2t1<-multGLM(modSpecies.s2t1, sp.cols = 1, var.cols=2:ncol(modSpecies.s2t1), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s2t1<- getPreds(preds, models=glm.s2t1$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

prediction.glm.s2t1<- data.frame(glm.s2t1$predictions)



crs(Pred.glm.s2t1) <- crs(envi)
plot(Pred.glm.s2t1, main= "Occurence Probability from 2000 to 2009 of Species 2 (s2_t1) by GLM")


##s2_t2
#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s2t2=s2_t2$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s2t2)<-~x+y
crs(PresAbs.s2t2)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s2t2<- raster::extract(envi, PresAbs.s2t2, df=T)

values.s2t2<- values.s2t2[, -1] # first column not need it

modSpecies.s2t2<-data.frame(pres= PresAbs.s2t2$Observed, values.s2t2[1:ncol(values.s2t2)])

preds<- envi
## Favourability and Probability
glm.s2t2<-multGLM(modSpecies.s2t2, sp.cols = 1, var.cols=2:ncol(modSpecies.s2t2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s2t2<- getPreds(preds, models=glm.s2t2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.glm.s2t2) <- crs(envi)
plot(Pred.glm.s2t2, main= "Occurence Probability from 2010 to 2014 of Species 2 (s2_t2) by GLM")


#s2_t3
#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s2t3=s2_t3$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s2t3)<-~x+y
crs(PresAbs.s2t3)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s2t3<- raster::extract(envi, PresAbs.s2t3, df=T)

values.s2t3<- values.s2t3[, -1] # first column not need it

modSpecies.s2t3<-data.frame(pres= PresAbs.s2t3$Observed, values.s2t3[1:ncol(values.s2t3)])

preds<- envi
## Favourability and Probability
glm.s2t3<-multGLM(modSpecies.s2t3, sp.cols = 1, var.cols=2:ncol(modSpecies.s2t3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s2t3<- getPreds(preds, models=glm.s2t3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.glm.s2t3) <- crs(envi)
plot(Pred.glm.s2t3, main= "Occurence Probability from 2015 to 2023 of Species 2 (s2_t3) by GLM")





####overlapping timescale (T)
##s2_T1

#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s2T1=s2_T1$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s2T1)<-~x+y
crs(PresAbs.s2T1)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s2T1<- raster::extract(envi, PresAbs.s2T1, df=T)

values.s2T1<- values.s2T1[, -1] # first column not need it

modSpecies.s2T1<-data.frame(pres= PresAbs.s2T1$Observed,values.s2T1[1:ncol(values.s2T1)])

preds<- envi
## Favourability and Probability
glm.s2T1<-multGLM(modSpecies.s2T1, sp.cols = 1, var.cols=2:ncol(modSpecies.s2T1), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s2T1<- getPreds(preds, models=glm.s2T1$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.glm.s2T1) <- crs(envi)
plot(Pred.glm.s2T1, main= "Occurence Probability from 2000 to 2010 of Species 2 (s2_T1) by GLM")


##s2_T2
PresAbs.s2T2=s2_T2$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s2T2)<-~x+y
crs(PresAbs.s2T2)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s2T2<- raster::extract(envi, PresAbs.s2T2, df=T)

values.s2T2<- values.s2T2[, -1] # first column not need it

modSpecies.s2T2<-data.frame(pres= PresAbs.s2T2$Observed, values.s2T2[1:ncol(values.s2T2)])

preds<- envi
## Favourability and Probability
glm.s2T2<-multGLM(modSpecies.s2T2, sp.cols = 1, var.cols=2:ncol(modSpecies.s2T2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s2T2<- getPreds(preds, models=glm.s2T2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.glm.s2T2) <- crs(envi)
plot(Pred.glm.s2T2, main= "Occurence Probability from 2000 to 2015 of Species 2 (s2_T2) by GLM")

##s2_T3
PresAbs.s2T3=s2_T3$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s2T3)<-~x+y
crs(PresAbs.s2T3)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s2T3<- raster::extract(envi, PresAbs.s2T3, df=T)

values.s2T3<- values.s2T3[, -1] # first column not need it

modSpecies.s2T3<-data.frame(pres= PresAbs.s2T3$Observed, values.s2T3[1:ncol(values.s2T3)])

preds<- envi
## Favourability and Probability
glm.s2T3<-multGLM(modSpecies.s2T3, sp.cols = 1, var.cols=2:ncol(modSpecies.s2T3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s2T3<- getPreds(preds, models=glm.s2T3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.glm.s2T3) <- crs(envi)
plot(Pred.glm.s2T3, main= "Occurence Probability from 2000 to 2023 of Species 2 (s2_T3) by GLM")


###########GAM####

## newdata for prediction

preds <- as.data.frame(envi) %>%drop_na() #Predictors[[myRandNum]] should be envi

predsXY <- as.data.frame(envi, xy=T) %>%drop_na()

###Non-overlapping time scale

## s2_t1
form_gam_s2t1 <- as.formula(paste0(names(modSpecies.s2t1)[1], "~", paste0("s(", names(modSpecies.s2t1)[2:ncol(modSpecies.s2t1)], ")", collapse = "+")))
gam.s2t1 <- gam(form_gam_s2t1, family = binomial, data = modSpecies.s2t1)
prediction.gam.s2t1<- predict(gam.s2t1, newdata = predsXY, type = "response")
df.pred.s2t1 <- data.frame(Pred=prediction.gam.s2t1)
prediction.gam.s2t1 <- data.frame(predsXY[,1:2], df.pred.s2t1$Pred)

Pred.gam.s2t1 <- rasterFromXYZ(prediction.gam.s2t1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s2t1, main=" Occurence Probability from 2000 to 2009 of Species 2 (s2_t1) by GAM")

## s2_t2
form_gam_s2t2 <- as.formula(paste0(names(modSpecies.s2t2)[1], "~", paste0("s(", names(modSpecies.s2t2)[2:ncol(modSpecies.s2t2)], ")", collapse = "+")))
gam.s2t2 <- gam(form_gam_s2t2, family = binomial, data = modSpecies.s2t2)
prediction.gam.s2t2<- predict(gam.s2t2, newdata = predsXY, type = "response")
df.pred.s2t2 <- data.frame(Pred=prediction.gam.s2t2)
prediction.gam.s2t2 <- data.frame(predsXY[,1:2], df.pred.s2t2$Pred)

Pred.gam.s2t2 <- rasterFromXYZ(prediction.gam.s2t2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s2t2, main=" Occurence Probability from 2010 to 2014 of Species 2 (s2_t2) by GAM")

## s2_t3
form_gam_s2t3 <- as.formula(paste0(names(modSpecies.s2t3)[1], "~", paste0("s(", names(modSpecies.s2t3)[2:ncol(modSpecies.s2t3)], ")", collapse = "+")))
gam.s2t3 <- gam(form_gam_s2t3, family = binomial, data = modSpecies.s2t3)
prediction.gam.s2t3<- predict(gam.s2t3, newdata = predsXY, type = "response")
df.pred.s2t3 <- data.frame(Pred=prediction.gam.s2t3)
prediction.gam.s2t3 <- data.frame(predsXY[,1:2], df.pred.s2t3$Pred)

Pred.gam.s2t3 <- rasterFromXYZ(prediction.gam.s2t3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s2t3, main=" Occurence Probability from 2015 to 2023 of Species 2 (s2_t3) by GAM")


###overlapping timescale###

## s2_T1
form_gam_s2T1 <- as.formula(paste0(names(modSpecies.s2T1)[1], "~", paste0("s(", names(modSpecies.s2T1)[2:ncol(modSpecies.s2T1)], ")", collapse = "+")))
gam.s2T1 <- gam(form_gam_s2T1, family = binomial, data = modSpecies.s2T1)
prediction.gam.s2T1<- predict(gam.s2T1, newdata = predsXY, type = "response")
df.pred.s2T1 <- data.frame(Pred=prediction.gam.s2T1)
prediction.gam.s2T1 <- data.frame(predsXY[,1:2], df.pred.s2T1$Pred)

Pred.gam.s2T1 <- rasterFromXYZ(prediction.gam.s2T1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s2T1, main=" Occurence Probability from 2000 to 2010 of Species 2 (s2_T1) by GAM")

## s2_T2
form_gam_s2T2 <- as.formula(paste0(names(modSpecies.s2T2)[1], "~", paste0("s(", names(modSpecies.s2T2)[2:ncol(modSpecies.s2T2)], ")", collapse = "+")))
gam.s2T2 <- gam(form_gam_s2T2, family = binomial, data = modSpecies.s2T2)
prediction.gam.s2T2<- predict(gam.s2T2, newdata = predsXY, type = "response")
df.pred.s2T2 <- data.frame(Pred=prediction.gam.s2T2)
prediction.gam.s2T2 <- data.frame(predsXY[,1:2], df.pred.s2T2$Pred)

Pred.gam.s2T2 <- rasterFromXYZ(prediction.gam.s2T2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s2T2, main=" Occurence Probability from 2000 to 2015 of Species 2 (s2_T2) by GAM")

## s2_T3
form_gam_s2T3 <- as.formula(paste0(names(modSpecies.s2T3)[1], "~", paste0("s(", names(modSpecies.s2T3)[2:ncol(modSpecies.s2T3)], ")", collapse = "+")))
gam.s2T3 <- gam(form_gam_s2T3, family = binomial, data = modSpecies.s2T3)
prediction.gam.s2T3<- predict(gam.s2T3, newdata = predsXY, type = "response")
df.pred.s2T3 <- data.frame(Pred=prediction.gam.s2T3)
prediction.gam.s2T3 <- data.frame(predsXY[,1:2], df.pred.s2T3$Pred)

Pred.gam.s2T3 <- rasterFromXYZ(prediction.gam.s2T3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s2T3, main=" Occurence Probability from 2000 to 2023 of Species 2 (s2_T3) by GAM")





#####Random Forest####

###Non-overlapping (t)

#s2_t1
rf.s2t1<-ranger(modSpecies.s2t1$pres ~., data= modSpecies.s2t1, importance='impurity') 
Pred.rf.s2t1<- predict(
  rf.s2t1,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s2t1$num.trees)

prediction.rf.s2t1 <- data.frame(predsXY[,1:2], Pred.rf.s2t1$predictions)
prediction.rf.s2t1$Pred.rf.s2t1.predictions[ prediction.rf.s2t1$Pred.predictions == 1] <- 1 - 2.2e-16  #so that the probability is not 100% (too arrogant) but a smaller amount that is the most significant close to 1
Pred.rf.s2t1 <- rasterFromXYZ(prediction.rf.s2t1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s2t1, main= " Occurence Probability from 2000-2009 of Species 2 (s2_t1) by Random Forest")


#s2_t2
rf.s2t2<-ranger(modSpecies.s2t2$pres ~., data= modSpecies.s2t2, importance='impurity') 
Pred.rf.s2t2<- predict(
  rf.s2t2,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s2t2$num.trees)

prediction.rf.s2t2 <- data.frame(predsXY[,1:2], Pred.rf.s2t2$predictions)
prediction.rf.s2t2$Pred.rf.s2t2.predictions[ prediction.rf.s2t2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s2t2 <- rasterFromXYZ(prediction.rf.s2t2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s2t2, main= " Occurence Probability 2010-2014 of Species 2 (s2_t2) by Random Forest")

#s2_t3
rf.s2t3<-ranger(modSpecies.s2t3$pres ~., data= modSpecies.s2t3, importance='impurity') 
Pred.rf.s2t3<- predict(
  rf.s2t3,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s2t3$num.trees)

prediction.rf.s2t3 <- data.frame(predsXY[,1:2], Pred.rf.s2t3$predictions)
prediction.rf.s2t3$Pred.rf.s2t3.predictions[ prediction.rf.s2t3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s2t3 <- rasterFromXYZ(prediction.rf.s2t3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s2t3, main= " Occurence Probability from 2015-2023 of Species 2 (s2_t3) by Random Forest")



## overlapping timescale (T)
#s2_T1
rf.s2T1<-ranger(modSpecies.s2T1$pres ~., data= modSpecies.s2T1, importance='impurity') 
Pred.rf.s2T1<- predict(
  rf.s2T1,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s2T1$num.trees)

prediction.rf.s2T1 <- data.frame(predsXY[,1:2], Pred.rf.s2T1$predictions)
prediction.rf.s2T1$Pred.rf.s2T1.predictions[ prediction.rf.s2T1$Pred.predictions == 1] <- 1 - 2.2e-16  #so that the probability is not 100% (too arrogant) but a smaller amount that is the most significant close to 1
Pred.rf.s2T1 <- rasterFromXYZ(prediction.rf.s2T1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s2T1, main= " Occurence Probability from 2000-2010 of Species 2 (s2_T1) by Random Forest")


#s2_T2
rf.s2T2<-ranger(modSpecies.s2T2$pres ~., data= modSpecies.s2T2, importance='impurity') 
Pred.rf.s2T2<- predict(
  rf.s2T2,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s2T2$num.trees)

prediction.rf.s2T2 <- data.frame(predsXY[,1:2], Pred.rf.s2T2$predictions)
prediction.rf.s2T2$Pred.rf.s2T2.predictions[ prediction.rf.s2T2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s2T2 <- rasterFromXYZ(prediction.rf.s2T2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s2T2, main= " Occurence Probability from 2000-2015 of Species 2 (s2_T2) by Random Forest")

#s2_T3
rf.s2T3<-ranger(modSpecies.s2T3$pres ~., data= modSpecies.s2T3, importance='impurity') 
Pred.rf.s2T3<- predict(
  rf.s2T3,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s2T3$num.trees)

prediction.rf.s2T3 <- data.frame(predsXY[,1:2], Pred.rf.s2T3$predictions)
prediction.rf.s2T3$Pred.rf.s2T3.predictions[ prediction.rf.s2T3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s2T3 <- rasterFromXYZ(prediction.rf.s2T3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s2T3, main= " Occurence Probability from 2000-2023 of Species 2 (s2_T3) by Random Forest")



#############EVALUATION PERFORMANCE##########
auc.gam.s2.t1<- AUCFunzGAM(modSpecies.s2t1, TrainValue= 0.6, TestValue = 0.4) # because the modSpecies.s2t1 has too few no. of occ, so if we partioning the test by 0.2, there is not a chance it got the binary of the Pred => must increase the test partion to get both values
auc.gam.s2.t2<- AUCFunzGAM(modSpecies.s2t2)
auc.gam.s2.t3<- AUCFunzGAM(modSpecies.s2t3)

auc.gam.s2.T1<- AUCFunzGAM(modSpecies.s2T1)
auc.gam.s2.T2<- AUCFunzGAM(modSpecies.s2T2)
auc.gam.s2.T3<- AUCFunzGAM(modSpecies.s2T3)



auc.glm.s2.t1<- AUCFunzGLM(modSpecies.s2t1, TrainValue = 0.6, TestValue = 0.4)
auc.glm.s2.t2<- AUCFunzGLM(modSpecies.s2t2)
auc.glm.s2.t3<- AUCFunzGLM(modSpecies.s2t3)

auc.glm.s2.T1<- AUCFunzGLM(modSpecies.s2T1)
auc.glm.s2.T2<- AUCFunzGLM(modSpecies.s2T2)
auc.glm.s2.T3<- AUCFunzGLM(modSpecies.s2T3)



auc.rf.s2.t1<- AUCFunzRF(modSpecies.s2t1, TrainValue = 0.6, TestValue = 0.4)
auc.rf.s2.t2<- AUCFunzRF(modSpecies.s2t2)
auc.rf.s2.t3<- AUCFunzRF(modSpecies.s2t3)

auc.rf.s2.T1<- AUCFunzRF(modSpecies.s2T1)
auc.rf.s2.T2<- AUCFunzRF(modSpecies.s2T2)
auc.rf.s2.T3<- AUCFunzRF(modSpecies.s2T3)



tss.glm.s2.t1<- TSSFunzGLM(modSpecies.s2t1, TrainValue = 0.6, TestValue = 0.4)
tss.glm.s2.t2<- TSSFunzGLM(modSpecies.s2t2)
tss.glm.s2.t3<- TSSFunzGLM(modSpecies.s2t3)

tss.glm.s2.T1<- TSSFunzGLM(modSpecies.s2T1)
tss.glm.s2.T2<- TSSFunzGLM(modSpecies.s2T2)
tss.glm.s2.T3<- TSSFunzGLM(modSpecies.s2T3)



tss.gam.s2.t1<- TSSFunzGAM(modSpecies.s2t1, TrainValue = 0.6, TestValue = 0.4)
tss.gam.s2.t2<- TSSFunzGAM(modSpecies.s2t2)
tss.gam.s2.t3<- TSSFunzGAM(modSpecies.s2t3)

tss.gam.s2.T1<- TSSFunzGAM(modSpecies.s2T1)
tss.gam.s2.T2<- TSSFunzGAM(modSpecies.s2T2)
tss.gam.s2.T3<- TSSFunzGAM(modSpecies.s2T3)



tss.rf.s2.t1<- TSSFunzRF(modSpecies.s2t1, TrainValue = 0.6, TestValue = 0.4)
tss.rf.s2.t2<- TSSFunzRF(modSpecies.s2t2)
tss.rf.s2.t3<- TSSFunzRF(modSpecies.s2t3)

tss.rf.s2.T1<- TSSFunzRF(modSpecies.s2T1)
tss.rf.s2.T2<- TSSFunzRF(modSpecies.s2T2)
tss.rf.s2.T3<- TSSFunzRF(modSpecies.s2T3)



boyce.glm.s2.t1<- BoyceGLM(modSpecies.s2t1, TrainValue = 0.6, TestValue = 0.4)
boyce.glm.s2.t2<- BoyceGLM(modSpecies.s2t2)
boyce.glm.s2.t3<- BoyceGLM(modSpecies.s2t3)

boyce.glm.s2.T1<- BoyceGLM(modSpecies.s2T1)
boyce.glm.s2.T2<- BoyceGLM(modSpecies.s2T2)
boyce.glm.s2.T3<- BoyceGLM(modSpecies.s2T3)


boyce.gam.s2.t1<- BoyceGAM(modSpecies.s2t1, TrainValue = 0.6, TestValue = 0.4)
boyce.gam.s2.t2<- BoyceGAM(modSpecies.s2t2)
boyce.gam.s2.t3<- BoyceGAM(modSpecies.s2t3)

boyce.gam.s2.T1<- BoyceGAM(modSpecies.s2T1)
boyce.gam.s2.T2<- BoyceGAM(modSpecies.s2T2)
boyce.gam.s2.T3<- BoyceGAM(modSpecies.s2T3)


boyce.rf.s2.t1<- BoyceRF(modSpecies.s2t1, TrainValue = 0.6, TestValue = 0.4)
boyce.rf.s2.t2<- BoyceRF(modSpecies.s2t2)
boyce.rf.s2.t3<- BoyceRF(modSpecies.s2t3)

boyce.rf.s2.T1<- BoyceRF(modSpecies.s2T1)
boyce.rf.s2.T2<- BoyceRF(modSpecies.s2T2)
boyce.rf.s2.T3<- BoyceRF(modSpecies.s2T3)



##better GPT rcm
algorithm<- rep(c("GAM", "GLM", "RF"), each=6)
temporal_setting<-rep(c("t1", "t2", "t3", 'T1', "T2", "T3"), times = 3)
AUC <- c(auc.gam.s2.t1,auc.gam.s2.T1, auc.gam.s2.t2, auc.gam.s2.T2, auc.gam.s2.t3, auc.gam.s2.T3,
         auc.glm.s2.t1,auc.glm.s2.T1, auc.glm.s2.t2, auc.glm.s2.T2, auc.glm.s2.t3, auc.glm.s2.T3,
         auc.rf.s2.t1,auc.rf.s2.T1, auc.rf.s2.t2, auc.rf.s2.T2, auc.rf.s2.t3, auc.rf.s2.T3)
TSS<- c(tss.gam.s2.t1, tss.gam.s2.T1, tss.gam.s2.t2, tss.gam.s2.T2, tss.gam.s2.t3, tss.gam.s2.T3,
        tss.glm.s2.t1, tss.glm.s2.T1, tss.glm.s2.t2, tss.glm.s2.T2, tss.glm.s2.t3, tss.glm.s2.T3,
        tss.rf.s2.t1, tss.rf.s2.T1, tss.rf.s2.t2, tss.rf.s2.T2, tss.rf.s2.t3, tss.rf.s2.T3)
Boyce<- c(boyce.gam.s2.t1, boyce.gam.s2.T1, boyce.gam.s2.t2, boyce.gam.s2.T2, boyce.gam.s2.t3, boyce.gam.s2.T3,
          boyce.glm.s2.t1, boyce.glm.s2.T1, boyce.glm.s2.t2, boyce.glm.s2.T2, boyce.glm.s2.t3, boyce.glm.s2.T3,
          boyce.rf.s2.t1, boyce.rf.s2.T1, boyce.rf.s2.t2, boyce.rf.s2.T2, boyce.rf.s2.t3, boyce.rf.s2.T3)
perf_df <- data.frame(algorithm, temporal_setting, AUC, TSS, Boyce)

melted_perf <- perf_df %>% pivot_longer(cols = c(AUC, TSS, Boyce), names_to = "metric", values_to = "value")

ggplot(melted_perf, aes(x = algorithm, y = value, fill = metric)) +
       geom_boxplot() +
       facet_wrap(~metric, scales = "free_y", ncol = 1) +
       labs(title = "Performance of SDM Algorithms", x = "Algorithms", y = "Performance") +
       scale_fill_manual(values = c("blue", "red", "green")) +
       theme_minimal() 
