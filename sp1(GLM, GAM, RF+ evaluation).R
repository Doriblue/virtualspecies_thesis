

####Species1: Gấu chó####

para_s1<- formatFunctions(temp=c(fun='dnorm', mean = 8.25, sd = 8.25),
                       preci=c(fun='dnorm', mean =406, sd=350),
                       elev= c(fun='dnorm', mean=2100, sd=900))

s1<- generateSpFromFun(raster.stack = envi[[c("temp", "preci", "elev")]],parameters = para_s1, plot = TRUE)
s1_pa <- convertToPA(s1, prob.method="logistic",alpha = -0.05, beta = "random", species.prevalence= 0.05)

s1_suit<-plotSuitabilityToProba(s1_pa)
s1_res<-plotResponse(s1_pa)

#T=sum, overlap
s1_T1 <- sampleOccurrences(s1_pa,n = 574,type = "presence-absence", extract.probability = TRUE,plot = F)
s1_T2 <- sampleOccurrences(s1_pa,n = 1297,type = "presence-absence", extract.probability = TRUE,plot = F)
s1_T3 <- sampleOccurrences(s1_pa,n = 8032,type = "presence-absence", extract.probability = TRUE,plot = F)
#t=single range, not overlap
s1_t1 <- sampleOccurrences(s1_pa,n = 564,type = "presence-absence", extract.probability = TRUE,plot = F)
s1_t2 <- sampleOccurrences(s1_pa,n = 669,type = "presence-absence", extract.probability = TRUE,plot = F)
s1_t3 <- sampleOccurrences(s1_pa,n = 6801,type = "presence-absence", extract.probability = TRUE,plot = F)




############GLM############
###non overlapping time scale (t)
##s1_t1
#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s1t1=s1_t1$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s1t1)<-~x+y
crs(PresAbs.s1t1)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s1t1<- raster::extract(envi, PresAbs.s1t1, df=T)

values.s1t1<- values.s1t1[, -1] # first column not need it

modSpecies.s1t1<-data.frame(pres= PresAbs.s1t1@data[,1], values.s1t1[1:ncol(values.s1t1)])

preds<- envi
#Favourability and Probability
Model.s1t1<-multGLM(modSpecies.s1t1, sp.cols = 1, var.cols=2:ncol(modSpecies.s1t1), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

#get the predictions for occurence probability from GLM and the environmental condition provided at the begining
Pred.s1t1<- getPreds(preds, models=Model.s1t1$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.s1t1) <- crs(envi)
plot(Pred.s1t1, main= "Occurence Probability from 1960-1979 of Species 1 (s1_t1) by GLM")



##s1_t2
#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s1t2=s1_t2$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s1t2)<-~x+y
crs(PresAbs.s1t2)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s1t2<- raster::extract(envi, PresAbs.s1t2, df=T)

values.s1t2<- values.s1t2[, -1] # first column not need it

modSpecies.s1t2<-data.frame(pres= PresAbs.s1t2@data[,1], values.s1t2[1:ncol(values.s1t2)])

preds<- envi
## Favourability and Probability
Model.s1t2<-multGLM(modSpecies.s1t2, sp.cols = 1, var.cols=2:ncol(modSpecies.s1t2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.s1t2<- getPreds(preds, models=Model.s1t2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.s1t2) <- crs(envi)
plot(Pred.s1t2, main= "Occurence Probability from 1980 to 1999 of Species 1 (s1_t2) by GLM")



#s1_t3
#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s1t3=s1_t3$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s1t3)<-~x+y
crs(PresAbs.s1t3)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s1t3<- raster::extract(envi, PresAbs.s1t3, df=T)

values.s1t3<- values.s1t3[, -1] # first column not need it

modSpecies.s1t3<-data.frame(pres= PresAbs.s1t3@data[,1], values.s1t3[1:ncol(values.s1t3)])

preds<- envi
## Favourability and Probability
Model.s1t3<-multGLM(modSpecies.s1t3, sp.cols = 1, var.cols=2:ncol(modSpecies.s1t3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.s1t3<- getPreds(preds, models=Model.s1t3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.s1t3) <- crs(envi)
plot(Pred.s1t3, main= "Occurence Probability from 2000 to 2020 of Species 1 (s1_t3) by GLM")



###overlapping timescale (T)####
##s1_T1

#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s1T1=s1_T1$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s1T1)<-~x+y
crs(PresAbs.s1T1)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s1T1<- raster::extract(envi, PresAbs.s1T1, df=T)

values.s1T1<- values.s1T1[, -1] # first column not need it

modSpecies.s1T1<-data.frame(pres= PresAbs.s1T1@data[,1], values.s1T1[1:ncol(values.s1T1)])

preds<- envi
## Favourability and Probability
Model.s1T1<-multGLM(modSpecies.s1T1, sp.cols = 1, var.cols=2:ncol(modSpecies.s1T1), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.s1T1<- getPreds(preds, models=Model.s1T1$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.s1T1) <- crs(envi)
plot(Pred.s1T1, main= "Occurence Probability from 1960 to 1980 of Species 1 (s1_T1) by GLM")


##s1_T2
PresAbs.s1T2=s1_T2$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s1T2)<-~x+y
crs(PresAbs.s1T2)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s1T2<- raster::extract(envi, PresAbs.s1T2, df=T)

values.s1T2<- values.s1T2[, -1] # first column not need it

modSpecies.s1T2<-data.frame(pres= PresAbs.s1T2@data[,1], values.s1T2[1:ncol(values.s1T2)])

preds<- envi
## Favourability and Probability
Model.s1T2<-multGLM(modSpecies.s1T2, sp.cols = 1, var.cols=2:ncol(modSpecies.s1T2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.s1T2<- getPreds(preds, models=Model.s1T2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.s1T2) <- crs(envi)
plot(Pred.s1T2, main= "Occurence Probability from 1960 to 2000 of Species 1 (s1_T2) by GLM")

##s1_T3
PresAbs.s1T3=s1_T3$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s1T3)<-~x+y
crs(PresAbs.s1T3)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s1T3<- raster::extract(envi, PresAbs.s1T3, df=T)

values.s1T3<- values.s1T3[, -1] # first column not need it

modSpecies.s1T3<-data.frame(pres= PresAbs.s1T3@data[,1], values.s1T3[1:ncol(values.s1T3)])

preds<- envi
## Favourability and Probability
Model.s1T3<-multGLM(modSpecies.s1T3, sp.cols = 1, var.cols=2:ncol(modSpecies.s1T3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.s1T3<- getPreds(preds, models=Model.s1T3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.s1T3) <- crs(envi)
plot(Pred.s1T3, main= "Occurence Probability from 1960 to 2020 of Species 1 (s1_T3) by GLM")


###########GAM############

## newdata for prediction

preds <- as.data.frame(envi) %>%drop_na() #Predictors[[myRandNum]] should be envi

predsXY <- as.data.frame(envi, xy=T) %>%drop_na()

###Non-overlapping time scale

## s1_t1
form_gam_s1t1 <- as.formula(paste0(names(modSpecies.s1t1)[1], "~", paste0("s(", names(modSpecies.s1t1)[2:4], ")", collapse = "+")))
gam.s1t1 <- gam(form_gam_s1t1, family = binomial, data = modSpecies.s1t1)
prediction.s1t1<- predict(gam.s1t1, newdata = preds, type = "response")
df.pred.s1t1 <- data.frame(Pred=prediction.s1t1)
prediction.s1t1 <- data.frame(predsXY[,1:2], df.pred.s1t1$Pred)

Pred.gam.s1t1 <- rasterFromXYZ(prediction.s1t1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s1t1, main=" Occurence Probability from 1960 to 1979 of Species 1 (s1_t1) by GAM")

## s1_t2
form_gam_s1t2 <- as.formula(paste0(names(modSpecies.s1t2)[1], "~", paste0("s(", names(modSpecies.s1t2)[2:4], ")", collapse = "+")))
gam.s1t2 <- gam(form_gam_s1t2, family = binomial, data = modSpecies.s1t2)
prediction.s1t2<- predict(gam.s1t2, newdata = preds, type = "response")
df.pred.s1t2 <- data.frame(Pred=prediction.s1t2)
prediction.s1t2 <- data.frame(predsXY[,1:2], df.pred.s1t2$Pred)

Pred.gam.s1t2 <- rasterFromXYZ(prediction.s1t2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s1t2, main=" Occurence Probability from 1980 to 1999 of Species 1 (s1_t2) by GAM")

## s1_t3
form_gam_s1t3 <- as.formula(paste0(names(modSpecies.s1t3)[1], "~", paste0("s(", names(modSpecies.s1t3)[2:4], ")", collapse = "+")))
gam.s1t3 <- gam(form_gam_s1t3, family = binomial, data = modSpecies.s1t3)
prediction.s1t3<- predict(gam.s1t3, newdata = preds, type = "response")
df.pred.s1t3 <- data.frame(Pred=prediction.s1t3)
prediction.s1t3 <- data.frame(predsXY[,1:2], df.pred.s1t3$Pred)

Pred.gam.s1t3 <- rasterFromXYZ(prediction.s1t3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s1t3, main=" Occurence Probability from 2000 to 2020 of Species 1 (s1_t3) by GAM")


###overlapping timescale###

## s1_T1
form_gam_s1T1 <- as.formula(paste0(names(modSpecies.s1T1)[1], "~", paste0("s(", names(modSpecies.s1T1)[2:4], ")", collapse = "+")))
gam.s1T1 <- gam(form_gam_s1T1, family = binomial, data = modSpecies.s1T1)
prediction.s1T1<- predict(gam.s1T1, newdata = preds, type = "response")
df.pred.s1T1 <- data.frame(Pred=prediction.s1T1)
prediction.s1T1 <- data.frame(predsXY[,1:2], df.pred.s1T1$Pred)

Pred.gam.s1T1 <- rasterFromXYZ(prediction.s1T1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s1T1, main=" Occurence Probability from 1960 to 1979 of Species 1 (s1_T1) by GAM")

## s1_t2
form_gam_s1T2 <- as.formula(paste0(names(modSpecies.s1T2)[1], "~", paste0("s(", names(modSpecies.s1T2)[2:4], ")", collapse = "+")))
gam.s1T2 <- gam(form_gam_s1T2, family = binomial, data = modSpecies.s1T2)
prediction.s1T2<- predict(gam.s1T2, newdata = preds, type = "response")
df.pred.s1T2 <- data.frame(Pred=prediction.s1T2)
prediction.s1T2 <- data.frame(predsXY[,1:2], df.pred.s1T2$Pred)

Pred.gam.s1T2 <- rasterFromXYZ(prediction.s1T2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s1T2, main=" Occurence Probability from 1960 to 1999 of Species 1 (s1_T2) by GAM")

## s1_T3
form_gam_s1T3 <- as.formula(paste0(names(modSpecies.s1T3)[1], "~", paste0("s(", names(modSpecies.s1T3)[2:4], ")", collapse = "+")))
gam.s1T3 <- gam(form_gam_s1T3, family = binomial, data = modSpecies.s1T3)
prediction.s1T3<- predict(gam.s1T3, newdata = preds, type = "response")
df.pred.s1T3 <- data.frame(Pred=prediction.s1T3)
prediction.s1T3 <- data.frame(predsXY[,1:2], df.pred.s1T3$Pred)

Pred.gam.s1T3 <- rasterFromXYZ(prediction.s1T3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s1T3, main=" Occurence Probability from 1960 to 2020 of Species 1 (s1_T3) by GAM")





#########Random Forest#############

###Non-overlapping (t)

#s1_t1
rf.s1t1<-ranger(modSpecies.s1t1$pres ~., data= modSpecies.s1t1, importance='impurity') 
Pred.rf.s1t1<- predict(
  rf.s1t1,
  data =preds,
  predict.all = FALSE,
  num.trees = rf.s1t1$num.trees)

prediction.rf.s1t1 <- data.frame(predsXY[,1:2], Pred.rf.s1t1$predictions)
prediction.rf.s1t1$Pred.rf.s1t1.predictions[ prediction.rf.s1t1$Pred.predictions == 1] <- 1 - 2.2e-16  #so that the probability is not 100% (too arrogant) but a smaller amount that is the most significant close to 1
Pred.rf.s1t1 <- rasterFromXYZ(prediction.rf.s1t1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s1t1, main= " Occurence Probability from 1960-1979 of Species 1 (s1_t1) by Random Forest")


#s1_t2
rf.s1t2<-ranger(modSpecies.s1t2$pres ~., data= modSpecies.s1t2, importance='impurity') 
Pred.rf.s1t2<- predict(
  rf.s1t2,
  data =preds,
  predict.all = FALSE,
  num.trees = rf.s1t2$num.trees)

prediction.rf.s1t2 <- data.frame(predsXY[,1:2], Pred.rf.s1t2$predictions)
prediction.rf.s1t2$Pred.rf.s1t2.predictions[ prediction.rf.s1t2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s1t2 <- rasterFromXYZ(prediction.rf.s1t2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s1t2, main= " Occurence Probability from 1980-1999 of Species 1 (s1_t2) by Random Forest")

#s1_t3
rf.s1t3<-ranger(modSpecies.s1t3$pres ~., data= modSpecies.s1t3, importance='impurity') 
Pred.rf.s1t3<- predict(
  rf.s1t3,
  data =preds,
  predict.all = FALSE,
  num.trees = rf.s1t3$num.trees)

prediction.rf.s1t3 <- data.frame(predsXY[,1:2], Pred.rf.s1t3$predictions)
prediction.rf.s1t3$Pred.rf.s1t3.predictions[ prediction.rf.s1t3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s1t3 <- rasterFromXYZ(prediction.rf.s1t3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s1t3, main= " Occurence Probability from 2000-2020 of Species 1 (s1_t3) by Random Forest")



## overlapping timescale (T)
#s1_T1
rf.s1T1<-ranger(modSpecies.s1T1$pres ~., data= modSpecies.s1T1, importance='impurity') 
Pred.rf.s1T1<- predict(
  rf.s1T1,
  data =preds,
  predict.all = FALSE,
  num.trees = rf.s1T1$num.trees)

prediction.rf.s1T1 <- data.frame(predsXY[,1:2], Pred.rf.s1T1$predictions)
prediction.rf.s1T1$Pred.rf.s1T1.predictions[ prediction.rf.s1T1$Pred.predictions == 1] <- 1 - 2.2e-16  #so that the probability is not 100% (too arrogant) but a smaller amount that is the most significant close to 1
Pred.rf.s1T1 <- rasterFromXYZ(prediction.rf.s1T1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s1T1, main= " Occurence Probability from 1960-1980 of Species 1 (s1_T1) by Random Forest")


#s1_T2
rf.s1T2<-ranger(modSpecies.s1T2$pres ~., data= modSpecies.s1T2, importance='impurity') 
Pred.rf.s1T2<- predict(
  rf.s1T2,
  data =preds,
  predict.all = FALSE,
  num.trees = rf.s1T2$num.trees)

prediction.rf.s1T2 <- data.frame(predsXY[,1:2], Pred.rf.s1T2$predictions)
prediction.rf.s1T2$Pred.rf.s1T2.predictions[ prediction.rf.s1T2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s1T2 <- rasterFromXYZ(prediction.rf.s1T2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s1T2, main= " Occurence Probability from 1960-2000 of Species 1 (s1_T2) by Random Forest")

#s1_T3
rf.s1T3<-ranger(modSpecies.s1T3$pres ~., data= modSpecies.s1T3, importance='impurity') 
Pred.rf.s1T3<- predict(
  rf.s1T3,
  data =preds,
  predict.all = FALSE,
  num.trees = rf.s1T3$num.trees)

prediction.rf.s1T3 <- data.frame(predsXY[,1:2], Pred.rf.s1T3$predictions)
prediction.rf.s1T3$Pred.rf.s1T3.predictions[ prediction.rf.s1T3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s1T3 <- rasterFromXYZ(prediction.rf.s1T3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s1T3, main= " Occurence Probability from 1960-2020 of Species 1 (s1_T3) by Random Forest")

#############EVALUATION PERFORMANCE##########
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



#########Evaluation####
algorithm<- rep(c("GAM", "GLM", "RF"), each=6)
temporal_setting<-rep(c("t1", "t2", "t3", 'T1', "T2", "T3"), times = 3)
AUC <- c(auc.gam.s1.t1,auc.gam.s1.T1, auc.gam.s1.t2, auc.gam.s1.T2, auc.gam.s1.t3, auc.gam.s1.T3,
         auc.glm.s1.t1,auc.glm.s1.T1, auc.glm.s1.t2, auc.glm.s1.T2, auc.glm.s1.t3, auc.glm.s1.T3,
         auc.rf.s1.t1,auc.rf.s1.T1, auc.rf.s1.t2, auc.rf.s1.T2, auc.rf.s1.t3, auc.rf.s1.T3)
TSS<- c(tss.gam.s1.t1, tss.gam.s1.T1, tss.gam.s1.t2, tss.gam.s1.T2, tss.gam.s1.t3, tss.gam.s1.T3,
        tss.glm.s1.t1, tss.glm.s1.T1, tss.glm.s1.t2, tss.glm.s1.T2, tss.glm.s1.t3, tss.glm.s1.T3,
        tss.rf.s1.t1, tss.rf.s1.T1, tss.rf.s1.t2, tss.rf.s1.T2, tss.rf.s1.t3, tss.rf.s1.T3)
Boyce<- c(boyce.gam.s1.t1, boyce.gam.s1.T1, boyce.gam.s1.t2, boyce.gam.s1.T2, boyce.gam.s1.t3, boyce.gam.s1.T3,
          boyce.glm.s1.t1, boyce.glm.s1.T1, boyce.glm.s1.t2, boyce.glm.s1.T2, boyce.glm.s1.t3, boyce.glm.s1.T3,
          boyce.rf.s1.t1, boyce.rf.s1.T1, boyce.rf.s1.t2, boyce.rf.s1.T2, boyce.rf.s1.t3, boyce.rf.s1.T3)
perf_df <- data.frame(algorithm, temporal_setting, AUC, TSS, Boyce)

melted_perf <- perf_df %>% pivot_longer(cols = c(AUC, TSS, Boyce), names_to = "metric", values_to = "value")

ggplot(melted_perf, aes(x = algorithm, y = value, fill = metric)) +
       geom_boxplot() +
       facet_wrap(~metric, scales = "free_y", ncol = 1) +
       labs(title = "Performance of SDM Algorithms", x = "Algorithms", y = "Performance") +
       scale_fill_manual(values = c("blue", "red", "green")) +
       theme_minimal() 



