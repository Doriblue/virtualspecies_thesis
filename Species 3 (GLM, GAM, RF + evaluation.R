para_s3<- formatFunctions(temp=c(fun='dnorm', mean = 15.8, sd = 0.50),
                          preci=c(fun='dnorm', mean =1455, sd=500),
                          elev= c(fun='dnorm', mean=1600, sd=1400))

s3<- generateSpFromFun(raster.stack = envi[[c("temp", "preci", "elev")]],parameters = para_s3, plot = TRUE)
s3_pa <- convertToPA(s3, prob.method="logistic",alpha = -0.05, beta = "random", species.prevalence= 0.3)

s3_suit<-plotSuitabilityToProba(s3_pa)
s3_res<-plotResponse(s3_pa)

#T=sum, overlap
s3_T1 <- sampleOccurrences(s3_pa,n = 64,type = "presence-absence", extract.probability = TRUE,plot = T)
s3_T2 <- sampleOccurrences(s3_pa,n = 210,type = "presence-absence", extract.probability = TRUE,plot = F)
s3_T3 <- sampleOccurrences(s3_pa,n = 294,type = "presence-absence", extract.probability = TRUE,plot = F, replacement = T)
#t=single range, not overlap
s3_t1 <- sampleOccurrences(s3_pa,n = 51,type = "presence-absence", extract.probability = TRUE,plot = F)
s3_t2 <- sampleOccurrences(s3_pa,n = 128,type = "presence-absence", extract.probability = TRUE,plot = F)
s3_t3 <- sampleOccurrences(s3_pa,n = 115,type = "presence-absence", extract.probability = TRUE,plot = F, replacement = T)

########GLM########
####non overlapping time scale (t)
##s3_t1
#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s3t1=s3_t1$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s3t1)<-~x+y
crs(PresAbs.s3t1)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s3t1<- raster::extract(envi, PresAbs.s3t1, df=T)

values.s3t1<- values.s3t1[, -1] # first column not need it

modSpecies.s3t1<-data.frame(pres= PresAbs.s3t1$Observed, values.s3t1[1:ncol(values.s3t1)])

preds<- envi
## Favourability and Probability
glm.s3t1<-multGLM(modSpecies.s3t1, sp.cols = 1, var.cols=2:ncol(modSpecies.s3t1), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s3t1<- getPreds(preds, models=glm.s3t1$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

prediction.glm.s3t1<- data.frame(glm.s3t1$predictions)



crs(Pred.glm.s3t1) <- crs(envi)
plot(Pred.glm.s3t1, main= "Occurence Probability from 2000 to 2014 of Species 3 (s3_t1) by GLM")


##s3_t2
#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s3t2=s3_t2$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s3t2)<-~x+y
crs(PresAbs.s3t2)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s3t2<- raster::extract(envi, PresAbs.s3t2, df=T)

values.s3t2<- values.s3t2[, -1] # first column not need it

modSpecies.s3t2<-data.frame(pres= PresAbs.s3t2$Observed, values.s3t2[1:ncol(values.s3t2)])

preds<- envi
## Favourability and Probability
glm.s3t2<-multGLM(modSpecies.s3t2, sp.cols = 1, var.cols=2:ncol(modSpecies.s3t2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s3t2<- getPreds(preds, models=glm.s3t2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.glm.s3t2) <- crs(envi)
plot(Pred.glm.s3t2, main= "Occurence Probability from 2015 to 2019 of Species 3 (s3_t2) by GLM")


#s3_t3
#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s3t3=s3_t3$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s3t3)<-~x+y
crs(PresAbs.s3t3)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s3t3<- raster::extract(envi, PresAbs.s3t3, df=T)

values.s3t3<- values.s3t3[, -1] # first column not need it

modSpecies.s3t3<-data.frame(pres= PresAbs.s3t3$Observed, values.s3t3[1:ncol(values.s3t3)])

preds<- envi
## Favourability and Probability
glm.s3t3<-multGLM(modSpecies.s3t3, sp.cols = 1, var.cols=2:ncol(modSpecies.s3t3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s3t3<- getPreds(preds, models=glm.s3t3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.glm.s3t3) <- crs(envi)
plot(Pred.glm.s3t3, main= "Occurence Probability from 2020 to 2023 of Species 3 (s3_t3) by GLM")





####overlapping timescale (T)
##s3_T1

#extract PA points, then exchange it into geographical vari by giving it coordinates and crs
PresAbs.s3T1=s3_T1$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s3T1)<-~x+y
crs(PresAbs.s3T1)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s3T1<- raster::extract(envi, PresAbs.s3T1, df=T)

values.s3T1<- values.s3T1[, -1] # first column not need it

modSpecies.s3T1<-data.frame(pres= PresAbs.s3T1$Observed,values.s3T1[1:ncol(values.s3T1)])

preds<- envi
## Favourability and Probability
glm.s3T1<-multGLM(modSpecies.s3T1, sp.cols = 1, var.cols=2:ncol(modSpecies.s3T1), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s3T1<- getPreds(preds, models=glm.s3T1$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.glm.s3T1) <- crs(envi)
plot(Pred.glm.s3T1, main= "Occurence Probability from 2000 to 2015 of Species 3 (s3_T1) by GLM")


##s3_T2
PresAbs.s3T2=s3_T2$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s3T2)<-~x+y
crs(PresAbs.s3T2)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s3T2<- raster::extract(envi, PresAbs.s3T2, df=T)

values.s3T2<- values.s3T2[, -1] # first column not need it

modSpecies.s3T2<-data.frame(pres= PresAbs.s3T2$Observed, values.s3T2[1:ncol(values.s3T2)])

preds<- envi
## Favourability and Probability
glm.s3T2<-multGLM(modSpecies.s3T2, sp.cols = 1, var.cols=2:ncol(modSpecies.s3T2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s3T2<- getPreds(preds, models=glm.s3T2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.glm.s3T2) <- crs(envi)
plot(Pred.glm.s3T2, main= "Occurence Probability from 2000 to 2020 of Species 3 (s3_T2) by GLM")

##s3_T3
PresAbs.s3T3=s3_T3$sample.points[, c( "x", "y", "Observed")]
coordinates(PresAbs.s3T3)<-~x+y
crs(PresAbs.s3T3)<-crs(envi)

#extract the envi vari value in the coordinates of each sample points 
values.s3T3<- raster::extract(envi, PresAbs.s3T3, df=T)

values.s3T3<- values.s3T3[, -1] # first column not need it

modSpecies.s3T3<-data.frame(pres= PresAbs.s3T3$Observed, values.s3T3[1:ncol(values.s3T3)])

preds<- envi
## Favourability and Probability
glm.s3T3<-multGLM(modSpecies.s3T3, sp.cols = 1, var.cols=2:ncol(modSpecies.s3T3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE) 

Pred.glm.s3T3<- getPreds(preds, models=glm.s3T3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)

crs(Pred.glm.s3T3) <- crs(envi)
plot(Pred.glm.s3T3, main= "Occurence Probability from 2000 to 2023 of Species 3 (s3_T3) by GLM")


###########GAM####

## newdata for prediction

preds <- as.data.frame(envi) %>%drop_na() #Predictors[[myRandNum]] should be envi

predsXY <- as.data.frame(envi, xy=T) %>%drop_na()

###Non-overlapping time scale

## s3_t1
form_gam_s3t1 <- as.formula(paste0(names(modSpecies.s3t1)[1], "~", paste0("s(", names(modSpecies.s3t1)[2:ncol(modSpecies.s3t1)], ")", collapse = "+")))
gam.s3t1 <- gam(form_gam_s3t1, family = binomial, data = modSpecies.s3t1)
prediction.gam.s3t1<- predict(gam.s3t1, newdata = predsXY, type = "response")
df.pred.s3t1 <- data.frame(Pred=prediction.gam.s3t1)
prediction.gam.s3t1 <- data.frame(predsXY[,1:2], df.pred.s3t1$Pred)

Pred.gam.s3t1 <- rasterFromXYZ(prediction.gam.s3t1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s3t1, main=" Occurence Probability from 2000 to 2014 of Species 3 (s3_t1) by GAM")

## s3_t2
form_gam_s3t2 <- as.formula(paste0(names(modSpecies.s3t2)[1], "~", paste0("s(", names(modSpecies.s3t2)[2:ncol(modSpecies.s3t2)], ")", collapse = "+")))
gam.s3t2 <- gam(form_gam_s3t2, family = binomial, data = modSpecies.s3t2)
prediction.gam.s3t2<- predict(gam.s3t2, newdata = predsXY, type = "response")
df.pred.s3t2 <- data.frame(Pred=prediction.gam.s3t2)
prediction.gam.s3t2 <- data.frame(predsXY[,1:2], df.pred.s3t2$Pred)

Pred.gam.s3t2 <- rasterFromXYZ(prediction.gam.s3t2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s3t2, main=" Occurence Probability from 2015 to 2019 of Species 3 (s3_t2) by GAM")

## s3_t3
form_gam_s3t3 <- as.formula(paste0(names(modSpecies.s3t3)[1], "~", paste0("s(", names(modSpecies.s3t3)[2:ncol(modSpecies.s3t3)], ")", collapse = "+")))
gam.s3t3 <- gam(form_gam_s3t3, family = binomial, data = modSpecies.s3t3)
prediction.gam.s3t3<- predict(gam.s3t3, newdata = predsXY, type = "response")
df.pred.s3t3 <- data.frame(Pred=prediction.gam.s3t3)
prediction.gam.s3t3 <- data.frame(predsXY[,1:2], df.pred.s3t3$Pred)

Pred.gam.s3t3 <- rasterFromXYZ(prediction.gam.s3t3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s3t3, main=" Occurence Probability from 2020 to 2023 of Species 3 (s3_t3) by GAM")


###overlapping timescale###

## s3_T1
form_gam_s3T1 <- as.formula(paste0(names(modSpecies.s3T1)[1], "~", paste0("s(", names(modSpecies.s3T1)[2:ncol(modSpecies.s3T1)], ")", collapse = "+")))
gam.s3T1 <- gam(form_gam_s3T1, family = binomial, data = modSpecies.s3T1)
prediction.gam.s3T1<- predict(gam.s3T1, newdata = predsXY, type = "response")
df.pred.s3T1 <- data.frame(Pred=prediction.gam.s3T1)
prediction.gam.s3T1 <- data.frame(predsXY[,1:2], df.pred.s3T1$Pred)

Pred.gam.s3T1 <- rasterFromXYZ(prediction.gam.s3T1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s3T1, main=" Occurence Probability from 2000 to 2015 of Species 3 (s3_T1) by GAM")

## s3_T2
form_gam_s3T2 <- as.formula(paste0(names(modSpecies.s3T2)[1], "~", paste0("s(", names(modSpecies.s3T2)[2:ncol(modSpecies.s3T2)], ")", collapse = "+")))
gam.s3T2 <- gam(form_gam_s3T2, family = binomial, data = modSpecies.s3T2)
prediction.gam.s3T2<- predict(gam.s3T2, newdata = predsXY, type = "response")
df.pred.s3T2 <- data.frame(Pred=prediction.gam.s3T2)
prediction.gam.s3T2 <- data.frame(predsXY[,1:2], df.pred.s3T2$Pred)

Pred.gam.s3T2 <- rasterFromXYZ(prediction.gam.s3T2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s3T2, main=" Occurence Probability from 2000 to 2020 of Species 3 (s3_T2) by GAM")

## s3_T3
form_gam_s3T3 <- as.formula(paste0(names(modSpecies.s3T3)[1], "~", paste0("s(", names(modSpecies.s3T3)[2:ncol(modSpecies.s3T3)], ")", collapse = "+")))
gam.s3T3 <- gam(form_gam_s3T3, family = binomial, data = modSpecies.s3T3)
prediction.gam.s3T3<- predict(gam.s3T3, newdata = predsXY, type = "response")
df.pred.s3T3 <- data.frame(Pred=prediction.gam.s3T3)
prediction.gam.s3T3 <- data.frame(predsXY[,1:2], df.pred.s3T3$Pred)

Pred.gam.s3T3 <- rasterFromXYZ(prediction.gam.s3T3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.s3T3, main=" Occurence Probability from 2000 to 2023 of Species 3 (s3_T3) by GAM")





#####Random Forest####

###Non-overlapping (t)

#s3_t1
rf.s3t1<-ranger(modSpecies.s3t1$pres ~., data= modSpecies.s3t1, importance='impurity') 
Pred.rf.s3t1<- predict(
  rf.s3t1,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s3t1$num.trees)

prediction.rf.s3t1 <- data.frame(predsXY[,1:2], Pred.rf.s3t1$predictions)
prediction.rf.s3t1$Pred.rf.s3t1.predictions[ prediction.rf.s3t1$Pred.predictions == 1] <- 1 - 2.2e-16  #so that the probability is not 100% (too arrogant) but a smaller amount that is the most significant close to 1
Pred.rf.s3t1 <- rasterFromXYZ(prediction.rf.s3t1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s3t1, main= " Occurence Probability from 2000-2014 of Species 3 (s3_t1) by Random Forest")


#s3_t2
rf.s3t2<-ranger(modSpecies.s3t2$pres ~., data= modSpecies.s3t2, importance='impurity') 
Pred.rf.s3t2<- predict(
  rf.s3t2,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s3t2$num.trees)

prediction.rf.s3t2 <- data.frame(predsXY[,1:2], Pred.rf.s3t2$predictions)
prediction.rf.s3t2$Pred.rf.s3t2.predictions[ prediction.rf.s3t2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s3t2 <- rasterFromXYZ(prediction.rf.s3t2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s3t2, main= " Occurence Probability 2015-2019 of Species 3 (s3_t2) by Random Forest")

#s3_t3
rf.s3t3<-ranger(modSpecies.s3t3$pres ~., data= modSpecies.s3t3, importance='impurity') 
Pred.rf.s3t3<- predict(
  rf.s3t3,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s3t3$num.trees)

prediction.rf.s3t3 <- data.frame(predsXY[,1:2], Pred.rf.s3t3$predictions)
prediction.rf.s3t3$Pred.rf.s3t3.predictions[ prediction.rf.s3t3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s3t3 <- rasterFromXYZ(prediction.rf.s3t3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s3t3, main= " Occurence Probability from 2020-2023 of Species 3 (s3_t3) by Random Forest")



## overlapping timescale (T)
#s3_T1
rf.s3T1<-ranger(modSpecies.s3T1$pres ~., data= modSpecies.s3T1, importance='impurity') 
Pred.rf.s3T1<- predict(
  rf.s3T1,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s3T1$num.trees)

prediction.rf.s3T1 <- data.frame(predsXY[,1:2], Pred.rf.s3T1$predictions)
prediction.rf.s3T1$Pred.rf.s3T1.predictions[ prediction.rf.s3T1$Pred.predictions == 1] <- 1 - 2.2e-16  #so that the probability is not 100% (too arrogant) but a smaller amount that is the most significant close to 1
Pred.rf.s3T1 <- rasterFromXYZ(prediction.rf.s3T1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s3T1, main= " Occurence Probability from 2000-2015 of Species 3 (s3_T1) by Random Forest")


#s3_T2
rf.s3T2<-ranger(modSpecies.s3T2$pres ~., data= modSpecies.s3T2, importance='impurity') 
Pred.rf.s3T2<- predict(
  rf.s3T2,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s3T2$num.trees)

prediction.rf.s3T2 <- data.frame(predsXY[,1:2], Pred.rf.s3T2$predictions)
prediction.rf.s3T2$Pred.rf.s3T2.predictions[ prediction.rf.s3T2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s3T2 <- rasterFromXYZ(prediction.rf.s3T2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s3T2, main= " Occurence Probability from 2000-2020 of Species 3 (s3_T2) by Random Forest")

#s3_T3
rf.s3T3<-ranger(modSpecies.s3T3$pres ~., data= modSpecies.s3T3, importance='impurity') 
Pred.rf.s3T3<- predict(
  rf.s3T3,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.s3T3$num.trees)

prediction.rf.s3T3 <- data.frame(predsXY[,1:2], Pred.rf.s3T3$predictions)
prediction.rf.s3T3$Pred.rf.s3T3.predictions[ prediction.rf.s3T3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.s3T3 <- rasterFromXYZ(prediction.rf.s3T3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.s3T3, main= " Occurence Probability from 2000-2023 of Species 3 (s3_T3) by Random Forest")



#############EVALUATION PERFORMANCE##########
auc.gam.s3.t1<- AUCFunzGAM(modSpecies.s3t1) # because the modSpecies.s3t1 has too few no. of occ, so if we partioning the test by 0.2, there is not a chance it got the binary of the Pred => must increase the test partion to get both values
auc.gam.s3.t2<- AUCFunzGAM(modSpecies.s3t2)
auc.gam.s3.t3<- AUCFunzGAM(modSpecies.s3t3)

auc.gam.s3.T1<- AUCFunzGAM(modSpecies.s3T1)
auc.gam.s3.T2<- AUCFunzGAM(modSpecies.s3T2)
auc.gam.s3.T3<- AUCFunzGAM(modSpecies.s3T3)



auc.glm.s3.t1<- AUCFunzGLM(modSpecies.s3t1)
auc.glm.s3.t2<- AUCFunzGLM(modSpecies.s3t2)
auc.glm.s3.t3<- AUCFunzGLM(modSpecies.s3t3)

auc.glm.s3.T1<- AUCFunzGLM(modSpecies.s3T1)
auc.glm.s3.T2<- AUCFunzGLM(modSpecies.s3T2)
auc.glm.s3.T3<- AUCFunzGLM(modSpecies.s3T3)



auc.rf.s3.t1<- AUCFunzRF(modSpecies.s3t1)
auc.rf.s3.t2<- AUCFunzRF(modSpecies.s3t2)
auc.rf.s3.t3<- AUCFunzRF(modSpecies.s3t3)

auc.rf.s3.T1<- AUCFunzRF(modSpecies.s3T1)
auc.rf.s3.T2<- AUCFunzRF(modSpecies.s3T2)
auc.rf.s3.T3<- AUCFunzRF(modSpecies.s3T3)



tss.glm.s3.t1<- TSSFunzGLM(modSpecies.s3t1)
tss.glm.s3.t2<- TSSFunzGLM(modSpecies.s3t2)
tss.glm.s3.t3<- TSSFunzGLM(modSpecies.s3t3)

tss.glm.s3.T1<- TSSFunzGLM(modSpecies.s3T1)
tss.glm.s3.T2<- TSSFunzGLM(modSpecies.s3T2)
tss.glm.s3.T3<- TSSFunzGLM(modSpecies.s3T3)



tss.gam.s3.t1<- TSSFunzGAM(modSpecies.s3t1)
tss.gam.s3.t2<- TSSFunzGAM(modSpecies.s3t2)
tss.gam.s3.t3<- TSSFunzGAM(modSpecies.s3t3)

tss.gam.s3.T1<- TSSFunzGAM(modSpecies.s3T1)
tss.gam.s3.T2<- TSSFunzGAM(modSpecies.s3T2)
tss.gam.s3.T3<- TSSFunzGAM(modSpecies.s3T3)



tss.rf.s3.t1<- TSSFunzRF(modSpecies.s3t1)
tss.rf.s3.t2<- TSSFunzRF(modSpecies.s3t2)
tss.rf.s3.t3<- TSSFunzRF(modSpecies.s3t3)

tss.rf.s3.T1<- TSSFunzRF(modSpecies.s3T1)
tss.rf.s3.T2<- TSSFunzRF(modSpecies.s3T2)
tss.rf.s3.T3<- TSSFunzRF(modSpecies.s3T3)



boyce.glm.s3.t1<- BoyceGLM(modSpecies.s3t1)
boyce.glm.s3.t2<- BoyceGLM(modSpecies.s3t2)
boyce.glm.s3.t3<- BoyceGLM(modSpecies.s3t3)

boyce.glm.s3.T1<- BoyceGLM(modSpecies.s3T1)
boyce.glm.s3.T2<- BoyceGLM(modSpecies.s3T2)
boyce.glm.s3.T3<- BoyceGLM(modSpecies.s3T3)


boyce.gam.s3.t1<- BoyceGAM(modSpecies.s3t1)
boyce.gam.s3.t2<- BoyceGAM(modSpecies.s3t2)
boyce.gam.s3.t3<- BoyceGAM(modSpecies.s3t3)

boyce.gam.s3.T1<- BoyceGAM(modSpecies.s3T1)
boyce.gam.s3.T2<- BoyceGAM(modSpecies.s3T2)
boyce.gam.s3.T3<- BoyceGAM(modSpecies.s3T3)


boyce.rf.s3.t1<- BoyceRF(modSpecies.s3t1)
boyce.rf.s3.t2<- BoyceRF(modSpecies.s3t2)
boyce.rf.s3.t3<- BoyceRF(modSpecies.s3t3)

boyce.rf.s3.T1<- BoyceRF(modSpecies.s3T1)
boyce.rf.s3.T2<- BoyceRF(modSpecies.s3T2)
boyce.rf.s3.T3<- BoyceRF(modSpecies.s3T3)



##better GPT rcm
algorithm<- rep(c("GAM", "GLM", "RF"), each=6)
temporal_setting<-rep(c("t1", "t2", "t3", 'T1', "T2", "T3"), times = 3)
AUC <- c(auc.gam.s3.t1,auc.gam.s3.T1, auc.gam.s3.t2, auc.gam.s3.T2, auc.gam.s3.t3, auc.gam.s3.T3,
         auc.glm.s3.t1,auc.glm.s3.T1, auc.glm.s3.t2, auc.glm.s3.T2, auc.glm.s3.t3, auc.glm.s3.T3,
         auc.rf.s3.t1,auc.rf.s3.T1, auc.rf.s3.t2, auc.rf.s3.T2, auc.rf.s3.t3, auc.rf.s3.T3)
TSS<- c(tss.gam.s3.t1, tss.gam.s3.T1, tss.gam.s3.t2, tss.gam.s3.T2, tss.gam.s3.t3, tss.gam.s3.T3,
        tss.glm.s3.t1, tss.glm.s3.T1, tss.glm.s3.t2, tss.glm.s3.T2, tss.glm.s3.t3, tss.glm.s3.T3,
        tss.rf.s3.t1, tss.rf.s3.T1, tss.rf.s3.t2, tss.rf.s3.T2, tss.rf.s3.t3, tss.rf.s3.T3)
Boyce<- c(boyce.gam.s3.t1, boyce.gam.s3.T1, boyce.gam.s3.t2, boyce.gam.s3.T2, boyce.gam.s3.t3, boyce.gam.s3.T3,
          boyce.glm.s3.t1, boyce.glm.s3.T1, boyce.glm.s3.t2, boyce.glm.s3.T2, boyce.glm.s3.t3, boyce.glm.s3.T3,
          boyce.rf.s3.t1, boyce.rf.s3.T1, boyce.rf.s3.t2, boyce.rf.s3.T2, boyce.rf.s3.t3, boyce.rf.s3.T3)
perf_df <- data.frame(algorithm, temporal_setting, AUC, TSS, Boyce)

melted_perf <- perf_df %>% pivot_longer(cols = c(AUC, TSS, Boyce), names_to = "metric", values_to = "value")

ggplot(melted_perf, aes(x = algorithm, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  labs(title = "Performance of SDM Algorithms for Species 3", x = "Algorithms", y = "Performance") +
  scale_fill_manual(values = c("blue", "red", "green")) +
  theme_minimal() 
