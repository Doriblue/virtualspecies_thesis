
################################################################################################
#########################################Real Species1##########################################
################################################################################################
####RS1_t1/T1####
Rs1t1_GBIF <- read_csv("GBIF data/cleaned-data/s1_t1_T1.csv")
Rs1t1_loca<- data.frame(Rs1t1_GBIF$occurrenceStatus, Rs1t1_GBIF$decimalLongitude, Rs1t1_GBIF$decimalLatitude)
Rs1t1_loca$Rs1t1_GBIF.occurrenceStatus<- decostand(Rs1t1_loca$Rs1t1_GBIF.occurrenceStatus, method="pa")
colnames(Rs1t1_loca)<- c("Observed", "x", "y")
myRespXY.s1t1 <- Rs1t1_loca[, c('x', 'y')]

Rs1t1_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies1 t1/T1",
                                     expl.var = envi_stack,
                                     resp.var = Rs1t1_loca$Observed,
                                     resp.xy = myRespXY.s1t1,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs1t1_loca),
                                     PA.strategy = "random")
PseuAbs.Rs1t1<- data.frame(Rs1t1_formdat@data.species, Rs1t1_formdat@coord)
PseuAbs.Rs1t1[is.na(PseuAbs.Rs1t1)] <- 0
colnames(PseuAbs.Rs1t1)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs1t1)<-~x+y
crs(PseuAbs.Rs1t1)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs1t1<- raster::extract(envi, PseuAbs.Rs1t1, df=T)
values.Rs1t1<- values.Rs1t1[, -1] # first column not need it
values.Rs1t1$Observed<- c(PseuAbs.Rs1t1$Observed)
modSpecies.Rs1t1<-data.frame(pres= values.Rs1t1$Observed, values.Rs1t1[1:3])


####RS1_t2####
Rs1t2_GBIF <- read_csv("GBIF data/cleaned-data/s1-t2.csv")
Rs1t2_loca<- data.frame(Rs1t2_GBIF$occurrenceStatus, Rs1t2_GBIF$decimalLongitude, Rs1t2_GBIF$decimalLatitude)
Rs1t2_loca$Rs1t2_GBIF.occurrenceStatus<- decostand(Rs1t2_loca$Rs1t2_GBIF.occurrenceStatus, method="pa")
colnames(Rs1t2_loca)<- c("Observed", "x", "y")
myRespXY.s1t2 <- Rs1t2_loca[, c('x', 'y')]

Rs1t2_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies1 t2",
                                   expl.var = envi_stack,
                                   resp.var = Rs1t2_loca$Observed,
                                   resp.xy = myRespXY.s1t2,
                                   PA.nb.rep=1,
                                   PA.nb.absences=3*nrow(Rs1t2_loca),
                                   PA.strategy = "random")
PseuAbs.Rs1t2<- data.frame(Rs1t2_formdat@data.species, Rs1t2_formdat@coord)
PseuAbs.Rs1t2[is.na(PseuAbs.Rs1t2)] <- 0
colnames(PseuAbs.Rs1t2)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs1t2)<-~x+y
crs(PseuAbs.Rs1t2)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs1t2<- raster::extract(envi, PseuAbs.Rs1t2, df=T)
values.Rs1t2<- values.Rs1t2[, -1] # first column not need it
values.Rs1t2$Observed<- c(PseuAbs.Rs1t2$Observed)
modSpecies.Rs1t2<-data.frame(pres= values.Rs1t2$Observed, values.Rs1t2[1:3])



####RS1_t3####
Rs1t3_GBIF <- read_csv("GBIF data/cleaned-data/s1-t3.csv")
Rs1t3_loca<- data.frame(Rs1t3_GBIF$occurrenceStatus, Rs1t3_GBIF$decimalLongitude, Rs1t3_GBIF$decimalLatitude)
Rs1t3_loca$Rs1t3_GBIF.occurrenceStatus<- decostand(Rs1t3_loca$Rs1t3_GBIF.occurrenceStatus, method="pa")
colnames(Rs1t3_loca)<- c("Observed", "x", "y")
myRespXY.s1t3 <- Rs1t3_loca[, c('x', 'y')]

Rs1t3_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies1 t3",
                                     expl.var = envi_stack,
                                     resp.var = Rs1t3_loca$Observed,
                                     resp.xy = myRespXY.s1t3,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs1t3_loca),
                                     PA.strategy = "random")
PseuAbs.Rs1t3<- data.frame(Rs1t3_formdat@data.species, Rs1t3_formdat@coord)
PseuAbs.Rs1t3[is.na(PseuAbs.Rs1t3)] <- 0
colnames(PseuAbs.Rs1t3)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs1t3)<-~x+y
crs(PseuAbs.Rs1t3)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs1t3<- raster::extract(envi, PseuAbs.Rs1t3, df=T)
values.Rs1t3<- values.Rs1t3[, -1] # first column not need it
values.Rs1t3$Observed<- c(PseuAbs.Rs1t3$Observed)
modSpecies.Rs1t3<-data.frame(pres= values.Rs1t3$Observed, values.Rs1t3[1:3])

####RS1_T2####
Rs1T2_GBIF <- read_csv("GBIF data/cleaned-data/s1_T2.csv")
Rs1T2_loca<- data.frame(Rs1T2_GBIF$occurrenceStatus, Rs1T2_GBIF$decimalLongitude, Rs1T2_GBIF$decimalLatitude)
Rs1T2_loca$Rs1T2_GBIF.occurrenceStatus<- decostand(Rs1T2_loca$Rs1T2_GBIF.occurrenceStatus, method="pa")
colnames(Rs1T2_loca)<- c("Observed", "x", "y")
myRespXY.s1T2 <- Rs1T2_loca[, c('x', 'y')]

Rs1T2_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies1 T2",
                                     expl.var = envi_stack,
                                     resp.var = Rs1T2_loca$Observed,
                                     resp.xy = myRespXY.s1T2,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs1T2_loca),
                                     PA.strategy = "random")
PseuAbs.Rs1T2<- data.frame(Rs1T2_formdat@data.species, Rs1T2_formdat@coord)
PseuAbs.Rs1T2[is.na(PseuAbs.Rs1T2)] <- 0
colnames(PseuAbs.Rs1T2)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs1T2)<-~x+y
crs(PseuAbs.Rs1T2)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs1T2<- raster::extract(envi, PseuAbs.Rs1T2, df=T)
values.Rs1T2<- values.Rs1T2[, -1] # first column not need it
values.Rs1T2$Observed<- c(PseuAbs.Rs1T2$Observed)
modSpecies.Rs1T2<-data.frame(pres= values.Rs1T2$Observed, values.Rs1T2[1:3])


####RS1_T3####
Rs1T3_GBIF <- read_csv("GBIF data/cleaned-data/s1_T3.csv")
Rs1T3_loca<- data.frame(Rs1T3_GBIF$occurrenceStatus, Rs1T3_GBIF$decimalLongitude, Rs1T3_GBIF$decimalLatitude)
Rs1T3_loca$Rs1T3_GBIF.occurrenceStatus<- decostand(Rs1T3_loca$Rs1T3_GBIF.occurrenceStatus, method="pa")
colnames(Rs1T3_loca)<- c("Observed", "x", "y")
myRespXY.s1T3 <- Rs1T3_loca[, c('x', 'y')]

###
Rs1T3_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies1 T3",
                                     expl.var = envi_stack,
                                     resp.var = Rs1T3_loca$Observed,
                                     resp.xy = myRespXY.s1T3,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs1T3_loca),
                                     PA.strategy = "random")
PseuAbs.Rs1T3<- data.frame(Rs1T3_formdat@data.species, Rs1T3_formdat@coord)
PseuAbs.Rs1T3[is.na(PseuAbs.Rs1T3)] <- 0
colnames(PseuAbs.Rs1T3)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs1T3)<-~x+y
crs(PseuAbs.Rs1T3)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs1T3<- raster::extract(envi, PseuAbs.Rs1T3, df=T)
values.Rs1T3<- values.Rs1T3[, -1] # first column not need it
values.Rs1T3$Observed<- c(PseuAbs.Rs1T3$Observed)
modSpecies.Rs1T3<-data.frame(pres= values.Rs1T3$Observed, values.Rs1T3[1:3])

####################SDM#####################
#####GLM#####
glm.Rs1t1<-multGLM(modSpecies.Rs1t1, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs1t1), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs1t1<- getPreds(envi, models=glm.Rs1t1$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs1t1<- data.frame(glm.Rs1t1$predictions)
crs(Pred.glm.Rs1t1) <- crs(envi)
plot(Pred.glm.Rs1t1, main= "Occurence Probability from 2000 to 2010 of Real-Species 1 (s1_t1) by GLM")

glm.Rs1t2<-multGLM(modSpecies.Rs1t2, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs1t2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs1t2<- getPreds(envi, models=glm.Rs1t2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs1t2<- data.frame(glm.Rs1t2$predictions)
crs(Pred.glm.Rs1t2) <- crs(envi)
plot(Pred.glm.Rs1t2, main= "Occurence Probability from 2011 to 2015 of Real-Species 1 (s1_t2) by GLM")

glm.Rs1t3<-multGLM(modSpecies.Rs1t3, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs1t3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs1t3<- getPreds(envi, models=glm.Rs1t3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs1t3<- data.frame(glm.Rs1t3$predictions)
crs(Pred.glm.Rs1t3) <- crs(envi)
plot(Pred.glm.Rs1t3, main= "Occurence Probability from 2016 to 2020 of Real-Species 1 (s1_t3) by GLM")

glm.Rs1T2<-multGLM(modSpecies.Rs1T2, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs1T2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs1T2<- getPreds(envi, models=glm.Rs1T2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs1T2<- data.frame(glm.Rs1T2$predictions)
crs(Pred.glm.Rs1T2) <- crs(envi)
plot(Pred.glm.Rs1T2, main= "Occurence Probability from 2000 to 2015 of Real-Species 1 (s1_T2) by GLM")

glm.Rs1T3<-multGLM(modSpecies.Rs1T3, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs1T3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs1T3<- getPreds(envi, models=glm.Rs1T3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs1T3<- data.frame(glm.Rs1T3$predictions)
crs(Pred.glm.Rs1T3) <- crs(envi)
plot(Pred.glm.Rs1T3, main= "Occurence Probability from 2000 to 2020 of Real-Species 1 (Rs1_T3) by GLM")

#####GAM#####
## Rs1_t1
gam.Rs1t1 <- GAM_sp(modSpecies.Rs1t1)
prediction.gam.Rs1t1<- predict(gam.Rs1t1,newdata = predsXY, type = "response")
df.pred.Rs1t1 <- data.frame(Pred=prediction.gam.Rs1t1)
prediction.gam.Rs1t1 <- data.frame(predsXY[,1:2], df.pred.Rs1t1$Pred)
Pred.gam.Rs1t1 <- rasterFromXYZ(prediction.gam.Rs1t1,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs1t1, main=" Occurence Probability from 2000 to 2010 of Real-Species 1 (Rs1_t1) by GAM")

## Rs1_t2
gam.Rs1t2 <- GAM_sp(modSpecies.Rs1t2)
prediction.gam.Rs1t2<- predict(gam.Rs1t2,newdata = predsXY, type = "response")
df.pred.Rs1t2 <- data.frame(Pred=prediction.gam.Rs1t2)
prediction.gam.Rs1t2 <- data.frame(predsXY[,1:2], df.pred.Rs1t2$Pred)
Pred.gam.Rs1t2 <- rasterFromXYZ(prediction.gam.Rs1t2,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs1t2, main=" Occurence Probability from 2011 to 2016 of Real-Species 1 (Rs1_t2) by GAM")

## Rs1_t3
gam.Rs1t3 <- GAM_sp(modSpecies.Rs1t3)
prediction.gam.Rs1t3<- predict(gam.Rs1t3,newdata = predsXY, type = "response")
df.pred.Rs1t3 <- data.frame(Pred=prediction.gam.Rs1t3)
prediction.gam.Rs1t3 <- data.frame(predsXY[,1:2], df.pred.Rs1t3$Pred)
Pred.gam.Rs1t3 <- rasterFromXYZ(prediction.gam.Rs1t3,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs1t3, main=" Occurence Probability from 2016 to 2020 of Real-Species 1 (Rs1_t3) by GAM")

## Rs1_T2
gam.Rs1T2 <- GAM_sp(modSpecies.Rs1T2)
prediction.gam.Rs1T2<- predict(gam.Rs1T2,newdata = predsXY, type = "response")
df.pred.Rs1T2 <- data.frame(Pred=prediction.gam.Rs1T2)
prediction.gam.Rs1T2 <- data.frame(predsXY[,1:2], df.pred.Rs1T2$Pred)
Pred.gam.Rs1T2 <- rasterFromXYZ(prediction.gam.Rs1T2,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs1T2, main=" Occurence Probability from 2011 to 2016 of Real-Species 1 (Rs1_T2) by GAM")

## Rs1_T3
gam.Rs1T3 <- GAM_sp(modSpecies.Rs1T3)
prediction.gam.Rs1T3<- predict(gam.Rs1T3,newdata = predsXY, type = "response")
df.pred.Rs1T3 <- data.frame(Pred=prediction.gam.Rs1T3)
prediction.gam.Rs1T3 <- data.frame(predsXY[,1:2], df.pred.Rs1T3$Pred)
Pred.gam.Rs1T3 <- rasterFromXYZ(prediction.gam.Rs1T3,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs1T3, main=" Occurence Probability from 2016 to 2020 of Real-Species 1 (Rs1_T3) by GAM")


#####RF#####
#Rs1_t1
rf.Rs1t1<- ranger_sp(modSpecies.Rs1t1)
Pred.rf.Rs1t1<- predict(
  rf.Rs1t1,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs1t1$num.trees)

prediction.rf.Rs1t1 <- data.frame(predsXY[,1:2], Pred.rf.Rs1t1$predictions)
prediction.rf.Rs1t1$Pred.rf.Rs1t1.predictions[ prediction.rf.Rs1t1$Pred.predictions == 1] <- 1 - 2.2e-16  #so that the probability is not 100% (too arrogant) but a smaller amount that is the most significant close to 1
Pred.rf.Rs1t1 <- rasterFromXYZ(prediction.rf.Rs1t1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs1t1, main= " Occurence Probability from 2000-2009 of Species 1 (Rs1_t1) by Random Forest")


#Rs1_t2
rf.Rs1t2<-ranger_sp(modSpecies.Rs1t2) 
Pred.rf.Rs1t2<- predict(
  rf.Rs1t2,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs1t2$num.trees)

prediction.rf.Rs1t2 <- data.frame(predsXY[,1:2], Pred.rf.Rs1t2$predictions)
prediction.rf.Rs1t2$Pred.rf.Rs1t2.predictions[ prediction.rf.Rs1t2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs1t2 <- rasterFromXYZ(prediction.rf.Rs1t2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs1t2, main= " Occurence Probability from 2010-2014 of Species 1 (Rs1_t2) by Random Forest")

#Rs1_t3
rf.Rs1t3<-ranger_sp(modSpecies.Rs1t3) 
Pred.rf.Rs1t3<- predict(
  rf.Rs1t3,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs1t3$num.trees)

prediction.rf.Rs1t3 <- data.frame(predsXY[,1:2], Pred.rf.Rs1t3$predictions)
prediction.rf.Rs1t3$Pred.rf.Rs1t3.predictions[ prediction.rf.Rs1t3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs1t3 <- rasterFromXYZ(prediction.rf.Rs1t3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs1t3, main= " Occurence Probability from 2015-2023 of Species 1 (Rs1_t3) by Random Forest")

## overlapping timescale (T)
#Rs1_T2
rf.Rs1T2<-ranger_sp(modSpecies.Rs1T2)
Pred.rf.Rs1T2<- predict(
  rf.Rs1T2,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs1T2$num.trees)

prediction.rf.Rs1T2 <- data.frame(predsXY[,1:2], Pred.rf.Rs1T2$predictions)
prediction.rf.Rs1T2$Pred.rf.Rs1T2.predictions[ prediction.rf.Rs1T2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs1T2 <- rasterFromXYZ(prediction.rf.Rs1T2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs1T2, main= " Occurence Probability from 2000-2015 of Species 1 (Rs1_T2) by Random Forest")

#Rs1_T3
rf.Rs1T3<-ranger_sp(modSpecies.Rs1T3)
Pred.rf.Rs1T3<- predict(
  rf.Rs1T3,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs1T3$num.trees)

prediction.rf.Rs1T3 <- data.frame(predsXY[,1:2], Pred.rf.Rs1T3$predictions)
prediction.rf.Rs1T3$Pred.rf.Rs1T3.predictions[ prediction.rf.Rs1T3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs1T3 <- rasterFromXYZ(prediction.rf.Rs1T3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs1T3, main= " Occurence Probability from 2000-2023 of Species 1 (Rs1_T3) by Random Forest")

############EVALUATION PERFORMANCE############

#####Create table of results
auc.gam.Rs1.t1<- AUCFunzGAM(modSpecies.Rs1t1)
auc.gam.Rs1.t2<- AUCFunzGAM(modSpecies.Rs1t2)
auc.gam.Rs1.t3<- AUCFunzGAM(modSpecies.Rs1t3)

auc.gam.Rs1.T1<- AUCFunzGAM(modSpecies.Rs1t1)
auc.gam.Rs1.T2<- AUCFunzGAM(modSpecies.Rs1T2)
auc.gam.Rs1.T3<- AUCFunzGAM(modSpecies.Rs1T3)



auc.glm.Rs1.t1<- AUCFunzGLM(modSpecies.Rs1t1)
auc.glm.Rs1.t2<- AUCFunzGLM(modSpecies.Rs1t2)
auc.glm.Rs1.t3<- AUCFunzGLM(modSpecies.Rs1t3)

auc.glm.Rs1.T1<- AUCFunzGLM(modSpecies.Rs1t1)
auc.glm.Rs1.T2<- AUCFunzGLM(modSpecies.Rs1T2)
auc.glm.Rs1.T3<- AUCFunzGLM(modSpecies.Rs1T3)



auc.rf.Rs1.t1<- AUCFunzRF(modSpecies.Rs1t1)
auc.rf.Rs1.t2<- AUCFunzRF(modSpecies.Rs1t2)
auc.rf.Rs1.t3<- AUCFunzRF(modSpecies.Rs1t3)

auc.rf.Rs1.T1<- AUCFunzRF(modSpecies.Rs1t1)
auc.rf.Rs1.T2<- AUCFunzRF(modSpecies.Rs1T2)
auc.rf.Rs1.T3<- AUCFunzRF(modSpecies.Rs1T3)



tss.glm.Rs1.t1<- TSSFunzGLM(modSpecies.Rs1t1)
tss.glm.Rs1.t2<- TSSFunzGLM(modSpecies.Rs1t2)
tss.glm.Rs1.t3<- TSSFunzGLM(modSpecies.Rs1t3)

tss.glm.Rs1.T1<- TSSFunzGLM(modSpecies.Rs1t1)
tss.glm.Rs1.T2<- TSSFunzGLM(modSpecies.Rs1T2)
tss.glm.Rs1.T3<- TSSFunzGLM(modSpecies.Rs1T3)



tss.gam.Rs1.t1<- TSSFunzGAM(modSpecies.Rs1t1)
tss.gam.Rs1.t2<- TSSFunzGAM(modSpecies.Rs1t2)
tss.gam.Rs1.t3<- TSSFunzGAM(modSpecies.Rs1t3)

tss.gam.Rs1.T1<- TSSFunzGAM(modSpecies.Rs1t1)
tss.gam.Rs1.T2<- TSSFunzGAM(modSpecies.Rs1T2)
tss.gam.Rs1.T3<- TSSFunzGAM(modSpecies.Rs1T3)



tss.rf.Rs1.t1<- TSSFunzRF(modSpecies.Rs1t1)
tss.rf.Rs1.t2<- TSSFunzRF(modSpecies.Rs1t2)
tss.rf.Rs1.t3<- TSSFunzRF(modSpecies.Rs1t3)

tss.rf.Rs1.T1<- TSSFunzRF(modSpecies.Rs1t1)
tss.rf.Rs1.T2<- TSSFunzRF(modSpecies.Rs1T2)
tss.rf.Rs1.T3<- TSSFunzRF(modSpecies.Rs1T3)



boyce.glm.Rs1.t1<- BoyceGLM(modSpecies.Rs1t1)
boyce.glm.Rs1.t2<- BoyceGLM(modSpecies.Rs1t2)
boyce.glm.Rs1.t3<- BoyceGLM(modSpecies.Rs1t3)

boyce.glm.Rs1.T1<- BoyceGLM(modSpecies.Rs1t1)
boyce.glm.Rs1.T2<- BoyceGLM(modSpecies.Rs1T2)
boyce.glm.Rs1.T3<- BoyceGLM(modSpecies.Rs1T3)


boyce.gam.Rs1.t1<- BoyceGAM(modSpecies.Rs1t1)
boyce.gam.Rs1.t2<- BoyceGAM(modSpecies.Rs1t2)
boyce.gam.Rs1.t3<- BoyceGAM(modSpecies.Rs1t3)

boyce.gam.Rs1.T1<- BoyceGAM(modSpecies.Rs1t1)
boyce.gam.Rs1.T2<- BoyceGAM(modSpecies.Rs1T2)
boyce.gam.Rs1.T3<- BoyceGAM(modSpecies.Rs1T3)


boyce.rf.Rs1.t1<- BoyceRF(modSpecies.Rs1t1)
boyce.rf.Rs1.t2<- BoyceRF(modSpecies.Rs1t2)
boyce.rf.Rs1.t3<- BoyceRF(modSpecies.Rs1t3)

boyce.rf.Rs1.T1<- BoyceRF(modSpecies.Rs1t1)
boyce.rf.Rs1.T2<- BoyceRF(modSpecies.Rs1T2)
boyce.rf.Rs1.T3<- BoyceRF(modSpecies.Rs1T3)



#########Evaluation####
algorithm<- rep(c("GAM", "GLM", "RF"), each=6)
temporal_setting<-rep(c("t1", "t2", "t3", 'T1', "T2", "T3"), times = 3)
AUC <- c(auc.gam.Rs1.t1,auc.gam.Rs1.T1, auc.gam.Rs1.t2, auc.gam.Rs1.T2, auc.gam.Rs1.t3, auc.gam.Rs1.T3,
         auc.glm.Rs1.t1,auc.glm.Rs1.T1, auc.glm.Rs1.t2, auc.glm.Rs1.T2, auc.glm.Rs1.t3, auc.glm.Rs1.T3,
         auc.rf.Rs1.t1,auc.rf.Rs1.T1, auc.rf.Rs1.t2, auc.rf.Rs1.T2, auc.rf.Rs1.t3, auc.rf.Rs1.T3)
TSS<- c(tss.gam.Rs1.t1, tss.gam.Rs1.T1, tss.gam.Rs1.t2, tss.gam.Rs1.T2, tss.gam.Rs1.t3, tss.gam.Rs1.T3,
        tss.glm.Rs1.t1, tss.glm.Rs1.T1, tss.glm.Rs1.t2, tss.glm.Rs1.T2, tss.glm.Rs1.t3, tss.glm.Rs1.T3,
        tss.rf.Rs1.t1, tss.rf.Rs1.T1, tss.rf.Rs1.t2, tss.rf.Rs1.T2, tss.rf.Rs1.t3, tss.rf.Rs1.T3)
Boyce<- c(boyce.gam.Rs1.t1, boyce.gam.Rs1.T1, boyce.gam.Rs1.t2, boyce.gam.Rs1.T2, boyce.gam.Rs1.t3, boyce.gam.Rs1.T3,
          boyce.glm.Rs1.t1, boyce.glm.Rs1.T1, boyce.glm.Rs1.t2, boyce.glm.Rs1.T2, boyce.glm.Rs1.t3, boyce.glm.Rs1.T3,
          boyce.rf.Rs1.t1, boyce.rf.Rs1.T1, boyce.rf.Rs1.t2, boyce.rf.Rs1.T2, boyce.rf.Rs1.t3, boyce.rf.Rs1.T3)
perf_df <- data.frame(algorithm, temporal_setting, AUC, TSS, Boyce)

melted_perf <- perf_df %>% pivot_longer(cols = c(AUC, TSS, Boyce), names_to = "metric", values_to = "value")

ggplot(melted_perf, aes(x = algorithm, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  labs(title = "Performance of SDM Algorithms for Real Species 1-set 2", x = "Algorithms", y = "Performance") +
  scale_fill_manual(values = c("blue", "red", "green")) +
  theme_minimal() 
################################################################################################
#########################################Real Species 2#########################################
################################################################################################
####Rs2_t1/T1####
Rs2t1_GBIF <- read_csv("GBIF data/cleaned-data/s2_t1_T1.csv")
Rs2t1_loca<- data.frame(Rs2t1_GBIF$occurrenceStatus, Rs2t1_GBIF$decimalLongitude, Rs2t1_GBIF$decimalLatitude)
Rs2t1_loca$Rs2t1_GBIF.occurrenceStatus<- decostand(Rs2t1_loca$Rs2t1_GBIF.occurrenceStatus, method="pa")
colnames(Rs2t1_loca)<- c("Observed", "x", "y")
myRespXY.s2t1 <- Rs2t1_loca[, c('x', 'y')]

Rs2t1_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies2 t1/T1",
                                     expl.var = envi_stack,
                                     resp.var = Rs2t1_loca$Observed,
                                     resp.xy = myRespXY.s2t1,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs2t1_loca),
                                     PA.strategy = "random")
PseuAbs.Rs2t1<- data.frame(Rs2t1_formdat@data.species, Rs2t1_formdat@coord)
PseuAbs.Rs2t1[is.na(PseuAbs.Rs2t1)] <- 0
colnames(PseuAbs.Rs2t1)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs2t1)<-~x+y
crs(PseuAbs.Rs2t1)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs2t1<- raster::extract(envi, PseuAbs.Rs2t1, df=T)
values.Rs2t1<- values.Rs2t1[, -1] # first column not need it
values.Rs2t1$Observed<- c(PseuAbs.Rs2t1$Observed)
modSpecies.Rs2t1<-data.frame(pres= values.Rs2t1$Observed, values.Rs2t1[1:3])


####Rs2_t2####
Rs2t2_GBIF <- read_csv("GBIF data/cleaned-data/s2-t2.csv", col_names = T)
Rs2t2_loca<- data.frame(Rs2t2_GBIF$occurrenceStatus, Rs2t2_GBIF$decimalLongitude, Rs2t2_GBIF$decimalLatitude)
Rs2t2_loca$Rs2t2_GBIF.occurrenceStatus<- decostand(Rs2t2_loca$Rs2t2_GBIF.occurrenceStatus, method="pa")
colnames(Rs2t2_loca)<- c("Observed", "x", "y")
myRespXY.s2t2 <- Rs2t2_loca[, c('x', 'y')]

Rs2t2_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies2 t2",
                                     expl.var = envi_stack,
                                     resp.var = Rs2t2_loca$Observed,
                                     resp.xy = myRespXY.s2t2,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs2t2_loca),
                                     PA.strategy = "random")
PseuAbs.Rs2t2<- data.frame(Rs2t2_formdat@data.species, Rs2t2_formdat@coord)
PseuAbs.Rs2t2[is.na(PseuAbs.Rs2t2)] <- 0
colnames(PseuAbs.Rs2t2)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs2t2)<-~x+y
crs(PseuAbs.Rs2t2)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs2t2<- raster::extract(envi, PseuAbs.Rs2t2, df=T)
values.Rs2t2<- values.Rs2t2[, -1] # first column not need it
values.Rs2t2$Observed<- c(PseuAbs.Rs2t2$Observed)
modSpecies.Rs2t2<-data.frame(pres= values.Rs2t2$Observed, values.Rs2t2[1:3])



####Rs2_t3####
Rs2t3_GBIF <- read_csv("GBIF data/cleaned-data/s2-t3.csv")
Rs2t3_loca<- data.frame(Rs2t3_GBIF$occurrenceStatus, Rs2t3_GBIF$decimalLongitude, Rs2t3_GBIF$decimalLatitude)
Rs2t3_loca$Rs2t3_GBIF.occurrenceStatus<- decostand(Rs2t3_loca$Rs2t3_GBIF.occurrenceStatus, method="pa")
colnames(Rs2t3_loca)<- c("Observed", "x", "y")
myRespXY.s2t3 <- Rs2t3_loca[, c('x', 'y')]

Rs2t3_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies2 t3",
                                     expl.var = envi_stack,
                                     resp.var = Rs2t3_loca$Observed,
                                     resp.xy = myRespXY.s2t3,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs2t3_loca),
                                     PA.strategy = "random")
PseuAbs.Rs2t3<- data.frame(Rs2t3_formdat@data.species, Rs2t3_formdat@coord)
PseuAbs.Rs2t3[is.na(PseuAbs.Rs2t3)] <- 0
colnames(PseuAbs.Rs2t3)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs2t3)<-~x+y
crs(PseuAbs.Rs2t3)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs2t3<- raster::extract(envi, PseuAbs.Rs2t3, df=T)
values.Rs2t3<- values.Rs2t3[, -1] # first column not need it
values.Rs2t3$Observed<- c(PseuAbs.Rs2t3$Observed)
modSpecies.Rs2t3<-data.frame(pres= values.Rs2t3$Observed, values.Rs2t3[1:3])

####Rs2_T2####
Rs2T2_GBIF <- read_csv("GBIF data/cleaned-data/s2_T2.csv")
Rs2T2_loca<- data.frame(Rs2T2_GBIF$occurrenceStatus, Rs2T2_GBIF$decimalLongitude, Rs2T2_GBIF$decimalLatitude)
Rs2T2_loca$Rs2T2_GBIF.occurrenceStatus<- decostand(Rs2T2_loca$Rs2T2_GBIF.occurrenceStatus, method="pa")
colnames(Rs2T2_loca)<- c("Observed", "x", "y")
myRespXY.s2T2 <- Rs2T2_loca[, c('x', 'y')]

Rs2T2_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies2 T2",
                                     expl.var = envi_stack,
                                     resp.var = Rs2T2_loca$Observed,
                                     resp.xy = myRespXY.s2T2,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs2T2_loca),
                                     PA.strategy = "random")
PseuAbs.Rs2T2<- data.frame(Rs2T2_formdat@data.species, Rs2T2_formdat@coord)
PseuAbs.Rs2T2[is.na(PseuAbs.Rs2T2)] <- 0
colnames(PseuAbs.Rs2T2)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs2T2)<-~x+y
crs(PseuAbs.Rs2T2)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs2T2<- raster::extract(envi, PseuAbs.Rs2T2, df=T)
values.Rs2T2<- values.Rs2T2[, -1] # first column not need it
values.Rs2T2$Observed<- c(PseuAbs.Rs2T2$Observed)
modSpecies.Rs2T2<-data.frame(pres= values.Rs2T2$Observed, values.Rs2T2[1:3])


####Rs2_T3####
Rs2T3_GBIF <- read_csv("GBIF data/cleaned-data/s2_T3.csv")
Rs2T3_loca<- data.frame(Rs2T3_GBIF$occurrenceStatus, Rs2T3_GBIF$decimalLongitude, Rs2T3_GBIF$decimalLatitude)
Rs2T3_loca$Rs2T3_GBIF.occurrenceStatus<- decostand(Rs2T3_loca$Rs2T3_GBIF.occurrenceStatus, method="pa")
colnames(Rs2T3_loca)<- c("Observed", "x", "y")
myRespXY.s2T3 <- Rs2T3_loca[, c('x', 'y')]

###
Rs2T3_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies2 T3",
                                     expl.var = envi_stack,
                                     resp.var = Rs2T3_loca$Observed,
                                     resp.xy = myRespXY.s2T3,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs2T3_loca),
                                     PA.strategy = "random")
PseuAbs.Rs2T3<- data.frame(Rs2T3_formdat@data.species, Rs2T3_formdat@coord)
PseuAbs.Rs2T3[is.na(PseuAbs.Rs2T3)] <- 0
colnames(PseuAbs.Rs2T3)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs2T3)<-~x+y
crs(PseuAbs.Rs2T3)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs2T3<- raster::extract(envi, PseuAbs.Rs2T3, df=T)
values.Rs2T3<- values.Rs2T3[, -1] # first column not need it
values.Rs2T3$Observed<- c(PseuAbs.Rs2T3$Observed)
modSpecies.Rs2T3<-data.frame(pres= values.Rs2T3$Observed, values.Rs2T3[1:3])
####################SDM#####################
#####GLM#####
glm.Rs2t1<-multGLM(modSpecies.Rs2t1, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs2t1), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs2t1<- getPreds(envi, models=glm.Rs2t1$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs2t1<- data.frame(glm.Rs2t1$predictions)
crs(Pred.glm.Rs2t1) <- crs(envi)
plot(Pred.glm.Rs2t1, main= "Occurence Probability from 2000 to 2010 of Real-Species 1 (s1_t1) by GLM")

glm.Rs2t2<-multGLM(modSpecies.Rs2t2, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs2t2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs2t2<- getPreds(envi, models=glm.Rs2t2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs2t2<- data.frame(glm.Rs2t2$predictions)
crs(Pred.glm.Rs2t2) <- crs(envi)
plot(Pred.glm.Rs2t2, main= "Occurence Probability from 2011 to 2015 of Real-Species 1 (s1_t2) by GLM")

glm.Rs2t3<-multGLM(modSpecies.Rs2t3, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs2t3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs2t3<- getPreds(envi, models=glm.Rs2t3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs2t3<- data.frame(glm.Rs2t3$predictions)
crs(Pred.glm.Rs2t3) <- crs(envi)
plot(Pred.glm.Rs2t3, main= "Occurence Probability from 2016 to 2020 of Real-Species 1 (s1_t3) by GLM")

glm.Rs2T2<-multGLM(modSpecies.Rs2T2, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs2T2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs2T2<- getPreds(envi, models=glm.Rs2T2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs2T2<- data.frame(glm.Rs2T2$predictions)
crs(Pred.glm.Rs2T2) <- crs(envi)
plot(Pred.glm.Rs2T2, main= "Occurence Probability from 2000 to 2015 of Real-Species 1 (s1_T2) by GLM")

glm.Rs2T3<-multGLM(modSpecies.Rs2T3, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs2T3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs2T3<- getPreds(envi, models=glm.Rs2T3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs2T3<- data.frame(glm.Rs2T3$predictions)
crs(Pred.glm.Rs2T3) <- crs(envi)
plot(Pred.glm.Rs2T3, main= "Occurence Probability from 2000 to 2020 of Real-Species 1 (Rs2_T3) by GLM")

#####GAM#####
## Rs2_t1
gam.Rs2t1 <- GAM_sp(modSpecies.Rs2t1)
prediction.gam.Rs2t1<- predict(gam.Rs2t1,newdata = predsXY, type = "response")
df.pred.Rs2t1 <- data.frame(Pred=prediction.gam.Rs2t1)
prediction.gam.Rs2t1 <- data.frame(predsXY[,1:2], df.pred.Rs2t1$Pred)
Pred.gam.Rs2t1 <- rasterFromXYZ(prediction.gam.Rs2t1,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs2t1, main=" Occurence Probability from 2000 to 2010 of Real-Species 1 (Rs2_t1) by GAM")

## Rs2_t2
gam.Rs2t2 <- GAM_sp(modSpecies.Rs2t2)
prediction.gam.Rs2t2<- predict(gam.Rs2t2,newdata = predsXY, type = "response")
df.pred.Rs2t2 <- data.frame(Pred=prediction.gam.Rs2t2)
prediction.gam.Rs2t2 <- data.frame(predsXY[,1:2], df.pred.Rs2t2$Pred)
Pred.gam.Rs2t2 <- rasterFromXYZ(prediction.gam.Rs2t2,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs2t2, main=" Occurence Probability from 2011 to 2016 of Real-Species 1 (Rs2_t2) by GAM")

## Rs2_t3
gam.Rs2t3 <- GAM_sp(modSpecies.Rs2t3)
prediction.gam.Rs2t3<- predict(gam.Rs2t3,newdata = predsXY, type = "response")
df.pred.Rs2t3 <- data.frame(Pred=prediction.gam.Rs2t3)
prediction.gam.Rs2t3 <- data.frame(predsXY[,1:2], df.pred.Rs2t3$Pred)
Pred.gam.Rs2t3 <- rasterFromXYZ(prediction.gam.Rs2t3,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs2t3, main=" Occurence Probability from 2016 to 2020 of Real-Species 1 (Rs2_t3) by GAM")

## Rs2_T2
gam.Rs2T2 <- GAM_sp(modSpecies.Rs2T2)
prediction.gam.Rs2T2<- predict(gam.Rs2T2,newdata = predsXY, type = "response")
df.pred.Rs2T2 <- data.frame(Pred=prediction.gam.Rs2T2)
prediction.gam.Rs2T2 <- data.frame(predsXY[,1:2], df.pred.Rs2T2$Pred)
Pred.gam.Rs2T2 <- rasterFromXYZ(prediction.gam.Rs2T2,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs2T2, main=" Occurence Probability from 2011 to 2016 of Real-Species 1 (Rs2_T2) by GAM")

## Rs2_T3
gam.Rs2T3 <- GAM_sp(modSpecies.Rs2T3)
prediction.gam.Rs2T3<- predict(gam.Rs2T3,newdata = predsXY, type = "response")
df.pred.Rs2T3 <- data.frame(Pred=prediction.gam.Rs2T3)
prediction.gam.Rs2T3 <- data.frame(predsXY[,1:2], df.pred.Rs2T3$Pred)
Pred.gam.Rs2T3 <- rasterFromXYZ(prediction.gam.Rs2T3,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs2T3, main=" Occurence Probability from 2016 to 2020 of Real-Species 1 (Rs2_T3) by GAM")


#####RF#####
#Rs2_t1
rf.Rs2t1<- ranger_sp(modSpecies.Rs2t1)
Pred.rf.Rs2t1<- predict(
  rf.Rs2t1,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs2t1$num.trees)

prediction.rf.Rs2t1 <- data.frame(predsXY[,1:2], Pred.rf.Rs2t1$predictions)
prediction.rf.Rs2t1$Pred.rf.Rs2t1.predictions[ prediction.rf.Rs2t1$Pred.predictions == 1] <- 1 - 2.2e-16  #so that the probability is not 100% (too arrogant) but a smaller amount that is the most significant close to 1
Pred.rf.Rs2t1 <- rasterFromXYZ(prediction.rf.Rs2t1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs2t1, main= " Occurence Probability from 2000-2009 of Species 1 (Rs2_t1) by Random Forest")


#Rs2_t2
rf.Rs2t2<-ranger_sp(modSpecies.Rs2t2) 
Pred.rf.Rs2t2<- predict(
  rf.Rs2t2,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs2t2$num.trees)

prediction.rf.Rs2t2 <- data.frame(predsXY[,1:2], Pred.rf.Rs2t2$predictions)
prediction.rf.Rs2t2$Pred.rf.Rs2t2.predictions[ prediction.rf.Rs2t2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs2t2 <- rasterFromXYZ(prediction.rf.Rs2t2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs2t2, main= " Occurence Probability from 2010-2014 of Species 1 (Rs2_t2) by Random Forest")

#Rs2_t3
rf.Rs2t3<-ranger_sp(modSpecies.Rs2t3) 
Pred.rf.Rs2t3<- predict(
  rf.Rs2t3,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs2t3$num.trees)

prediction.rf.Rs2t3 <- data.frame(predsXY[,1:2], Pred.rf.Rs2t3$predictions)
prediction.rf.Rs2t3$Pred.rf.Rs2t3.predictions[ prediction.rf.Rs2t3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs2t3 <- rasterFromXYZ(prediction.rf.Rs2t3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs2t3, main= " Occurence Probability from 2015-2023 of Species 1 (Rs2_t3) by Random Forest")



## overlapping timescale (T)
#Rs2_T2
rf.Rs2T2<-ranger_sp(modSpecies.Rs2T2)
Pred.rf.Rs2T2<- predict(
  rf.Rs2T2,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs2T2$num.trees)

prediction.rf.Rs2T2 <- data.frame(predsXY[,1:2], Pred.rf.Rs2T2$predictions)
prediction.rf.Rs2T2$Pred.rf.Rs2T2.predictions[ prediction.rf.Rs2T2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs2T2 <- rasterFromXYZ(prediction.rf.Rs2T2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs2T2, main= " Occurence Probability from 2000-2015 of Species 1 (Rs2_T2) by Random Forest")

#Rs2_T3
rf.Rs2T3<-ranger_sp(modSpecies.Rs2T3)
Pred.rf.Rs2T3<- predict(
  rf.Rs2T3,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs2T3$num.trees)

prediction.rf.Rs2T3 <- data.frame(predsXY[,1:2], Pred.rf.Rs2T3$predictions)
prediction.rf.Rs2T3$Pred.rf.Rs2T3.predictions[ prediction.rf.Rs2T3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs2T3 <- rasterFromXYZ(prediction.rf.Rs2T3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs2T3, main= " Occurence Probability from 2000-2023 of Species 1 (Rs2_T3) by Random Forest")

############EVALUATION PERFORMANCE############

#####Create table of results
auc.gam.Rs2.t1<- AUCFunzGAM(modSpecies.Rs2t1)
auc.gam.Rs2.t2<- AUCFunzGAM(modSpecies.Rs2t2)
auc.gam.Rs2.t3<- AUCFunzGAM(modSpecies.Rs2t3)

auc.gam.Rs2.T1<- AUCFunzGAM(modSpecies.Rs2t1)
auc.gam.Rs2.T2<- AUCFunzGAM(modSpecies.Rs2T2)
auc.gam.Rs2.T3<- AUCFunzGAM(modSpecies.Rs2T3)



auc.glm.Rs2.t1<- AUCFunzGLM(modSpecies.Rs2t1)
auc.glm.Rs2.t2<- AUCFunzGLM(modSpecies.Rs2t2)
auc.glm.Rs2.t3<- AUCFunzGLM(modSpecies.Rs2t3)

auc.glm.Rs2.T1<- AUCFunzGLM(modSpecies.Rs2t1)
auc.glm.Rs2.T2<- AUCFunzGLM(modSpecies.Rs2T2)
auc.glm.Rs2.T3<- AUCFunzGLM(modSpecies.Rs2T3)



auc.rf.Rs2.t1<- AUCFunzRF(modSpecies.Rs2t1)
auc.rf.Rs2.t2<- AUCFunzRF(modSpecies.Rs2t2)
auc.rf.Rs2.t3<- AUCFunzRF(modSpecies.Rs2t3)

auc.rf.Rs2.T1<- AUCFunzRF(modSpecies.Rs2t1)
auc.rf.Rs2.T2<- AUCFunzRF(modSpecies.Rs2T2)
auc.rf.Rs2.T3<- AUCFunzRF(modSpecies.Rs2T3)



tss.glm.Rs2.t1<- TSSFunzGLM(modSpecies.Rs2t1)
tss.glm.Rs2.t2<- TSSFunzGLM(modSpecies.Rs2t2)
tss.glm.Rs2.t3<- TSSFunzGLM(modSpecies.Rs2t3)

tss.glm.Rs2.T1<- TSSFunzGLM(modSpecies.Rs2t1)
tss.glm.Rs2.T2<- TSSFunzGLM(modSpecies.Rs2T2)
tss.glm.Rs2.T3<- TSSFunzGLM(modSpecies.Rs2T3)



tss.gam.Rs2.t1<- TSSFunzGAM(modSpecies.Rs2t1)
tss.gam.Rs2.t2<- TSSFunzGAM(modSpecies.Rs2t2)
tss.gam.Rs2.t3<- TSSFunzGAM(modSpecies.Rs2t3)

tss.gam.Rs2.T1<- TSSFunzGAM(modSpecies.Rs2t1)
tss.gam.Rs2.T2<- TSSFunzGAM(modSpecies.Rs2T2)
tss.gam.Rs2.T3<- TSSFunzGAM(modSpecies.Rs2T3)



tss.rf.Rs2.t1<- TSSFunzRF(modSpecies.Rs2t1)
tss.rf.Rs2.t2<- TSSFunzRF(modSpecies.Rs2t2)
tss.rf.Rs2.t3<- TSSFunzRF(modSpecies.Rs2t3)

tss.rf.Rs2.T1<- TSSFunzRF(modSpecies.Rs2t1)
tss.rf.Rs2.T2<- TSSFunzRF(modSpecies.Rs2T2)
tss.rf.Rs2.T3<- TSSFunzRF(modSpecies.Rs2T3)



boyce.glm.Rs2.t1<- BoyceGLM(modSpecies.Rs2t1)
boyce.glm.Rs2.t2<- BoyceGLM(modSpecies.Rs2t2)
boyce.glm.Rs2.t3<- BoyceGLM(modSpecies.Rs2t3)

boyce.glm.Rs2.T1<- BoyceGLM(modSpecies.Rs2t1)
boyce.glm.Rs2.T2<- BoyceGLM(modSpecies.Rs2T2)
boyce.glm.Rs2.T3<- BoyceGLM(modSpecies.Rs2T3)


boyce.gam.Rs2.t1<- BoyceGAM(modSpecies.Rs2t1)
boyce.gam.Rs2.t2<- BoyceGAM(modSpecies.Rs2t2)
boyce.gam.Rs2.t3<- BoyceGAM(modSpecies.Rs2t3)

boyce.gam.Rs2.T1<- BoyceGAM(modSpecies.Rs2t1)
boyce.gam.Rs2.T2<- BoyceGAM(modSpecies.Rs2T2)
boyce.gam.Rs2.T3<- BoyceGAM(modSpecies.Rs2T3)


boyce.rf.Rs2.t1<- BoyceRF(modSpecies.Rs2t1)
boyce.rf.Rs2.t2<- BoyceRF(modSpecies.Rs2t2)
boyce.rf.Rs2.t3<- BoyceRF(modSpecies.Rs2t3)

boyce.rf.Rs2.T1<- BoyceRF(modSpecies.Rs2t1)
boyce.rf.Rs2.T2<- BoyceRF(modSpecies.Rs2T2)
boyce.rf.Rs2.T3<- BoyceRF(modSpecies.Rs2T3)



#########Evaluation####
algorithm<- rep(c("GAM", "GLM", "RF"), each=6)
temporal_setting<-rep(c("t1", "t2", "t3", 'T1', "T2", "T3"), times = 3)
AUC <- c(auc.gam.Rs2.t1,auc.gam.Rs2.T1, auc.gam.Rs2.t2, auc.gam.Rs2.T2, auc.gam.Rs2.t3, auc.gam.Rs2.T3,
         auc.glm.Rs2.t1,auc.glm.Rs2.T1, auc.glm.Rs2.t2, auc.glm.Rs2.T2, auc.glm.Rs2.t3, auc.glm.Rs2.T3,
         auc.rf.Rs2.t1,auc.rf.Rs2.T1, auc.rf.Rs2.t2, auc.rf.Rs2.T2, auc.rf.Rs2.t3, auc.rf.Rs2.T3)
TSS<- c(tss.gam.Rs2.t1, tss.gam.Rs2.T1, tss.gam.Rs2.t2, tss.gam.Rs2.T2, tss.gam.Rs2.t3, tss.gam.Rs2.T3,
        tss.glm.Rs2.t1, tss.glm.Rs2.T1, tss.glm.Rs2.t2, tss.glm.Rs2.T2, tss.glm.Rs2.t3, tss.glm.Rs2.T3,
        tss.rf.Rs2.t1, tss.rf.Rs2.T1, tss.rf.Rs2.t2, tss.rf.Rs2.T2, tss.rf.Rs2.t3, tss.rf.Rs2.T3)
Boyce<- c(boyce.gam.Rs2.t1, boyce.gam.Rs2.T1, boyce.gam.Rs2.t2, boyce.gam.Rs2.T2, boyce.gam.Rs2.t3, boyce.gam.Rs2.T3,
          boyce.glm.Rs2.t1, boyce.glm.Rs2.T1, boyce.glm.Rs2.t2, boyce.glm.Rs2.T2, boyce.glm.Rs2.t3, boyce.glm.Rs2.T3,
          boyce.rf.Rs2.t1, boyce.rf.Rs2.T1, boyce.rf.Rs2.t2, boyce.rf.Rs2.T2, boyce.rf.Rs2.t3, boyce.rf.Rs2.T3)
perf_df <- data.frame(algorithm, temporal_setting, AUC, TSS, Boyce)

melted_perf <- perf_df %>% pivot_longer(cols = c(AUC, TSS, Boyce), names_to = "metric", values_to = "value")

ggplot(melted_perf, aes(x = algorithm, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  labs(title = "Performance of SDM Algorithms for Real Species 1-set 2", x = "Algorithms", y = "Performance") +
  scale_fill_manual(values = c("blue", "red", "green")) +
  theme_minimal() 
################################################################################################
#########################################Real Species 3#########################################
################################################################################################
####Rs3_t1/T1####
Rs3t1_GBIF <- read_csv("GBIF data/cleaned-data/s3-t1_T1.csv")
Rs3t1_loca<- data.frame(Rs3t1_GBIF$occurrenceStatus, Rs3t1_GBIF$decimalLongitude, Rs3t1_GBIF$decimalLatitude)
Rs3t1_loca$Rs3t1_GBIF.occurrenceStatus<- decostand(Rs3t1_loca$Rs3t1_GBIF.occurrenceStatus, method="pa")
colnames(Rs3t1_loca)<- c("Observed", "x", "y")
myRespXY.s3t1 <- Rs3t1_loca[, c('x', 'y')]

Rs3t1_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies3 t1-T1",
                                     expl.var = envi_stack,
                                     resp.var = Rs3t1_loca$Observed,
                                     resp.xy = myRespXY.s3t1,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs3t1_loca),
                                     PA.strategy = "random")
PseuAbs.Rs3t1<- data.frame(Rs3t1_formdat@data.species, Rs3t1_formdat@coord)
PseuAbs.Rs3t1[is.na(PseuAbs.Rs3t1)] <- 0
colnames(PseuAbs.Rs3t1)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs3t1)<-~x+y
crs(PseuAbs.Rs3t1)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs3t1<- raster::extract(envi, PseuAbs.Rs3t1, df=T)
values.Rs3t1<- values.Rs3t1[, -1] # first column not need it
values.Rs3t1$Observed<- c(PseuAbs.Rs3t1$Observed)
modSpecies.Rs3t1<-data.frame(pres= values.Rs3t1$Observed, values.Rs3t1[1:3])


####Rs3_t2####
Rs3t2_GBIF <- read_csv("GBIF data/cleaned-data/s3-t2.csv")
Rs3t2_loca<- data.frame(Rs3t2_GBIF$occurrenceStatus, Rs3t2_GBIF$decimalLongitude, Rs3t2_GBIF$decimalLatitude)
Rs3t2_loca$Rs3t2_GBIF.occurrenceStatus<- decostand(Rs3t2_loca$Rs3t2_GBIF.occurrenceStatus, method="pa")
colnames(Rs3t2_loca)<- c("Observed", "x", "y")
myRespXY.s3t2 <- Rs3t2_loca[, c('x', 'y')]

Rs3t2_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies3 t2",
                                     expl.var = envi_stack,
                                     resp.var = Rs3t2_loca$Observed,
                                     resp.xy = myRespXY.s3t2,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs3t2_loca),
                                     PA.strategy = "random")
PseuAbs.Rs3t2<- data.frame(Rs3t2_formdat@data.species, Rs3t2_formdat@coord)
PseuAbs.Rs3t2[is.na(PseuAbs.Rs3t2)] <- 0
colnames(PseuAbs.Rs3t2)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs3t2)<-~x+y
crs(PseuAbs.Rs3t2)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs3t2<- raster::extract(envi, PseuAbs.Rs3t2, df=T)
values.Rs3t2<- values.Rs3t2[, -1] # first column not need it
values.Rs3t2$Observed<- c(PseuAbs.Rs3t2$Observed)
modSpecies.Rs3t2<-data.frame(pres= values.Rs3t2$Observed, values.Rs3t2[1:3])



####Rs3_t3####
Rs3t3_GBIF <- read_csv("GBIF data/cleaned-data/s3-t3.csv")
Rs3t3_loca<- data.frame(Rs3t3_GBIF$occurrenceStatus, Rs3t3_GBIF$decimalLongitude, Rs3t3_GBIF$decimalLatitude)
Rs3t3_loca$Rs3t3_GBIF.occurrenceStatus<- decostand(Rs3t3_loca$Rs3t3_GBIF.occurrenceStatus, method="pa")
colnames(Rs3t3_loca)<- c("Observed", "x", "y")
myRespXY.s3t3 <- Rs3t3_loca[, c('x', 'y')]

Rs3t3_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies3 t3",
                                     expl.var = envi_stack,
                                     resp.var = Rs3t3_loca$Observed,
                                     resp.xy = myRespXY.s3t3,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs3t3_loca),
                                     PA.strategy = "random")
PseuAbs.Rs3t3<- data.frame(Rs3t3_formdat@data.species, Rs3t3_formdat@coord)
PseuAbs.Rs3t3[is.na(PseuAbs.Rs3t3)] <- 0
colnames(PseuAbs.Rs3t3)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs3t3)<-~x+y
crs(PseuAbs.Rs3t3)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs3t3<- raster::extract(envi, PseuAbs.Rs3t3, df=T)
values.Rs3t3<- values.Rs3t3[, -1] # first column not need it
values.Rs3t3$Observed<- c(PseuAbs.Rs3t3$Observed)
modSpecies.Rs3t3<-data.frame(pres= values.Rs3t3$Observed, values.Rs3t3[1:3])

####Rs3_T2####
Rs3T2_GBIF <- read_csv("GBIF data/cleaned-data/s3_T2.csv")
Rs3T2_loca<- data.frame(Rs3T2_GBIF$occurrenceStatus, Rs3T2_GBIF$decimalLongitude, Rs3T2_GBIF$decimalLatitude)
Rs3T2_loca$Rs3T2_GBIF.occurrenceStatus<- decostand(Rs3T2_loca$Rs3T2_GBIF.occurrenceStatus, method="pa")
colnames(Rs3T2_loca)<- c("Observed", "x", "y")
myRespXY.s3T2 <- Rs3T2_loca[, c('x', 'y')]

Rs3T2_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies3 T2",
                                     expl.var = envi_stack,
                                     resp.var = Rs3T2_loca$Observed,
                                     resp.xy = myRespXY.s3T2,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs3T2_loca),
                                     PA.strategy = "random")
PseuAbs.Rs3T2<- data.frame(Rs3T2_formdat@data.species, Rs3T2_formdat@coord)
PseuAbs.Rs3T2[is.na(PseuAbs.Rs3T2)] <- 0
colnames(PseuAbs.Rs3T2)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs3T2)<-~x+y
crs(PseuAbs.Rs3T2)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs3T2<- raster::extract(envi, PseuAbs.Rs3T2, df=T)
values.Rs3T2<- values.Rs3T2[, -1] # first column not need it
values.Rs3T2$Observed<- c(PseuAbs.Rs3T2$Observed)
modSpecies.Rs3T2<-data.frame(pres= values.Rs3T2$Observed, values.Rs3T2[1:3])


####Rs3_T3####
Rs3T3_GBIF <- read_csv("GBIF data/cleaned-data/s3_T3.csv")
Rs3T3_loca<- data.frame(Rs3T3_GBIF$occurrenceStatus, Rs3T3_GBIF$decimalLongitude, Rs3T3_GBIF$decimalLatitude)
Rs3T3_loca$Rs3T3_GBIF.occurrenceStatus<- decostand(Rs3T3_loca$Rs3T3_GBIF.occurrenceStatus, method="pa")
colnames(Rs3T3_loca)<- c("Observed", "x", "y")
myRespXY.s3T3 <- Rs3T3_loca[, c('x', 'y')]

###
Rs3T3_formdat<- BIOMOD_FormatingData(resp.name = "RSpecies3 T3",
                                     expl.var = envi_stack,
                                     resp.var = Rs3T3_loca$Observed,
                                     resp.xy = myRespXY.s3T3,
                                     PA.nb.rep=1,
                                     PA.nb.absences=3*nrow(Rs3T3_loca),
                                     PA.strategy = "random")
PseuAbs.Rs3T3<- data.frame(Rs3T3_formdat@data.species, Rs3T3_formdat@coord)
PseuAbs.Rs3T3[is.na(PseuAbs.Rs3T3)] <- 0
colnames(PseuAbs.Rs3T3)<- c("Observed", "x", "y")
coordinates(PseuAbs.Rs3T3)<-~x+y
crs(PseuAbs.Rs3T3)<-crs(envi)
#extract the envi vari value in the coordinates of each sample points 
values.Rs3T3<- raster::extract(envi, PseuAbs.Rs3T3, df=T)
values.Rs3T3<- values.Rs3T3[, -1] # first column not need it
values.Rs3T3$Observed<- c(PseuAbs.Rs3T3$Observed)
modSpecies.Rs3T3<-data.frame(pres= values.Rs3T3$Observed, values.Rs3T3[1:3])
####################SDM#####################
#####GLM#####
glm.Rs3t1<-multGLM(modSpecies.Rs3t1, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs3t1), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs3t1<- getPreds(envi, models=glm.Rs3t1$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs3t1<- data.frame(glm.Rs3t1$predictions)
crs(Pred.glm.Rs3t1) <- crs(envi)
plot(Pred.glm.Rs3t1, main= "Occurence Probability from 2000 to 2010 of Real-Species 1 (s1_t1) by GLM")

glm.Rs3t2<-multGLM(modSpecies.Rs3t2, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs3t2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs3t2<- getPreds(envi, models=glm.Rs3t2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs3t2<- data.frame(glm.Rs3t2$predictions)
crs(Pred.glm.Rs3t2) <- crs(envi)
plot(Pred.glm.Rs3t2, main= "Occurence Probability from 2011 to 2015 of Real-Species 1 (s1_t2) by GLM")

glm.Rs3t3<-multGLM(modSpecies.Rs3t3, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs3t3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs3t3<- getPreds(envi, models=glm.Rs3t3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs3t3<- data.frame(glm.Rs3t3$predictions)
crs(Pred.glm.Rs3t3) <- crs(envi)
plot(Pred.glm.Rs3t3, main= "Occurence Probability from 2016 to 2020 of Real-Species 1 (s1_t3) by GLM")

glm.Rs3T2<-multGLM(modSpecies.Rs3T2, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs3T2), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs3T2<- getPreds(envi, models=glm.Rs3T2$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs3T2<- data.frame(glm.Rs3T2$predictions)
crs(Pred.glm.Rs3T2) <- crs(envi)
plot(Pred.glm.Rs3T2, main= "Occurence Probability from 2000 to 2015 of Real-Species 1 (s1_T2) by GLM")

glm.Rs3T3<-multGLM(modSpecies.Rs3T3, sp.cols = 1, var.cols=2:ncol(modSpecies.Rs3T3), family = "binomial", trim = FALSE, step=F,Y.prediction =FALSE, P.prediction =TRUE,FDR = FALSE, Favourability =FALSE, test.sample = 0.2) 
Pred.glm.Rs3T3<- getPreds(envi, models=glm.Rs3T3$models, id.col = NULL, Y = FALSE, P = TRUE, Favourability =FALSE)
prediction.glm.Rs3T3<- data.frame(glm.Rs3T3$predictions)
crs(Pred.glm.Rs3T3) <- crs(envi)
plot(Pred.glm.Rs3T3, main= "Occurence Probability from 2000 to 2020 of Real-Species 1 (Rs3_T3) by GLM")

#####GAM#####
## Rs3_t1
gam.Rs3t1 <- GAM_sp(modSpecies.Rs3t1)
prediction.gam.Rs3t1<- predict(gam.Rs3t1,newdata = predsXY, type = "response")
df.pred.Rs3t1 <- data.frame(Pred=prediction.gam.Rs3t1)
prediction.gam.Rs3t1 <- data.frame(predsXY[,1:2], df.pred.Rs3t1$Pred)
Pred.gam.Rs3t1 <- rasterFromXYZ(prediction.gam.Rs3t1,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs3t1, main=" Occurence Probability from 2000 to 2010 of Real-Species 1 (Rs3_t1) by GAM")

## Rs3_t2
gam.Rs3t2 <- GAM_sp(modSpecies.Rs3t2)
prediction.gam.Rs3t2<- predict(gam.Rs3t2,newdata = predsXY, type = "response")
df.pred.Rs3t2 <- data.frame(Pred=prediction.gam.Rs3t2)
prediction.gam.Rs3t2 <- data.frame(predsXY[,1:2], df.pred.Rs3t2$Pred)
Pred.gam.Rs3t2 <- rasterFromXYZ(prediction.gam.Rs3t2,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs3t2, main=" Occurence Probability from 2011 to 2016 of Real-Species 1 (Rs3_t2) by GAM")

## Rs3_t3
gam.Rs3t3 <- GAM_sp(modSpecies.Rs3t3)
prediction.gam.Rs3t3<- predict(gam.Rs3t3,newdata = predsXY, type = "response")
df.pred.Rs3t3 <- data.frame(Pred=prediction.gam.Rs3t3)
prediction.gam.Rs3t3 <- data.frame(predsXY[,1:2], df.pred.Rs3t3$Pred)
Pred.gam.Rs3t3 <- rasterFromXYZ(prediction.gam.Rs3t3,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs3t3, main=" Occurence Probability from 2016 to 2020 of Real-Species 1 (Rs3_t3) by GAM")

## Rs3_T2
gam.Rs3T2 <- GAM_sp(modSpecies.Rs3T2)
prediction.gam.Rs3T2<- predict(gam.Rs3T2,newdata = predsXY, type = "response")
df.pred.Rs3T2 <- data.frame(Pred=prediction.gam.Rs3T2)
prediction.gam.Rs3T2 <- data.frame(predsXY[,1:2], df.pred.Rs3T2$Pred)
Pred.gam.Rs3T2 <- rasterFromXYZ(prediction.gam.Rs3T2,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs3T2, main=" Occurence Probability from 2011 to 2016 of Real-Species 1 (Rs3_T2) by GAM")

## Rs3_T3
gam.Rs3T3 <- GAM_sp(modSpecies.Rs3T3)
prediction.gam.Rs3T3<- predict(gam.Rs3T3,newdata = predsXY, type = "response")
df.pred.Rs3T3 <- data.frame(Pred=prediction.gam.Rs3T3)
prediction.gam.Rs3T3 <- data.frame(predsXY[,1:2], df.pred.Rs3T3$Pred)
Pred.gam.Rs3T3 <- rasterFromXYZ(prediction.gam.Rs3T3,crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.gam.Rs3T3, main=" Occurence Probability from 2016 to 2020 of Real-Species 1 (Rs3_T3) by GAM")


#####RF#####
#Rs3_t1
rf.Rs3t1<- ranger_sp(modSpecies.Rs3t1)
Pred.rf.Rs3t1<- predict(
  rf.Rs3t1,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs3t1$num.trees)

prediction.rf.Rs3t1 <- data.frame(predsXY[,1:2], Pred.rf.Rs3t1$predictions)
prediction.rf.Rs3t1$Pred.rf.Rs3t1.predictions[ prediction.rf.Rs3t1$Pred.predictions == 1] <- 1 - 2.2e-16  #so that the probability is not 100% (too arrogant) but a smaller amount that is the most significant close to 1
Pred.rf.Rs3t1 <- rasterFromXYZ(prediction.rf.Rs3t1, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs3t1, main= " Occurence Probability from 2000-2009 of Species 1 (Rs3_t1) by Random Forest")


#Rs3_t2
rf.Rs3t2<-ranger_sp(modSpecies.Rs3t2) 
Pred.rf.Rs3t2<- predict(
  rf.Rs3t2,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs3t2$num.trees)

prediction.rf.Rs3t2 <- data.frame(predsXY[,1:2], Pred.rf.Rs3t2$predictions)
prediction.rf.Rs3t2$Pred.rf.Rs3t2.predictions[ prediction.rf.Rs3t2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs3t2 <- rasterFromXYZ(prediction.rf.Rs3t2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs3t2, main= " Occurence Probability from 2010-2014 of Species 1 (Rs3_t2) by Random Forest")

#Rs3_t3
rf.Rs3t3<-ranger_sp(modSpecies.Rs3t3) 
Pred.rf.Rs3t3<- predict(
  rf.Rs3t3,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs3t3$num.trees)

prediction.rf.Rs3t3 <- data.frame(predsXY[,1:2], Pred.rf.Rs3t3$predictions)
prediction.rf.Rs3t3$Pred.rf.Rs3t3.predictions[ prediction.rf.Rs3t3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs3t3 <- rasterFromXYZ(prediction.rf.Rs3t3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs3t3, main= " Occurence Probability from 2015-2023 of Species 1 (Rs3_t3) by Random Forest")



## overlapping timescale (T)
#Rs3_T2
rf.Rs3T2<-ranger_sp(modSpecies.Rs3T2)
Pred.rf.Rs3T2<- predict(
  rf.Rs3T2,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs3T2$num.trees)

prediction.rf.Rs3T2 <- data.frame(predsXY[,1:2], Pred.rf.Rs3T2$predictions)
prediction.rf.Rs3T2$Pred.rf.Rs3T2.predictions[ prediction.rf.Rs3T2$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs3T2 <- rasterFromXYZ(prediction.rf.Rs3T2, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs3T2, main= " Occurence Probability from 2000-2015 of Species 1 (Rs3_T2) by Random Forest")

#Rs3_T3
rf.Rs3T3<-ranger_sp(modSpecies.Rs3T3)
Pred.rf.Rs3T3<- predict(
  rf.Rs3T3,
  data =predsXY,
  predict.all = FALSE,
  num.trees = rf.Rs3T3$num.trees)

prediction.rf.Rs3T3 <- data.frame(predsXY[,1:2], Pred.rf.Rs3T3$predictions)
prediction.rf.Rs3T3$Pred.rf.Rs3T3.predictions[ prediction.rf.Rs3T3$Pred.predictions == 1] <- 1 - 2.2e-16 
Pred.rf.Rs3T3 <- rasterFromXYZ(prediction.rf.Rs3T3, crs="+proj=longlat +datum=WGS84 +no_defs")
plot(Pred.rf.Rs3T3, main= " Occurence Probability from 2000-2023 of Species 1 (Rs3_T3) by Random Forest")
############EVALUATION PERFORMANCE############

#####Create table of results
auc.gam.Rs3.t1<- AUCFunzGAM(modSpecies.Rs3t1)
auc.gam.Rs3.t2<- AUCFunzGAM(modSpecies.Rs3t2)
auc.gam.Rs3.t3<- AUCFunzGAM(modSpecies.Rs3t3)

auc.gam.Rs3.T1<- AUCFunzGAM(modSpecies.Rs3t1)
auc.gam.Rs3.T2<- AUCFunzGAM(modSpecies.Rs3T2)
auc.gam.Rs3.T3<- AUCFunzGAM(modSpecies.Rs3T3)



auc.glm.Rs3.t1<- AUCFunzGLM(modSpecies.Rs3t1)
auc.glm.Rs3.t2<- AUCFunzGLM(modSpecies.Rs3t2)
auc.glm.Rs3.t3<- AUCFunzGLM(modSpecies.Rs3t3)

auc.glm.Rs3.T1<- AUCFunzGLM(modSpecies.Rs3t1)
auc.glm.Rs3.T2<- AUCFunzGLM(modSpecies.Rs3T2)
auc.glm.Rs3.T3<- AUCFunzGLM(modSpecies.Rs3T3)



auc.rf.Rs3.t1<- AUCFunzRF(modSpecies.Rs3t1)
auc.rf.Rs3.t2<- AUCFunzRF(modSpecies.Rs3t2)
auc.rf.Rs3.t3<- AUCFunzRF(modSpecies.Rs3t3)

auc.rf.Rs3.T1<- AUCFunzRF(modSpecies.Rs3t1)
auc.rf.Rs3.T2<- AUCFunzRF(modSpecies.Rs3T2)
auc.rf.Rs3.T3<- AUCFunzRF(modSpecies.Rs3T3)



tss.glm.Rs3.t1<- TSSFunzGLM(modSpecies.Rs3t1)
tss.glm.Rs3.t2<- TSSFunzGLM(modSpecies.Rs3t2)
tss.glm.Rs3.t3<- TSSFunzGLM(modSpecies.Rs3t3)

tss.glm.Rs3.T1<- TSSFunzGLM(modSpecies.Rs3t1)
tss.glm.Rs3.T2<- TSSFunzGLM(modSpecies.Rs3T2)
tss.glm.Rs3.T3<- TSSFunzGLM(modSpecies.Rs3T3)



tss.gam.Rs3.t1<- TSSFunzGAM(modSpecies.Rs3t1)
tss.gam.Rs3.t2<- TSSFunzGAM(modSpecies.Rs3t2)
tss.gam.Rs3.t3<- TSSFunzGAM(modSpecies.Rs3t3)

tss.gam.Rs3.T1<- TSSFunzGAM(modSpecies.Rs3t1)
tss.gam.Rs3.T2<- TSSFunzGAM(modSpecies.Rs3T2)
tss.gam.Rs3.T3<- TSSFunzGAM(modSpecies.Rs3T3)



tss.rf.Rs3.t1<- TSSFunzRF(modSpecies.Rs3t1)
tss.rf.Rs3.t2<- TSSFunzRF(modSpecies.Rs3t2)
tss.rf.Rs3.t3<- TSSFunzRF(modSpecies.Rs3t3)

tss.rf.Rs3.T1<- TSSFunzRF(modSpecies.Rs3t1)
tss.rf.Rs3.T2<- TSSFunzRF(modSpecies.Rs3T2)
tss.rf.Rs3.T3<- TSSFunzRF(modSpecies.Rs3T3)



boyce.glm.Rs3.t1<- BoyceGLM(modSpecies.Rs3t1)
boyce.glm.Rs3.t2<- BoyceGLM(modSpecies.Rs3t2)
boyce.glm.Rs3.t3<- BoyceGLM(modSpecies.Rs3t3)

boyce.glm.Rs3.T1<- BoyceGLM(modSpecies.Rs3t1)
boyce.glm.Rs3.T2<- BoyceGLM(modSpecies.Rs3T2)
boyce.glm.Rs3.T3<- BoyceGLM(modSpecies.Rs3T3)


boyce.gam.Rs3.t1<- BoyceGAM(modSpecies.Rs3t1)
boyce.gam.Rs3.t2<- BoyceGAM(modSpecies.Rs3t2)
boyce.gam.Rs3.t3<- BoyceGAM(modSpecies.Rs3t3)

boyce.gam.Rs3.T1<- BoyceGAM(modSpecies.Rs3t1)
boyce.gam.Rs3.T2<- BoyceGAM(modSpecies.Rs3T2)
boyce.gam.Rs3.T3<- BoyceGAM(modSpecies.Rs3T3)


boyce.rf.Rs3.t1<- BoyceRF(modSpecies.Rs3t1)
boyce.rf.Rs3.t2<- BoyceRF(modSpecies.Rs3t2)
boyce.rf.Rs3.t3<- BoyceRF(modSpecies.Rs3t3)

boyce.rf.Rs3.T1<- BoyceRF(modSpecies.Rs3t1)
boyce.rf.Rs3.T2<- BoyceRF(modSpecies.Rs3T2)
boyce.rf.Rs3.T3<- BoyceRF(modSpecies.Rs3T3)



#########Evaluation####
algorithm<- rep(c("GAM", "GLM", "RF"), each=6)
temporal_setting<-rep(c("t1", "t2", "t3", 'T1', "T2", "T3"), times = 3)
AUC <- c(auc.gam.Rs3.t1,auc.gam.Rs3.T1, auc.gam.Rs3.t2, auc.gam.Rs3.T2, auc.gam.Rs3.t3, auc.gam.Rs3.T3,
         auc.glm.Rs3.t1,auc.glm.Rs3.T1, auc.glm.Rs3.t2, auc.glm.Rs3.T2, auc.glm.Rs3.t3, auc.glm.Rs3.T3,
         auc.rf.Rs3.t1,auc.rf.Rs3.T1, auc.rf.Rs3.t2, auc.rf.Rs3.T2, auc.rf.Rs3.t3, auc.rf.Rs3.T3)
TSS<- c(tss.gam.Rs3.t1, tss.gam.Rs3.T1, tss.gam.Rs3.t2, tss.gam.Rs3.T2, tss.gam.Rs3.t3, tss.gam.Rs3.T3,
        tss.glm.Rs3.t1, tss.glm.Rs3.T1, tss.glm.Rs3.t2, tss.glm.Rs3.T2, tss.glm.Rs3.t3, tss.glm.Rs3.T3,
        tss.rf.Rs3.t1, tss.rf.Rs3.T1, tss.rf.Rs3.t2, tss.rf.Rs3.T2, tss.rf.Rs3.t3, tss.rf.Rs3.T3)
Boyce<- c(boyce.gam.Rs3.t1, boyce.gam.Rs3.T1, boyce.gam.Rs3.t2, boyce.gam.Rs3.T2, boyce.gam.Rs3.t3, boyce.gam.Rs3.T3,
          boyce.glm.Rs3.t1, boyce.glm.Rs3.T1, boyce.glm.Rs3.t2, boyce.glm.Rs3.T2, boyce.glm.Rs3.t3, boyce.glm.Rs3.T3,
          boyce.rf.Rs3.t1, boyce.rf.Rs3.T1, boyce.rf.Rs3.t2, boyce.rf.Rs3.T2, boyce.rf.Rs3.t3, boyce.rf.Rs3.T3)
perf_df <- data.frame(algorithm, temporal_setting, AUC, TSS, Boyce)

melted_perf <- perf_df %>% pivot_longer(cols = c(AUC, TSS, Boyce), names_to = "metric", values_to = "value")

ggplot(melted_perf, aes(x = algorithm, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  labs(title = "Performance of SDM Algorithms for Real Species 1-set 2", x = "Algorithms", y = "Performance") +
  scale_fill_manual(values = c("blue", "red", "green")) +
  theme_minimal() 


