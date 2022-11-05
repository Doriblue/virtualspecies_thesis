####SET UP###
library(virtualspecies)
library(rnaturalearth)
library(tidyverse)
temp<-raster("wc2.1_10m_bio_1.tif")
preci<- raster("wc2.1_10m_bio_12.tif")
elev<- raster("wc2.1_10m_elev.tif")
Asia<- ne_countries(scale="medium", type="map_units", returnclass="sf", country=c('Malaysia','Thailand','Laos','Cambodia','Vietnam'))
study_area<- select(Asia, geometry, name_long)
elev<- mask(elev,study_area)
temp<- mask(temp,study_area)
preci<- mask(preci,study_area)

envi<-stack(elev, temp, preci)
envi<- crop(envi, ex)
names(envi)<- c("elev", "temp", "preci")
plot(envi)




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

####GLM####
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


###########GAM####

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
