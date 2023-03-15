library(virtualspecies)
library(rnaturalearth)
library(tidyverse)
library(fuzzySim)
library(mgcv)
library(ranger)
library(PresenceAbsence)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gghalves)
library(biomod2)
library(readr)
library(vegan)
library(FSA)

temp<-raster("wc2.1_10m_bio_1.tif")
preci<- raster("wc2.1_10m_bio_12.tif")
elev<- raster("wc2.1_10m_elev.tif")
Asia<- ne_countries(scale="medium", type="map_units", returnclass="sf", country=c('Myanmar','Thailand','Laos','Cambodia','Vietnam'))
study_area<- select(Asia, geometry, name_long)
elev<- mask(elev,study_area)
temp<- mask(temp,study_area)
preci<- mask(preci,study_area)

envi_stack<-stack(elev, temp, preci)
names(envi_stack)<- c("elev", "temp", "preci")
ex<- extent(90, 112, 4, 30)
envi<- crop(envi_stack, ex)
names(envi)<- c("elev", "temp", "preci")
plot(envi)



####Creating functions for GAM and RF calibration with split train/test dataset
GAM_sp <- function(x,TrainValue = 0.8, TestValue = 0.2) {
  library(groupdata2)
  library(ROCR)
  set.seed(999)
  inds <- partition(x, p = c(train = TrainValue, test = TestValue))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  sp_cols <- 1
  pred_cols <- 2:ncol(training)
  names(training)[sp_cols]
  names(training)[pred_cols]
  form_gam <- as.formula(paste0(names(training)[sp_cols], "~", paste0("s(", names(training)[pred_cols], ")", collapse = "+")))
  Model <- gam(form_gam, family = binomial, data = training)
  return(Model)
}

ranger_sp <- function(x,TrainValue = 0.8, TestValue = 0.2) {
  library(groupdata2)
  library(ROCR)
  set.seed(999)
  inds <- partition(x, p = c(train = TrainValue, test = TestValue))
  train <- as.data.frame(inds[1])
  test <- as.data.frame(inds[2])
  validation<-test
  training<-train
  Model<-ranger(training$pres ~., data= training, importance='impurity')
  return(Model)
  }
