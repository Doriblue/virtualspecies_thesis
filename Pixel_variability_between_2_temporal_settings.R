########Evaluate temporal settings (t vs T)##########
over.s1<- stack(Pred.rf.s1t1, Pred.rf.s1t2, Pred.rf.s1t3) 
n_over.s1<- stack(Pred.rf.s1T1, Pred.rf.s1T2, Pred.rf.s1T3)
cv.over.s1<- cv(over.s1, aszero = FALSE, na.rm = TRUE)
cv.n_over.s1 <- cv(n_over.s1, na.rm = TRUE, aszero = FALSE)
par(mfrow=c(1,2))
plot(cv.over.s1,main=paste("CV of overlapping SDMs- Virtual Species 1"))
plot(cv.n_over.s1,main=paste("CV of non-verlapping SDMs- Virtual Species 1"))
hist(cv.over.s1,main=paste("CV of overlapping SDMs- Virtual Species 1"))
hist(cv.n_over.s1, main=paste("CV of non-overlapping SDMs- Virtual Species 1"))

over.s2<- stack(Pred.rf.s2t1, Pred.rf.s2t2, Pred.rf.s2t3)
n_over.s2<- stack(Pred.rf.s2T1,Pred.rf.s2T2, Pred.rf.s2T3)
cv.over.s2<- cv(over.s2)
cv.n_over.s2 <- cv(n_over.s2)
par(mfrow=c(1,2))
plot(cv.over.s2, main=paste("CV of overlapping SDMs- Virtual Species 2"))
plot(cv.n_over.s2, main=paste("CV of non-overlapping SDMs- Virtual Species 2"))
hist(cv.over.s2,main=paste("CV of overlapping SDMs- Virtual Species 2"))
hist(cv.n_over.s2, main=paste("CV of non-overlapping SDMs- Virtual Species 2"))

over.s3<- stack(Pred.rf.s3t1, Pred.rf.s3t2, Pred.rf.s3t3)
n_over.s3<- stack(Pred.rf.s3T1,Pred.rf.s3T2, Pred.rf.s3T3)
cv.over.s3<- cv(over.s3)
cv.n_over.s3 <- cv(n_over.s3)
par(mfrow=c(1,2))
plot(cv.over.s3,main=paste("CV of overlapping SDMs- Virtual Species 3"))
plot(cv.n_over.s3,main=paste("CV of non-overlapping SDMs- Virtual Species 3"))
hist(cv.over.s3,main=paste("CV of overlapping SDMs- Virtual Species 3"))
hist(cv.n_over.s3, main=paste("CV of non-overlapping SDMs- Virtual Species 3"))

########Evaluate temporal settings (t vs T) of REAL SPECIES##########
over.Rs1<- stack(Pred.rf.Rs1t1, Pred.rf.Rs1t2, Pred.rf.Rs1t3)
n_over.Rs1<- stack(Pred.rf.Rs1t1, Pred.rf.Rs1T2, Pred.rf.Rs1T3)
cv.over.Rs1<- cv(over.Rs1)
cv.n_over.Rs1 <- cv(n_over.Rs1)
par(mfrow=c(1,2))
plot(cv.over.Rs1,main=paste("CV of overlapping SDMs- Real Species 1"))
plot(cv.n_over.Rs1,main=paste("CV of non-overlapping SDMs- Real Species 1"))
hist(cv.over.Rs1,main=paste("CV of overlapping SDMs- Real Species 1"))
hist(cv.n_over.Rs1, main=paste("CV of non-overlapping SDMs- Real Species 1"))

over.Rs2<- stack(Pred.rf.Rs2t1, Pred.rf.Rs2t2, Pred.rf.Rs2t3)
n_over.Rs2<- stack(Pred.rf.Rs2t1,Pred.rf.Rs2T2, Pred.rf.Rs2T3)
cv.over.Rs2<- cv(over.Rs2)
cv.n_over.Rs2 <- cv(n_over.Rs2)
par(mfrow=c(1,2))
plot(cv.over.Rs2,main=paste("CV of overlapping SDMs- Real Species 2"))
plot(cv.n_over.Rs2,main=paste("CV of non-overlapping SDMs- Real Species 2"))
hist(cv.over.Rs2,main=paste("CV of overlapping SDMs- Real Species 2"))
hist(cv.n_over.Rs2, main=paste("CV of non-overlapping SDMs- Real Species 2"))

over.Rs3<- stack(Pred.rf.Rs3t1, Pred.rf.Rs3t2, Pred.rf.Rs3t3)
n_over.Rs3<- stack(Pred.rf.Rs3t1,Pred.rf.Rs3T2, Pred.rf.Rs3T3)
cv.over.Rs3<- cv(over.Rs3)
cv.n_over.Rs3 <- cv(n_over.Rs3)
par(mfrow=c(1,2))
plot(cv.over.Rs3,main=paste("CV of overlapping SDMs- Real Species 3"))
plot(cv.n_over.Rs3,main=paste("CV of non-overlapping SDMs- Real Species 3"))
hist(cv.over.Rs3,main=paste("CV of overlapping SDMs- Real Species 3"))
hist(cv.n_over.Rs3, main=paste("CV of non-overlapping SDMs- Real Species 3"))


