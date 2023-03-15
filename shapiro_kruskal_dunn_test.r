##########SPECIES 1###########
######Shapiro test####

st.Rs1t1<- shapiro.test( sample( prediction.rf.Rs1t1$Pred.rf.Rs1t1.predictions, size = 5000))
st.Rs1t2<- shapiro.test( sample( prediction.rf.Rs1t2$Pred.rf.Rs1t2.predictions, size = 5000))
st.Rs1t3<- shapiro.test( sample( prediction.rf.Rs1t3$Pred.rf.Rs1t3.predictions, size = 5000))

st.Rs1T1<- shapiro.test( sample( prediction.rf.Rs1T1$Pred.rf.Rs1T1.predictions, size = 5000))
st.Rs1T2<- shapiro.test( sample( prediction.rf.Rs1T2$Pred.rf.Rs1T2.predictions, size = 5000))
st.Rs1T3<- shapiro.test( sample( prediction.rf.Rs1T3$Pred.rf.Rs1T3.predictions, size = 5000))

######Kruskal test######
Rsp1t1<-data.frame(value=prediction.rf.Rs1t1$Pred.rf.Rs1t1.predictions)
Rsp1t1$group<- "t1"
Rsp1t2<-data.frame(value=prediction.rf.Rs1t2$Pred.rf.Rs1t2.predictions)
Rsp1t2$group<- "t2"
Rsp1t3<-data.frame(value=prediction.rf.Rs1t3$Pred.rf.Rs1t3.predictions)
Rsp1t3$group<- "t3"

Rsp1T1<-data.frame(value=prediction.rf.Rs1t1$Pred.rf.Rs1T1.predictions)
Rsp1T1$group<- "T1"
Rsp1T2<-data.frame(value=prediction.rf.Rs1T2$Pred.rf.Rs1T2.predictions)
Rsp1T2$group<- "T2"
Rsp1T3<-data.frame(value=prediction.rf.Rs1T3$Pred.rf.Rs1T3.predictions)
Rsp1T3$group<- "T3"


df.Rsp1.n_over<- as.data.frame(rbind(Rsp1t1, Rsp1t2, Rsp1t3))
df.Rsp1.over<- as.data.frame(rbind(Rsp1T1, Rsp1T2, Rsp1T3))
K.Rsp1.n_over<- kruskal.test(value~group, data = df.Rsp1.n_over)
K.Rsp1.over<- kruskal.test(value~group, data = df.Rsp1.over)

#####Dunn test####
library("FSA")
df.Rsp1.n_over<- as.data.frame(rbind( Rsp1t2, Rsp1t3))
df.Rsp1.over<- as.data.frame(rbind( Rsp1T2, Rsp1T3))
df.Rsp1.dunn<- as.data.frame(rbind(df.Rsp1.n_over, df.Rsp1.over))
D.Rsp1<- dunnTest( value~group, data = df.Rsp1.dunn) # result we can see that the comparison of whatever to T3 all have a really small p (e-36), when the other are e-2/5/9 => T3 are more significantly different than all the other times (however, T3 are the overlaping time cover all the time period, it should contain all the other time period and therefore, it should be the least different with the others!!)





######Species 2#####
##Shapiro##
st.Rs2t1<- shapiro.test( sample( prediction.rf.Rs2t1$Pred.rf.Rs2t1.predictions, size = 5000))
st.Rs2t2<- shapiro.test( sample( prediction.rf.Rs2t2$Pred.rf.Rs2t2.predictions, size = 5000))
st.Rs2t3<- shapiro.test( sample( prediction.rf.Rs2t3$Pred.rf.Rs2t3.predictions, size = 5000))

st.Rs2T1<- shapiro.test( sample( prediction.rf.Rs2t1$Pred.rf.Rs2t1.predictions, size = 5000))
st.Rs2T2<- shapiro.test( sample( prediction.rf.Rs2T2$Pred.rf.Rs2T2.predictions, size = 5000))
st.Rs2T3<- shapiro.test( sample( prediction.rf.Rs2T3$Pred.rf.Rs2T3.predictions, size = 5000))
######Kruskal test######
Rsp2t1<-data.frame(value=prediction.rf.s2t1$Pred.rf.s2t1.predictions)
Rsp2t1$group<- "t1"
Rsp2t2<-data.frame(value=prediction.rf.s2t2$Pred.rf.s2t2.predictions)
Rsp2t2$group<- "t2"
Rsp2t3<-data.frame(value=prediction.rf.s2t3$Pred.rf.s2t3.predictions)
Rsp2t3$group<- "t3"

Rsp2T1<-data.frame(value=prediction.rf.s2T1$Pred.rf.s2T1.predictions)
Rsp2T1$group<- "T1"
Rsp2T2<-data.frame(value=prediction.rf.s2T2$Pred.rf.s2T2.predictions)
Rsp2T2$group<- "T2"
Rsp2T3<-data.frame(value=prediction.rf.s2T3$Pred.rf.s2T3.predictions)
Rsp2T3$group<- "T3"


df.Rsp2.n_over<- as.data.frame(rbind(Rsp2t1, Rsp2t2, Rsp2t3))
df.Rsp2.over<- as.data.frame(rbind(Rsp2T1, Rsp2T2, Rsp2T3))
K.Rsp2.n_over<- kruskal.test(value~group, data = df.Rsp2.n_over)
K.Rsp2.over<- kruskal.test(value~group, data = df.Rsp2.over)

#####Dunn test####
library("FSA")
df.Rsp2.n_over<- as.data.frame(rbind( Rsp2t2, Rsp2t3))
df.Rsp2.over<- as.data.frame(rbind( Rsp2T2, Rsp2T3))
df.Rsp2.dunn<- as.data.frame(rbind(df.Rsp2.n_over, df.Rsp2.over))
D.Rsp2<- dunnTest( value~group, data = df.Rsp2.dunn)


#####Species 3####
####Shapiro test####
st.s3t1<- shapiro.test( sample( prediction.rf.s3t1$Pred.rf.s3t1.predictions, size = 5000))
st.s3t2<- shapiro.test( sample( prediction.rf.s3t2$Pred.rf.s3t2.predictions, size = 5000))
st.s3t3<- shapiro.test( sample( prediction.rf.s3t3$Pred.rf.s3t3.predictions, size = 5000))

st.s3T1<- shapiro.test( sample( prediction.rf.s3T1$Pred.rf.s3T1.predictions, size = 5000))
st.s3T2<- shapiro.test( sample( prediction.rf.s3T2$Pred.rf.s3T2.predictions, size = 5000))
st.s3T3<- shapiro.test( sample( prediction.rf.s3T3$Pred.rf.s3T3.predictions, size = 5000))
######Kruskal test######
Rsp3t1<-data.frame(value=prediction.rf.s3t1$Pred.rf.s3t1.predictions)
Rsp3t1$group<- "t1"
Rsp3t2<-data.frame(value=prediction.rf.s3t2$Pred.rf.s3t2.predictions)
Rsp3t2$group<- "t2"
Rsp3t3<-data.frame(value=prediction.rf.s3t3$Pred.rf.s3t3.predictions)
Rsp3t3$group<- "t3"

Rsp3T1<-data.frame(value=prediction.rf.s3T1$Pred.rf.s3T1.predictions)
Rsp3T1$group<- "T1"
Rsp3T2<-data.frame(value=prediction.rf.s3T2$Pred.rf.s3T2.predictions)
Rsp3T2$group<- "T2"
Rsp3T3<-data.frame(value=prediction.rf.s3T3$Pred.rf.s3T3.predictions)
Rsp3T3$group<- "T3"


df.Rsp3.n_over<- as.data.frame(rbind(Rsp3t1, Rsp3t2, Rsp3t3))
df.Rsp3.over<- as.data.frame(rbind(Rsp3T1, Rsp3T2, Rsp3T3))
K.Rsp3.n_over<- kruskal.test(value~group, data = df.Rsp3.n_over)
K.Rsp3.over<- kruskal.test(value~group, data = df.Rsp3.over)

#####Dunn test####
library("FSA")
df.Rsp3.n_over<- as.data.frame(rbind( Rsp3t2, Rsp3t3))
df.Rsp3.over<- as.data.frame(rbind( Rsp3T2, Rsp3T3))
df.Rsp3.dunn<- as.data.frame(rbind(df.Rsp3.n_over, df.Rsp3.over))
D.Rsp3<- dunnTest( value~group, data = df.Rsp3.dunn)
