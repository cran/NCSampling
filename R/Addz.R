Addz <-
function(popfile,training,yvars,xvars,pool=F){
## fit a random forest regression to the training data and add predicted values to the population file
## inputs population file, training data, dependent variables and auxiliary variables
## adds a predicted value to each virtual plot for each for each dependent variable
## returns population file with added variables and r.squared values from regression models
## model can be fitted by stratum (preferred) or pooled across strata if insufficient training data
yvt<-match(yvars,names(training))
if(any(is.na(yvt))) stop('missing dependent variable/s in training data')  
xvt<-match(xvars,names(training))
if(any(is.na(xvt))) stop('missing auxiliary variable/s in training data') 
xvp<-match(xvars,names(popfile))
if(any(is.na(xvp))) stop('missing auxiliary variable/s in population data') 
N<-dim(popfile)[1]
nst<-length(levels(popfile$Strata))
if(pool==F){
name.p<-NULL
rsq<-matrix(0,nst,length(yvars))
for(j in 1:nst){
this.stratum<-levels(popfile$Strata)[j]
popj<-popfile[popfile$Strata==this.stratum,]
Ng<-dim(popj)[1]
for(i in 1:length(yvars)){
tda<-training[training$Strata==this.stratum,c(yvars[i],xvars)]
names(tda)[1]<-'Y'
rfm<-randomForest(Y~.,data=tda)
rsq[j,i]<-100*rev(rfm$rsq)[1]
zpj<-predict(rfm,newdata=popj)
eps<-var(tda$Y)-var(zpj)
zpc<-zpj+rnorm(Ng,0,sqrt(eps))
zpc<-pmax(zpc,0)
nc<-ncol(popj)
popj[,(nc+1)]<-zpc  
name.p<-c(name.p,yvars[i])
names(popj)[nc+1]<-paste(yvars[i],'.pred',sep='')
}
if(j==1) popa<-popj
if(j>1) popa<-rbind(popa,popj)
}
}
if(pool==T){
name.p<-NULL
rsq<-numeric(length(yvars))
for(i in 1:length(yvars)){
tda<-training[,c(yvars,'Strata',xvars)]
names(tda)[1]<-'Y'
rfm<-randomForest(Y~.,data=tda)
rsq[i]<-100*rev(rfm$rsq)[1]
zp<-predict(rfm,newdata=popfile)
eps<-var(tda$Y)-var(zp)
zpc<-zp+rnorm(N,0,sqrt(eps))
zpc<-pmax(zpc,0)
nc<-ncol(popfile)
popfile[,(nc+1)]<-zpc  
name.p<-c(name.p,yvars[i])
names(popfile)[nc+1]<-paste(yvars[i],'.pred',sep='')
popa<-popfile
}
}
if(pool==F) {rsqd<-data.frame(levels(popfile$Strata),name.p,c(round(rsq,1)))
names(rsqd)<-c('Strata','Y.variable','R.squared')}
if(pool==T) {rsqd<-data.frame(name.p,c(round(rsq,1)))
names(rsqd)<-c('Y.variable','R.squared')}
list(popfile=popa,r.squared=rsqd)
}
