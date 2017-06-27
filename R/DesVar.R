DesVar <-
function(popfile,nrefs,desvars,yvars,kvalue,B=1000,zvars=NULL,training=NULL,xvars=NULL,pool=F){
## calculate design variances based on repeated sampling of target set,
## with number of starts in clustering procedure equal to 1
## inputs population file, sample sizes, design variables, dependent variables, k value, number of realizations,
## predictor variable, optional trainning data and auxiliary variables
## if training data provided adds a predictor variable to the population file 
## calculates design rse for each stratum and each variable if interest
if(!is.null(training) & is.null(yvars)) stop('y-variable must be specified and included in the training data')
if(!is.null(training) & is.null(xvars)) stop('x-variable must be specified and included in the training data')
if(is.null(training) & is.null(zvars)) stop('z-variable must be specified and included in popfile if there is no training data')
if(is.null(training) & any(is.na(match(zvars,names(popfile))))) stop('z-variable must be specified and included in popfile if there is no training data')
if(!is.null(training) & is.na(sum(match(yvars,names(training))))) stop('y-variable must be specified and included in the training data')
if(!is.null(training) & is.na(sum(match(xvars,names(training))))) stop('x-variable must be specified and included in the training data')
if(!is.null(yvars) & !is.null(zvars) & length(yvars)!= length(zvars)) stop('if both present, zvars and yvars must be the same length')
if(!is.null(training) & !is.null(zvars)) popfile<-popfile[,names(popfile)!=zvars]
if(!is.null(training)){
temp<-Addz(popfile,training,yvars,xvars,pool)
print(' ************** Results of random forest models ****************')
print(temp$r.squared)
popfile<-temp$popfile
pvars<-paste(yvars,'.pred',sep='')
if(!is.null(zvars)) names(popfile)[match(pvars,names(popfile))]<-zvars
if(is.null(zvars)) zvars<-pvars
}
popfile$plot_type<-factor(popfile$plot_type,levels=c('B','C','E','R','T','X'))
targs<-popfile[popfile$plot_type=='B' | popfile$plot_type=='T',]
ng<-table(targs$Strata)
nst<-length(unique(popfile$Strata))
if(length(nrefs)!=nst) stop('sample sizes vector must have length equal to number of strata')
df<-NULL
actai<-matrix(0,nst,length(yvars)) ####actai<-matrix(0,nst,length(zvars))
estai<-estrai<-array(0,c(nst,B,length(yvars))) ####estai<-estrai<-array(0,c(nst,B,length(zvars)))
for(i in 1:nst){
this.stratum<-levels(targs$Strata)[i]
ni<-nrefs[i]
popi<-popfile[popfile$Strata==this.stratum,]
dfi<-data.frame(yvars,rep(this.stratum,length(yvars)),rep(ng[i],length(yvars)),rep(ni,length(yvars)),
rep(NA,length(yvars)),rep(NA,length(yvars)),rep(NA,length(yvars)),rep(NA,length(yvars)))
names(dfi)<-c('Var','Strata','Ng','n','mse','rmse.perc','Rel.Eff','mser')
if(ni<2 | is.na(ni) | ng[i]<2) {tempv<-NULL;df<-rbind(df,dfi)}
if(!is.na(ni)){
if(ni>1 & ng[i]>1){
kv<-min(kvalue,ni-1)
tempv<-DVar(popi,ni,zvars,desvars,kv,B=B)
if(!is.null(tempv$df)) df<-rbind(df,tempv$df)
if(length(yvars)==1) actai[i]<-tempv$act
if(length(yvars)>1) actai[i,]<-tempv$act
if(length(yvars)==1) estai[i,,1]<-tempv$est
if(length(yvars)>1) estai[i,,]<-tempv$est
if(length(yvars)==1) estrai[i,,1]<-tempv$estr
if(length(yvars)>1) estrai[i,,]<-tempv$estr
}
}
}
dfo<-NULL
for(i in 1:length(yvars)){
dfi<-df[df$Var==zvars[i],]
acta<-sum(dfi$Ng*actai[,i])/sum(dfi$Ng)
if(nst==1) esta<-estai[,,i] 
if(nst>1) esta<-apply(dfi$Ng * estai[,,i],2,sum) /sum(dfi$Ng)
if(nst==1) estra<-estrai[,,i]
if(nst>1) estra<-apply(dfi$Ng * estrai[,,i],2,sum) /sum(dfi$Ng)
ovv<-mean((acta-esta)^2)
ovvr<-mean((acta-estra)^2)
omn<-acta
ovrmse<-100*sqrt(ovv)/omn
ovrrmse<-100*sqrt(ovvr)/omn
ovre<-ovvr/ovv
temp<-dfi[1,]
temp$Strata<-'Overall'
temp$Ng<-sum(dfi$Ng)
temp$n<-sum(dfi$n)
temp$mse<-ovv
temp$rmse.perc<-ovrmse
temp$Rel.Eff<-ovre
temp$mser<-ovvr
if(i==1) dfo<-temp
if(i>1) dfo<-rbind(dfo,temp)
dfo$mse<-round(dfo$mse,2)
dfo$rmse.perc<-round(dfo$rmse.perc,2)
dfo$Rel.Eff<-round(dfo$Rel.Eff,1)
}
des.mse<-rbind(df,dfo)
des.mse[des.mse$Var==zvars,c('Var','Strata','Ng','n','mse','rmse.perc','Rel.Eff')]
}
