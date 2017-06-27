DVar <-
function(popfile,nrefs,yvars,desvars,kvalue,B=1000){
## calculate design variances based on repeated sampling of target set,
## with number of starts in clustering procedure equal to 1
## inputs population file, sample size, dependent variables, design variables, k-value and number of realizations 
## if training details provided adds a predictor variable to the population file 
## calculates design rse for a single stratum stratum for each dependent variable
targs<-popfile[popfile$plot_type=='B' | popfile$plot_type=='T',]
ng<-length(targs$Strata)
targsx<-popfile[popfile$plot_type=='X',]
nx<-length(targsx$Strata)
exist<-popfile[popfile$plot_type=='E',]
ne<-length(exist$Strata)
if(nrefs>ng) stop(paste('sample size is larger than target population in stratum',popfile$Strata,sep=' ')) 
lvar<-length(yvars)
if(lvar==1) act<-mean(targs[,yvars])
if(lvar>1) act<-apply(targs[,yvars],2,mean)
est<-estr<-matrix(0,B,lvar)
for(k in 1:2){ # for both random and NC samples
for(j in 1:B){
## resample population file
if(k==1) temp<-NC.sample(popfile,nrefs,desvars,'km',20,1)$popfile
if(k==2) temp<-R.sample1(popfile,nrefs)$popfile
refs<-temp[temp$plot_type=='R' | temp$plot_type=='E',]
targs<-temp[temp$plot_type=='B' | temp$plot_type=='T' | temp$plot_type=='X',]
dim(refs);dim(targs)
impt<-NULL
row.names(refs)<-refs$PID;row.names(targs)<-targs$PID
if(ng>0){
nn<-yai(x=refs[,desvars],y=refs[,yvars],method='euclidean',k=kvalue)
nntarg<-newtargets(nn,targs,k=kvalue) 
## impute vars in target plots
if(lvar==1) impt<-impute(nntarg,vars=yvars(nntarg),k=kvalue)[,1]
if(lvar>1) impt<-impute(nntarg,vars=yvars(nntarg),k=kvalue)[,yvars]
if(lvar==1 & k==1) est[j,]<-mean(impt)
if(lvar>1 & k==1) est[j,]<-apply(impt,2,mean)
if(lvar==1 & k==2) estr[j,]<-mean(impt)
if(lvar>1 & k==2) estr[j,]<-apply(impt,2,mean)
}
}}
mse<-mser<-numeric(lvar)
for(j in 1:lvar){
mse[j]<-mean((act[j]-est[,j])^2)
mser[j]<-mean((act[j]-estr[,j])^2)
}
rmse<-round(100*sqrt(mse)/act,2)
rmser<-round(100*sqrt(mser)/act,2)
re<-round(mser/mse,1)
df<-data.frame(yvars,popfile$Strata[1],rep(ng+nx,lvar),rep(nrefs+ne,lvar),round(mse,3),rmse,re,round(mser,3))
names(df)<-c('Var','Strata','Ng','n','mse','rmse.perc','Rel.Eff','mser')
row.names(df)<-1:length(yvars)
list(df=df,act=act,est=est,estr=estr)
}
