R.sample1 <-
function(popfile,nrefs){
## select random sample in single strata (AOI)
## inputs population file, sample size; outputs population file  
targs<-popfile[popfile$plot_type=='B' | popfile$plot_type=='T',]
cands<-popfile[popfile$plot_type=='B' | popfile$plot_type=='C',]
exist<-popfile[popfile$plot_type=='E',]
Ng<-dim(targs)[1]
Nc<-dim(cands)[1]
ne<-dim(exist)[1]
if(nrefs>Nc) stop('sample size must be less than or equal to number of candidate plots')  
if(Ng>0) {
sampi<-cands[sample(1:Nc,size=nrefs,replace=F),]
refs.ids<-data.frame(sampi$PID)
refs.ids$Strata<-targs$Strata[1]
}
names(refs.ids)[1]<-'PID'; #names(targs.ids)[1]<-'PID'
plot_type<-as.character(popfile$plot_type)
plot_type[match(refs.ids$PID,popfile$PID)]<-'R'
popfile$plot_type<-factor(plot_type)
temp<-list(popfile=popfile)
temp
}
