NC.sample <-
function(popfile,nrefs,desvars,ctype,imax,nst){
## select NC sample in multiple strata (AOIs)
## inputs population file, strata sample sizes, names of design variables and clustering type
## outputs pre-existing plots, reference plots, target plots and centroid means 
popfile$Strata<-factor(popfile$Strata)
targs<-popfile[popfile$plot_type=='B' | popfile$plot_type=='T',]
cands<-popfile[popfile$plot_type=='B' | popfile$plot_type=='C',]
exist<-popfile[popfile$plot_type=='E',]
Ns<-table(targs$Strata)
Nc<-table(cands$Strata)
ne<-table(exist$Strata)
ns<-nrefs
nzst<-length(levels(factor(popfile$Strata)))
if(any(ns>Nc)) stop('sample size must be less than or equal to number of candidate plots')  
if(any(ns>Ns)) stop('sample size must be less than or equal to number of target plots')  
strata.levels<-levels(popfile$Strata)
if(length(ns)!=nzst) stop('sample size required for all strata')
refst<-targst<-cmnt<-exist.ids<-refs.ids<-targs.ids<-cmns<-NULL
for(i in 1:length(strata.levels)){
popi<-popfile[popfile$Strata==strata.levels[i],]
if(Ns[i]>0) {
desvarsi<-desvars
if(is.matrix(desvars)) desvarsi<-desvars[i,]
desvarsi<-desvarsi[desvarsi!=''];desvarsi<-desvarsi[desvarsi!=' ']
centroidsi<-Centroids(popi,ns[i],desvarsi,'km',imax,nst)
sampi<-NC.select(popi,ns[i],desvarsi,centroidsi$centroids)
existt<-data.frame(sampi$exist$PID)
refst<-data.frame(sampi$refs$PID)
targst<-data.frame(sampi$targs$PID)
cmnt<-data.frame(centroidsi$cmns)
refst$Strata<-targst$Strata<-cmnt$Strata<-strata.levels[i]
refst$Cluster<-cmnt$Cluster<-1:ns[i]
targst$Cluster<-1:Ns[i]
if(!is.null(cmns)) {
if(ne[i]>0) {
existt$Strata<-strata.levels[i]
exist.ids<-data.frame(rbind(exist.ids,existt))
}
refs.ids<-data.frame(rbind(refs.ids,refst))
targs.ids<-data.frame(rbind(targs.ids,targst))
}
if(is.null(cmns)) {
if(ne[i]>0) {
existt$Strata<-strata.levels[i]
exist.ids<-existt
}
refs.ids<-refst
targs.ids<-targst
cmns<-cmnt
}
rows.cmns<-dim(cmns)[1]
if(dim(cmnt)[2]==dim(cmns)[2]) cmns<-rbind(cmns,cmnt)
if(dim(cmnt)[2]!=dim(cmns)[2]){
temp<-data.frame(matrix(NA,sum(ns[1:i]),max(dim(cmns)[2],dim(cmnt)[2])))
names(temp)<-unique(c(names(cmns),names(cmnt)))
temp[1:rows.cmns,match(names(cmns),names(temp))]<-cmns
temp[(1+rows.cmns):dim(temp)[1],match(names(cmnt),names(temp))]<-cmnt
cmns<-temp
}
}
}
names(refs.ids)[1]<-names(targs.ids)[1]<-'PID'
if(sum(ne)>0) names(exist.ids)[1]<-'PID'
if(sum(ne)>0) exist.ids$Strata<-factor(exist.ids$Strata)
refs.ids$Strata<-factor(refs.ids$Strata)
targs.ids$Strata<-factor(targs.ids$Strata)
plot_type<-as.character(popfile$plot_type)
plot_type[match(refs.ids$PID,popfile$PID)]<-'R'
popfile$plot_type<-factor(plot_type)
cmns$Strata<-factor(cmns$Strata)
list(popfile=popfile,cmns=cmns)
}
