Centroids <-
function(popfile,nrefs,desvars,ctype,imax,nst){
## separate target plots into clusters - single stratum only
## inputs population file, sample size, design vars, clustering type, 
## maximum number of iterations, number of sets of starting values (centres)
## outputs centroids and centroids means
targs<-popfile[popfile$plot_type=='B' | popfile$plot_type=='T',]
cands<-popfile[popfile$plot_type=='B' | popfile$plot_type=='C',]
ntargs<-dim(targs)[1]
ncands<-dim(cands)[1]
# create scaled design variables
sds<-apply(targs[,desvars],2,sd) 
bvars<-scale(targs[,desvars])[,sds>0] # remove design variables with zero variance
if(any(sds==0)) warning('one or more design variables have zero variance')
if(nrefs>=ncands) {
# since there are fewer candidate plots than reference plots no clustering is necessary 
targs$cluster<-1:ntargs
cmns<-matrix(0,ntargs,length(desvars))
}
if(nrefs>=ntargs) {
# since there are more reference plots than target plots no clustering is necessary 
targs$cluster<-1:ntargs
cmns<-matrix(0,ntargs,length(desvars))
}
# k-means clustering
if(ctype=='km' & nrefs<ncands & nrefs<ntargs) {
km<-kmeans(bvars,centers=nrefs,iter.max=imax,nstart=nst)
targs$cluster<-km$cluster
cmns<-matrix(0,nrefs,length(desvars))
}
# Ward's D clustering
if(ctype=='WD' & nrefs<ncands & nrefs<ntargs) {
km<-hclust(dist(bvars),method='ward.D2')
targs$cluster<-cutree(km,k=nrefs)
cmns<-matrix(0,nrefs,length(desvars))
}
# assemble centroids into a data frame
cind<-sort(unique(targs$cluster))
for(j in 1:length(desvars))
cmns[cind,j]<-c(tapply(targs[,desvars[j]],targs$cluster,mean))
cmns<-data.frame(cmns)
centroids<-as.data.frame(cmns);names(centroids)<-desvars
# provide unique ids for centroids
Np<-max(as.numeric(as.character(popfile$PID)))
if(nrefs>=ntargs) centroids$PID<-(Np+1):(Np+ntargs)
if(nrefs<ntargs) centroids$PID<-(Np+1):(Np+nrefs)
centroids$size<-c(table(targs$cluster)) #sum(table(targs$cluster))
list(centroids=centroids,cmns=cmns)
}
