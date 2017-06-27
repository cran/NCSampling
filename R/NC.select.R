NC.select <-
function(popfile,nrefs,desvars,centroids){
## select reference plots which are closest to centroids - single stratum only
## inputs population file, number of reference plots, design variables and cluster centroids
## outputs reference plots, pre-existing plots and target plots
exist<-popfile[popfile$plot_type=='E',]
targs<-popfile[popfile$plot_type=='B' | popfile$plot_type=='T',]
cands<-popfile[popfile$plot_type=='B' | popfile$plot_type=='C',]
nexist<-dim(exist)[1]
ntargs<-dim(targs)[1]
ncands<-dim(cands)[1]
row.names(cands)<-cands$PID
# nearest neighbour procedure with 12 neighbours
nn<-yai(x=cands[,desvars],method='euclidean',k=12)
# nearest neighbour set for centroid plots
row.names(centroids)<-centroids$PID
nntarg<-newtargets(nn,centroids) # the plots closest to the centroids
bestr<-as.data.frame(nntarg$neiIdsTrgs)
bestr$size<-centroids$size
for(j in 1:12)
bestr[,j]<-as.numeric(as.character(bestr[,j]))
bestr<-bestr[rev(order(bestr$size)),]
idk12<-c(as.matrix(bestr[,1:12]))
m12<-cands$PID[match(idk12,cands$PID)]
bestrPID<-unique(c(m12))[1:nrefs]
if(length(unique(c(m12)))<nrefs)
warning(paste('not enough suitable candidate plots in stratum',popfile$Strata[1],sep=' '))
# reference plots and target plots
ids<-match(bestrPID,cands$PID);ids<-ids[!is.na(ids)]
nid<-is.na(match(cands$PID,bestrPID)) * c(1:ncands);nid<-nid[nid>0]
if(length(ids)<nrefs) {
ids<-c(ids,nid)[1:nrefs]
nid<-c(ids,nid)[(nrefs+1):ncands]
}
refs<-cands[ids,];rem<-cands[nid,]
temp<-bestr[,1]
for(j in 2:12) temp<-c(temp,bestr[,j])
temp<-data.frame(temp,rep(bestr$size,12));names(temp)<-c('PID','size')
temp<-temp[!duplicated(temp$PID),]
refs<-merge(refs,temp)
list(refs=refs,exist=exist,targs=targs)
}
