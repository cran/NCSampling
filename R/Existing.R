Existing <-
function(popfile,nrefs,desvars,draw.plot){
## associate target plots with pre-existing reference plots 
## inputs population file, sample size, design variables and plot preference (T/F)
## outputs number of neighbours to pre-existing plots, number of target plots and population file with pre-existing plot neighbours identified
# obtain target plots, existing plots and stratum sizes 
targs<-popfile[popfile$plot_type=='B' | popfile$plot_type=='T',]
exist<-popfile[popfile$plot_type=='E',]
Ns<-table(targs$Strata)
ns<-nrefs
ne<-table(exist$Strata)
strata.levels<-levels(targs$Strata)
targs<-targs[order(targs$Strata),]
xtargs<-targs[order(targs$Strata),c('PID','plot_type','Strata')]
xtargs$exist<-0
## for each stratum where there is a pre-existing plot
plot.sum<-NULL
for(i in 1:length(ne)){
if(Ns[i]>0 & ns[i]<=ne[i]) stop('sample size <= number of existing plots in some strata')
if(Ns[i]>0 & ns[i]>ne[i]){
this.stratum<-strata.levels[i]
desvars_i<-desvars
if(is.matrix(desvars)) desvars_i<-desvars[i,]
desvars_i<-desvars_i[desvars_i!=''];desvars_i<-desvars_i[desvars_i!=' ']
targs_i<-targs[targs$Strata==this.stratum,]
exist_i<-exist[exist$Strata==this.stratum,]
pop_i<-popfile[popfile$Strata==this.stratum,]
## how many plots, on average, will become near neighbours to existing plots
tot.exist<-numeric(100)
for(j in 1:100){
temp<-NC.sample(pop_i,nrefs[i]-ne[i],desvars,'km',20,1)
refs_i<-temp$popfile[temp$popfile$plot_type=='E' | temp$popfile$plot_type=='R',]
row.names(refs_i)<-refs_i$PID;row.names(targs_i)<-targs_i$PID
targs_j<-targs_i[is.na(match(targs_i$PID,refs_i$PID)),]
nn<-yai(x=refs_i[,desvars_i],y=NULL,method='euclidean',k=1)
nntarg<-newtargets(nn,targs_j) 
nei<-nntarg$neiIdsTrgs
nexi<-nei[!is.na(match(nei,exist_i$PID))]
tot.exist[j]<-length(unique(nexi))
}
Nexi<-mean(tot.exist) 
## scaled design variables for target plots and existing plots based on target plots
tvars<-scale(targs_i[,desvars_i])
sc<-apply(targs_i[,desvars_i],2,mean)
ss<-apply(targs_i[,desvars_i],2,sd)
evars<-scale(exist_i[,desvars_i],center=sc,scale=ss)
## distance matrix, existing plots to target plots
dM<-as.matrix(dist(rbind(tvars,evars)),upper=T)
dMet<-dM[1:ne[i],(ne[i]+1):(ne[i]+Ns[i])]
md<-apply(dMet,2,min) 
mid<-order(md)[1:round(Nexi)]
etargs<-targs_i[mid,]
xtargs[match(etargs$PID,xtargs$PID),'exist']<-1
row.names(exist_i)<-exist_i$PID
nn<-yai(x=exist_i[,desvars_i],y=NULL,method='euclidean',k=1)
nntarg<-newtargets(nn,etargs) 
nei<-nntarg$neiIdsTrgs
nxi<-table(nei)
exist_i$No_neighbours<-0
exist_i$No_neighbours[match(row.names(nxi),exist_i$PID)]<-nxi
temps<-exist_i[,c('PID','Strata','No_neighbours')]
if(!is.null(plot.sum)) plot.sum<-rbind(plot.sum,temps)
if(is.null(plot.sum)) plot.sum<-temps
}
}
plot.sum$Plot.id<-1:length(plot.sum$PID)
temp<-factor(targs$plot_type,levels=c('B','T','X'))
targs$plot_type<-replace(temp,xtargs$exist==1,'X')
if(draw.plot==T) {
par(ask=T)
for(i in 1:length(ne)){
temp<-plot.sum[plot.sum$Strata==levels(plot.sum$Strata)[i],]
if(dim(temp)[1]>0){
with(temp,barplot(No_neighbours,names.arg=Plot.id, xlab='Plot number', 
ylab='Number of neighbours', main=paste('Stratum',levels(plot.sum$Strata)[i],sep=' - ')))
}
}
par(ask=F)
}
tab<-table(targs$plot_type,targs$Strata)
Ng<-tab[row.names(tab)=='B',]+tab[row.names(tab)=='T',]
Nx<-tab[row.names(tab)=='X',]
popfile[match(targs$PID,popfile$PID),'plot_type']<-targs$plot_type
list(Nx=Nx,Ng=Ng,popfile=popfile)
}
