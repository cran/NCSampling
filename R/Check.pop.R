Check.pop <-
function(popfile,desvars){
## check validity of population file
## inputs population file and names of design variables
## produces barchart of population structure
if(is.matrix(desvars)) if(dim(desvars)[1]!=length(levels(popfile$Strata))) stop('number of rows in design variable matrix not equal to number of strata')
vdesvars<-c(desvars);vdesvars<-vdesvars[vdesvars!=''];vdesvars<-vdesvars[vdesvars!=' ']
if(any(is.na(match(vdesvars,names(popfile))))) stop('population file must contain all design variables')
if(is.na(match('Strata',names(popfile)))) stop('population file must contain Strata')
if(!is.factor(popfile$Strata)) stop('Strata must be a factor variable')
if(is.na(match('plot_type',names(popfile)))) stop('population file must contain plot type')
if(is.na(match('PID',names(popfile)))) stop('population file must contain PID (plot ID)')
if(any(is.na(match(c('x','y'),names(popfile))))) warning('population file is missing x and/or y variables')
if(any(duplicated(popfile$PID))) stop('duplicated PID found in population file')
xy<-paste(popfile$x,popfile$y,sep='-')
if(any(duplicated(xy))) stop('duplicated x-y cordinates found in population file')
isna<-is.na(popfile$Strata)
if(any(isna==T)) stop('missing data found in Strata variable')
isna<-is.na(popfile$PID)
if(any(isna==T)) stop('missing data found in plot id (PID) variable')
isna<-is.na(popfile$plot_type)
if(any(isna==T)) stop('missing data found in plot type variable')
isnum<-apply(popfile[,vdesvars],2,is.numeric)
if(any(isnum==F)) stop('design variables must be numeric')
isna<-apply(popfile[,vdesvars],2,is.na)
if(any(c(isna)==T)) stop('missing data found in design variables')
levm<-match(levels(popfile$plot_type),c('B','C','E', 'T','X'))
if(any(is.na(levm))) stop('plot type must be one of B, C, E, T or X')
# separate popfile into plot types
exist<-popfile[popfile$plot_type=='E',]
targs<-popfile[popfile$plot_type=='B' | popfile$plot_type=='T',]
cands<-popfile[popfile$plot_type=='B' | popfile$plot_type=='C',]
both<-popfile[popfile$plot_type=='B',]
Ns<-table(popfile$Strata)
Ne<-table(exist$Strata)
Nc<-table(cands$Strata)
Nb<-table(both$Strata)
Ng<-table(targs$Strata)
if(any(Ng==0)) warning('some strata have no target plots')
if(any(Nc==0)) warning('some strata have no candidate plots')
# trellis type barplot of population structure
td1<-data.frame(levels(popfile$Strata),c(Ns));names(td1)<-c('Strata','N')
td1$Plot_type<-'Total'
td2<-data.frame(levels(popfile$Strata),c(Nc));names(td2)<-c('Strata','N')
td2$Plot_type<-'Cands'
td3<-data.frame(levels(popfile$Strata),c(Ng));names(td3)<-c('Strata','N')
td3$Plot_type<-'Targs'
td4<-data.frame(levels(popfile$Strata),c(Nb));names(td4)<-c('Strata','N')
td4$Plot_type<-'Cands/Targs'
td5<-data.frame(levels(popfile$Strata),c(Ne));names(td5)<-c('Strata','N')
td5$Plot_type<-'Exist'
tdt<-rbind(td1,td2,td3,td4,td5)
tdt$Plot_type<-factor(tdt$Plot_type,levels=c('Exist','Cands','Targs','Cands/Targs','Total'))
tdt$Plot_type<-factor(tdt$Plot_type)
no.b<-F
if(tapply(tdt$N,tdt$Plot_type,mean)[4]==0) no.b<-T
if(no.b) tdt<-rbind(td1,td2,td3,td5)
if(no.b) tdt$Plot_type<-factor(tdt$Plot_type,levels=c('Exist','Cands','Targs','Total'))
bonly<-any(popfile$plot_type=='B' & popfile$plot_type!='T') & any(popfile$plot_type=='B' & popfile$plot_type!='C')
if(bonly) tdt<-rbind(td1,td4,td5)
if(bonly) tdt$Plot_type<-factor(tdt$Plot_type,levels=c('Exist','Cands/Targs','Total'))
barchart(N~Plot_type | Strata, data=tdt,ylab='No. of plots',xlab='Plot type',main='Population structure', 
strip=strip.custom(strip.names=c(T,T)),ylim=c(0,1.05*max(tdt$N)))
}
