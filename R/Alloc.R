Alloc <-
function(popfile,ntotal){
## allocate sample among several strata, using proportional allocation
## inputs population file and total sample size
## outputs sample sizes for each stratum
targs<-popfile[popfile$plot_type=='B' | popfile$plot_type=='T',]
cands<-popfile[popfile$plot_type=='B' | popfile$plot_type=='C',]
Ns<-table(targs$Strata)
Nc<-table(cands$Strata)
ns<-round(ntotal*Ns/sum(Ns))
if(any(ns>Nc)) warning('Sample size reduced to number of candidate plots in one or more strata')
nsf<-pmin(ns,Nc)
if(any(ns>Nc)) {print('Initial sample size');print(ns)}
if(any(ns>Nc)) {print('Final sample size');print(nsf)}
nsf
}
