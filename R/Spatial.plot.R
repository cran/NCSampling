Spatial.plot <-
function(popfile,sampfile){
## x-y graph of candidate plots, target plots, existing plots and reference plots
## inputs population file and sample file
## produces graph showing spatial location of various plots by stratum
popfile$plot_type<-factor(popfile$plot_type,levels=c('B','C','E','R','T','X'))
sampfile$plot_type<-factor(sampfile$plot_type,levels=c('B','C','E','R','T','X'))
targs<-sampfile[sampfile$plot_type=='T',]
cands<-sampfile[sampfile$plot_type=='C',]
both<-sampfile[sampfile$plot_type=='B',]
bothp<-popfile[popfile$plot_type=='B',]
exist<-sampfile[sampfile$plot_type=='E',]
xtargs<-sampfile[sampfile$plot_type=='X',]
refs<-sampfile[sampfile$plot_type=='R',]
par(ask=T)
for(i in 1:length(levels(popfile$Strata))){
this.stratum<-levels(popfile$Strata)[i]
popi<-popfile[popfile$Strata==this.stratum,]
ptab<-table(popi$plot_type);ptab
sampi<-sampfile[sampfile$Strata==this.stratum,]
stab<-table(sampi$plot_type);stab
Nbi<-stab[1];Nci<-stab[2];nei<-stab[3];Ngi<-stab[5];Nxi<-stab[6];Nbpi<-ptab[1]
xl<-c(min(popi$x),max(popi$x))
if(Nbi==0 & nei>0) xl<-c(min(popi$x),1.3*max(popi$x)-0.3*min(popi$x))
if(Nbi>0 & (Nbi!=Nci | Nbi!=Ngi) & nei>0) xl<-c(min(popi$x),1.3*max(popi$x)-0.3*min(popi$x))
yl<-c(min(popi$y),1.1*max(popi$y)-0.1*min(popi$y))
if(Nbi==0 & nei>0)  yl<-c(min(popi$y),max(popi$y))
if(Nbi>0 & (Ngi!=Nci | Ngi!=Ngi) & nei>0) yl<-c(min(popi$y),max(popi$y))
# graph all plot types which are present
plot(popfile$x,popfile$y,type='n',xlab='X',ylab='Y',xlim=xl,ylim=yl)
with(both[both$Strata==this.stratum,],points(x,y,pch=16,col=8))
with(cands[cands$Strata==this.stratum,],points(x,y,pch=16,col=8))
with(refs[refs$Strata==this.stratum,],points(x,y,pch=16,col=3))
with(exist[exist$Strata==this.stratum,],points(x,y,pch=16,col=4))
with(targs[targs$Strata==this.stratum,],points(x,y,pch=16,col='coral'))
with(xtargs[xtargs$Strata==this.stratum,],points(x,y,pch=16,col=5))
if(Nbpi>0 & Nci>0) with(both[both$Strata==this.stratum,],points(x,y,pch=1))
title(paste('Stratum ',this.stratum,sep='- '))
# construct legend according to plot types which are present
ltx<-lpc<-lcl<-NULL
if(Nbi>0 & Nci==0) {ltx<-'cands/targs';lpc<-16;lcl<-8}
if(Nbi>0 & Nci>0) {ltx<-'cands/targs';lpc<-1;lcl<-1}
if(Nci>0) {ltx<-c(ltx,'cands');lpc<-c(lpc,16);lcl<-c(lcl,8)}
if(Ngi>0) {ltx<-c(ltx,'targs');lpc<-c(lpc,16);lcl<-c(lcl,'coral')}
if(Nxi>0) {ltx<-c(ltx,'exist neighbs');lpc<-c(lpc,16);lcl<-c(lcl,5)}
if(nei>0) {ltx<-c(ltx,'existing');lpc<-c(lpc,16);lcl<-c(lcl,4)}
{ltx<-c(ltx,'refs');lpc<-c(lpc,16);lcl<-c(lcl,3)}
if(length(ltx)<5) legend(horiz=T,'top',ltx,pch=lpc,col=lcl,bty='n')
if(length(ltx)>4 & Nbi==0) legend(horiz=T,'top',ltx,pch=lpc,col=lcl,bty='n')
if(length(ltx)>4 & Nbi>0) legend(horiz=F,'topright',ltx,pch=lpc,col=lcl,bty='n')
}
par(ask=F)
}
