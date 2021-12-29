R_CI <- function(r=0.36, n=50, rep=20000, conf=95){

z=.5*log((1+r)/(1-r))
se=1/sqrt(n-3)
zvec=rnorm(rep)*se+z
ivec=(exp(2*zvec)-1)/(exp(2*zvec)+1)
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ivec,low)
UL=quantile(ivec,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)
LE=(exp(2*(.5*log((1+r)/(1-r))+se*qnorm(low,0,1)))-1)/(exp(2*(.5*log((1+r)/(1-r))+se*qnorm(low,0,1)))+1)
UE=(exp(2*(.5*log((1+r)/(1-r))-se*qnorm(low,0,1)))-1)/(exp(2*(.5*log((1+r)/(1-r))-se*qnorm(low,0,1)))+1)
LE4=format(LE,digits=4)
UE4=format(UE,digits=4)

hist(ivec,
     breaks='FD',
     col='yellow',
     xlab=paste(conf,'% Monte Carlo CI ','LL',LL4,'  UL',UL4,'\n',conf,'% Exact CI ','LL',LE4,'  UL',UE4),
     main='Monte Carlo Distribution of r')
}
