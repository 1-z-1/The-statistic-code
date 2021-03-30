library(xlsx)
m = read.xlsx('data gallary/第2章/nerve.xlsx',1)
X = c(m[,1],m[,2],m[,3],m[,4])
Median.nerve = median(X)
TBoot = NULL
n =20
B=1000
SD.nerve = NULL
for(i in 1:B)
{
  Xsample = sample(X,n,T)
  Tboot = median(Xsample)
  TBoot = c(TBoot,Tboot)
  SD.nerve = c(SD.nerve,sd(TBoot))
}
Sd.median.nerve = sd(TBoot)
plot(1:B,SD.nerve,col=4)
hist(TBoot,col=3)


# 这有三种置信区间
Lcl = Median.nerve+qnorm(0.025,0,1)*Sd.median.nerve
Ucl = Median.nerve-qnorm(0.025,0,1)*Sd.median.nerve
NORM.interval = c(Lcl,Ucl)

Lcl = 2*Median.nerve-quantile(TBoot,0.975)
Ucl = 2*Median.nerve-quantile(TBoot,0.025)
PIVOTAL.interval = c(Lcl,Ucl)

Lcl = quantile(TBoot,0.025)
Ucl = quantil(TBoot,0.975)
QUATILE.interval = c(Lcl,Ucl)