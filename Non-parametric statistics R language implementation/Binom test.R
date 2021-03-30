# 与wilcox检验形成对比
spammail = c(310,350,370,377,389,400,415,425,440,295,
             325,296,250,340,298,365,375,360,385)
suc.num = sum(spammail>320)
n = length(spammail)
binom.test(suc.num,n,0.5)