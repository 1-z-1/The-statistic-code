HA = matrix(c(50,92,15,90),2)
HB = matrix(c(47,5,135,60),2)
m = c(HA,HB)
x = array(m,c(2,2,2))#给与array不同的维度，（高维列联表）
mantelhaen.test(x)