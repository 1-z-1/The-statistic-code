library(foreign)
camera = read.spss('data gallery/camera-chisq2.sav')
attach(camera)
ridit_outlook = rbind(table(c01),table(c02),table(c03),table(c04),table(c05))
ridit_outlook
ridit_fun = rbind(table(c11),table(c12),table(c13),table(c14),table(c15),table(c16))
ridit_fun

ridit.test = function(x)# 这个x应该是一个矩阵列之间应该有强弱关系
{
  order.num = ncol(x) # 取列数
  treat.num = nrow(x) # 取行数
  rowsum = rowSums(x) # 求行和
  colsum = colSums(x) # 求列和
  total = sum(rowsum) # 求和
  N = (colsum/2)[1:order.num]+c(0,(cumsum(colsum))[1:order.num-1])
  ri = N/total
  p_coni = x/outer(rowsum,rep(1,order.num),"*")
  pi. = rowsum/total
  score = p_coni %*% ri
  confi_inter = matrix(c(score-1/sqrt(3*rowsum),score+1/sqrt(3*rowsum)),
                       byrow = F, ncol = 2)
  if(length(rle(sort(ri))$lengths) == length(ri))
  {
    w = (12*total/(total+1))*sum(rowsum*(score-0.5)^2)
  }
  if(length(rle(sort(ri))$lengths) <= length(ri))
  {
    tao = rle(sort(ri))$lengths
    T = 1-sum(tao^3-tao)/(order.num^3-order.num)
    w = (12*total/((total+1)*T))*sum(rowsum*(score-0.5)^2)
  }
  pvalue = pchisq(w,treat.num-1,lower.tail = F)
  list(score,confi_inter = confi_inter,W=w,Pvalue = pvalue)
}

options(digits = 4)
res_outlook = ridit.test(ridit_outlook)
graph_outlook = res_outlook$confi_inter
plot(0,0,ylim=c(0,1),xlim=c(1,5),xlab="outlook",ylab="",
     main = "Ridit value confidence interval",col="gray7")
abline(h=0.5)
for(i in 1:nrow(graph_outlook))
  lines(c(i,i),graph_outlook[i,],lwd=2)
res_fun = ridit.test(ridit_fun)
graph_fun = res_fun$confi_inter
plot(0,0,ylim = c(0,1),xlim = c(1,6),xlab="function",ylab ="",
     main = "Ridit value cofidenceinterval",col = "gray7")
abline(h=0.5)
for(i in 1:nrow(graph_fun))
  lines(c(i,i),graph_fun[i,],lwd=2)
