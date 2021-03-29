ridit.test = function(x)
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