BM.test = function(x,y,q,alt)
{
  xy = c(x,y)
  quantile.xy = quantile(xy,q)
  t = sum(xy>quantile.xy)
  lx = length(x[x!=quantile.xy])
  ly = length(y[y!=quantile.xy])
  lxy = lx+ly
  A = sum(x>quantile.xy)
  z = (A-lx*t)/(lx+ly)/(lx*ly*t*(lx+ly-t)/
                          (lx+ly)^3)^0.5
  
  if(A>min(lx,t)/2)
  {
    z1 = (A+0.5-lx*t)/(lx+ly)/
      (lx*ly*t*(lx+ly-t)/(lx+ly)^3)^0.5
  }
  else
  {
    z1 = (A-0.5-lx*t)/(lx+ly)/
      (lx*ly*t*(lx+ly-t)/(lx+ly)^3)^0.5
  }
  if(alt == 'greater')
  {
    pv1 = 1-phyper(A,lx,ly,t)
    pv2 = 1-pnorm(z)
    pv3 = 1-pnorm(z1)
  }
  if(alt == 'less')
  {
    pv1 = phyper(A,lx,ly,t)
    pv2 = pnorm(z)
    pv3 = pnorm(z1)
  }
  if(alt =='two.side')
  {
    pv1 = 2*min(1-phyper(A,lx,ly,t),
                phyper(A,lx,ly,t))
    pv2 = 2*min(1-pnorm(z),1-pnorm(z))
    pv3 = 2*min(pnorm(z1),pnorm(z1))
  }
  conting.table = matrix(c(A,lx-A,lx,t-A,ly-(t-A)
                           ,ly,t,lxy-t,lxy)
                         ,3,3)
  col.name = c('x','y','x+y')
  row.name = c('>Mxy','<Mxy','TOTAL')
  dimnames(conting.table) = list(row.name,col.name)
  list(contingency.table = conting.table,
       p.value = pv1,pvnorm = pv2,pvnorm = pv3)
}