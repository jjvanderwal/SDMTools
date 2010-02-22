#define the I stat function. I-statistic if from (Warren, Glor et al. 2008)
#ALL VALUES MUST BE POSITIVE
Istat = function(x,y){
  if(length(which(dim(x)==dim(y)))!=2) stop('asc objects must be of the same extent')#confirm same extents
  if (min(c(x,y),na.rm=T)<0) stop('all values must be positive')#confirm all values are positive
  pos = which(is.finite(x)) #positions in the data which have a value (are not NA)
  px = x[pos]/sum(x[pos]) #calculate the proportionate value relative to the sum of values across the raster
  py = y[pos]/sum(y[pos]) #calculate the proportionate value relative to the sum of values across the raster
  I = 1 - 0.5 * sqrt(sum((sqrt(px)-sqrt(py))^2)) #calculate the I-Statistic
  return(I)
}
