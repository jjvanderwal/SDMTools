SigDiff = function(x,y,pattern=TRUE){
  if(length(which(dim(x)==dim(y)))!=2) stop('asc objects must be of the same extent')#confirm same extents
  pos = which(is.finite(x)) #positions in the data which have a value (are not NA)
  if(pattern){px = x[pos]/sum(x[pos])}else{px = x[pos]} #calculate the proportionate value relative to the sum of values across the raster
  if(pattern){py = y[pos]/sum(y[pos])}else{py = y[pos]} #calculate the proportionate value relative to the sum of values across the raster
  diff.xy = px-py
  diff.xy=scale(diff.xy) #create z-scores
  t.sig = pnorm(diff.xy) #get the significance values of the z-scores
  out = x; out[pos] = t.sig #create the output ascii grid and write significance values to it
  return(out)
}
