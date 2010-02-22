compare.matrix <-
function(x,y,nbins,...){
  #confirm same extents
  if(length(which(dim(x)==dim(y)))!=2) stop('asc objects must be of the same extent')
  #select only positions where data is finite
  pos=which(is.finite(x))
  #create a dataframe where each asc file is a column of finite data
  tdata=data.frame(x=x[pos], y=y[pos])
  #get the range of the data and add a small number to maximum so it will be contained within a bin
  tt = range(c(tdata$x,tdata$y),na.rm=T); tt[2] = tt[2] + 1e-7
  #create the bin classes
  bins = seq(tt[1],tt[2],(tt[2]-tt[1])/nbins)
  mids = bins[1:(length(bins)-1)] + 0.5 * mean(diff(bins))
  #reclass the data into bins
  for (i in 1:length(mids)){
    tdata$x[which(tdata$x>=bins[i] & tdata$x<bins[i+1])] = mids[i]
    tdata$y[which(tdata$y>=bins[i] & tdata$y<bins[i+1])] = mids[i]
  }
  #get frequencies (counts) of data combinations
  aggdata=aggregate(x=tdata$x, by=list(x=tdata$x, y=tdata$y), FUN=length)
  #create a data frame of all possible combinations of data within range
  tasc.p=expand.grid(x=mids, y=mids)  
  #merge tasc.p with aggdata
  mdata=merge(x=tasc.p, y=aggdata, all=TRUE)
  #convert to a matrix
  mdata <- matrix(mdata$x, nrow = length(mids), ncol=length(mids), byrow=TRUE)
  #create an image
  suppressWarnings(image(x=mids, y=mids, z=mdata, col=c(heat.colors(10)[10:1]),...))
  #overlay contours
  contour(x=mids, y=mids, z=mdata,  col="black", lty="solid", add=TRUE,...)
}

