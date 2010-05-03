compare.matrix <-
function(x,y,nbins,...){
	#check if raster from sp or raster package and convert if necessary
	if (any(class(x) %in% 'RasterLayer')) { x = asc.from.raster(x); y = asc.from.raster(y) }
	if (any(class(x) == 'SpatialGridDataFrame')) { x = asc.from.sp(x); y = asc.from.sp(y) }
	#confirm same extents
	if(length(which(dim(x)==dim(y)))!=2) stop('matrix objects must be of the same extent')
	#select only positions where data is finite
	pos=which(is.finite(x))
	#create a dataframe where each asc file is a column of finite data
	tdata=data.frame(x=x[pos], y=y[pos])
	#get the range of the data and add a small number to maximum so it will be contained within a bin
	tt = range(c(tdata$x,tdata$y),na.rm=T); tt[2] = tt[2] + 1e-7
	#create the bin classes
	bins = seq(tt[1],tt[2],(tt[2]-tt[1])/nbins)
	mids = bins[1:(length(bins)-1)] + 0.5 * mean(diff(bins))
	tdata$xbin = cut(tdata$x,breaks=bins,labels=mids,include.lowest=T,right=T)
	tdata$ybin = cut(tdata$y,breaks=bins,labels=mids,include.lowest=T,right=T)
	#get frequencies (counts) of data combinations
	aggdata=aggregate(x=tdata$x, by=list(x=tdata$xbin, y=tdata$ybin), FUN=length)
	#create a data frame of all possible combinations of data within range
	tasc.p=expand.grid(x=mids, y=mids)  
	#merge tasc.p with aggdata
	mdata=merge(x=tasc.p, y=aggdata, all=TRUE)
	names(mdata)[3] = 'out'
	#convert to a matrix
	mdata <- matrix(mdata$out, nrow = length(mids), ncol=length(mids), byrow=TRUE)
	#create an image
	suppressWarnings(image(x=mids, y=mids, z=mdata, col=c(heat.colors(10)[10:1]),...))
	#overlay contours
	contour(x=mids, y=mids, z=mdata,  col="black", lty="solid", add=TRUE,...)
}

