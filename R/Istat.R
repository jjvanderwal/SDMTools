#define the I stat function. I-statistic if from (Warren, Glor et al. 2008)
#ALL VALUES MUST BE POSITIVE
Istat = function(x,y){
	#check if raster from sp or raster package and convert if necessary
	if (any(class(x) %in% 'RasterLayer')) { x = asc.from.raster(x); y = asc.from.raster(y) }
	if (any(class(x) == 'SpatialGridDataFrame')) { x = asc.from.sp(x); y = asc.from.sp(y) }
	if(length(which(dim(x)==dim(y)))!=2) stop('matrix / raster objects must be of the same extent')#confirm same extents
	if (min(c(x,y),na.rm=T)<0) stop('all values must be positive')#confirm all values are positive
	pos = which(is.finite(x) & is.finite(y)) #positions in the data which have a value (are not NA)
	px = x[pos]/sum(x[pos]) #calculate the proportionate value relative to the sum of values across the raster
	py = y[pos]/sum(y[pos]) #calculate the proportionate value relative to the sum of values across the raster
	I = 1 - 0.5 * sqrt(sum((sqrt(px)-sqrt(py))^2)) #calculate the I-Statistic
	return(I)
}
