ImageDiff = function(tasc,sig.levels=c(0.025,0.975),tcol=terrain.colors(3),...){
	#check if raster from sp or raster package and convert if necessary
	if (any(class(tasc) %in% 'RasterLayer')) tasc = asc.from.raster(tasc)
	if (any(class(tasc) == 'SpatialGridDataFrame')) tasc = asc.from.sp(tasc)
	tasc[which(is.finite(tasc) & tasc<=sig.levels[1])] = 9
	tasc[which(is.finite(tasc) & tasc>sig.levels[1] & tasc<sig.levels[2])] = 10
	tasc[which(is.finite(tasc) & tasc<=1)] = 11
	image(tasc,col=tcol,zlim=c(9,11),...)
}
