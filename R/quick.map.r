# CREATE A FUNCTION FOR MAP COLORS

quick.map= function(sdm.asc, threshold, bkgd.col = 'grey',cols=heat.colors(100), zlim=NULL, pnts=NULL, ...){
	#check if raster from sp or raster package and convert if necessary
	if (any(class(sdm.asc) %in% 'RasterLayer')) sdm.asc = asc.from.raster(sdm.asc)
	if (any(class(sdm.asc) == 'SpatialGridDataFrame')) sdm.asc = asc.from.sp(sdm.asc)

	#get range of data
	trange=range(sdm.asc,na.rm=T)
	#make sure threshold is within range
	if (!(trange[1]< threshold & trange[2]>threshold)) stop('Cannot be 0')
	#rework sdm.asc
	sdm.asc[which(!is.na(sdm.asc) & sdm.asc<=threshold)] = 0
	tvals = seq(threshold,trange[2],(trange[2]-threshold)/(length(cols)+2))
	for (i in 1:length(cols)) sdm.asc[which(!is.na(sdm.asc) & sdm.asc<tvals[i] & sdm.asc>=tvals[i+1])] = 0
	#create the image
	if (is.null(zlim)) { lim = range(sdm.asc,na.rm=T) } else { lim = zlim }
	image(sdm.asc, col=c(bkgd.col,cols), xlab="", ylab="",zlim=lim,...)
	#add the scale legend
	if (!is.null(pnts)) legend.gradient(pnts,cols=c(bkgd.col,cols),limits=lim)
}
 
