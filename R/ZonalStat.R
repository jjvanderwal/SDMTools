### zonal statistics
#mat is a matrix of data to be summarized
#zones is a matrix of data representing a raster of uniquely identified patches (from ConnCompLabel) or "ZONES"
#FUN is a vector of functions to be applied...

ZonalStat <- 
function(mat,zones,FUN='all')	{
	#check if functions are defined
	for (fun in FUN) { 
		if (!is.function(try(match.fun(fun),silent=TRUE))) { 
			FUN = FUN[-which(FUN==fun)]; warning(paste(fun,' is not a defined function!'))
		}
	}
	if (length(FUN)<1) stop('no known functions defined in request')

	#check if raster from sp or raster package and convert if necessary
	if (any(class(mat) %in% 'RasterLayer')) mat = asc.from.raster(mat)
	if (any(class(mat) == 'SpatialGridDataFrame')) mat = asc.from.sp(mat)
	if (any(class(zones) %in% 'RasterLayer')) zones = asc.from.raster(zones)
	if (any(class(zones) == 'SpatialGridDataFrame')) zones = asc.from.sp(zones)
	#check to ensure matrix
	mat = try(as.matrix(mat)); if (!is.matrix(mat)) stop('objects must be a matrix')
	zones = try(as.matrix(zones)); if (!is.matrix(zones)) stop('objects must be a matrix')
	if (!(dim(zones)[1]==dim(mat)[1] & dim(zones)[2]==dim(mat)[2])) stop('objects must be of the same extent / cellsize')
	#setup a matrix to summarize
	tt = data.frame(zones=as.vector(zones),data=as.vector(mat)); na.count = nrow(tt)
	tt = na.omit(tt); na.count = na.count-nrow(tt) #omit and count NA values
	tt = split(tt$data,tt$zones)
	
	#cycle through each of the functions and extract the data
	if (any(FUN=='all')) { FUN = c(FUN,'quantile','mean','sd','length'); FUN=FUN[-which(FUN=='all')]; FUN=unique(FUN) }
	
	#define the output
	out = data.frame(zones=names(tt))
	for (fun in FUN) { 
		if (fun == 'quantile') {
			out$min = out$qtr.25 = out$median = out$qtr.75 = out$max = NA
		} else { out[[fun]] = NA }		
	}
	
	#cycle through each of the groups in tt
	for (ii in 1:length(tt)) {
		#cycle through each of the functions
		for (fun in FUN) {
			if (fun=='quantile') {
				qq = quantile(tt[[ii]])
				out$min[ii] = qq[1]; out$qtr.25[ii] = qq[2]; out$median[ii] = qq[3]; out$qtr.75[ii] = qq[4]; out$max[ii] = qq[5]
			} else { out[ii,fun] = match.fun(fun)(tt[[ii]]) }
		}
	}
	#add an attribute defining how many NAs were removed
	attr(out,'excluded NAs') = na.count
	
	#return the data
	return(out)
}
