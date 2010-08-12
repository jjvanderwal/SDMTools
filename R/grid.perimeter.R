grid.perimeter <-
function(mat)	{
	#check input for class for returning info
	if (any(class(mat) == 'asc')) { 
		attrib = attributes(mat)
	} else if (any(class(mat) %in% 'RasterLayer')) {
		attrib = mat; mat = asc.from.raster(mat)
	} else if (any(class(mat) == 'SpatialGridDataFrame')) {
		attrib = mat; mat = asc.from.sp(mat)
	} else {
		attrib = attributes(mat)
	}
	#check to ensure asc 
	if (!any(class(mat) == 'asc')) { stop('objects must be of class "asc"') }
	#apply the gridinfo 
	perim = grid.info(getXYcoords(mat)$y,attr(mat,'cellsize'))
	perim = perim$top+perim$bottom+2*perim$side
	mat[is.finite(mat)] = 1; for (ii in 1:length(perim)) mat[,ii] = mat[,ii] * perim[ii]
	#reset the attributes of the input
	if (any(class(attrib) %in% 'RasterLayer')) {
		attrib = setValues(attrib, as.vector(t(t(unclass(mat))[dim(mat)[2]:1,]))); return(attrib)
	} else if (any(class(attrib) == 'SpatialGridDataFrame')) {
		attrib@data[1] = as.vector(unclass(mat)[,dim(mat)[2]:1]); return(attrib)
	} else {
		attributes(mat) = attrib; return(mat)
	}
}
