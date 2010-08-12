#x is an ascii object
#latlong is the coordinate system, it can be in degrees when is TRUE or meters(East, North) when is FALSE

grid.area <-
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
	area = grid.info(getXYcoords(mat)$y,attr(mat,'cellsize'))$area
	mat[is.finite(mat)] = 1; for (ii in 1:length(area)) mat[,ii] = mat[,ii] * area[ii]
	#reset the attributes of the input
	if (any(class(attrib) %in% 'RasterLayer')) {
		attrib = setValues(attrib, as.vector(t(t(unclass(mat))[dim(mat)[2]:1,]))); return(attrib)
	} else if (any(class(attrib) == 'SpatialGridDataFrame')) {
		attrib@data[1] = as.vector(unclass(mat)[,dim(mat)[2]:1]); return(attrib)
	} else {
		attributes(mat) = attrib; return(mat)
	}
}
