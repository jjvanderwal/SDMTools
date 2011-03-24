slope <- 
function(mat,latlon=FALSE) {
	#check input for class for returning info
	if (any(class(mat) == 'asc')) { attrib = attributes(mat)
	} else if (any(class(mat) %in% 'RasterLayer')) { attrib = mat; mat = asc.from.raster(mat)
	} else if (any(class(mat) == 'SpatialGridDataFrame')) { attrib = mat; mat = asc.from.sp(mat)
	} else { attrib = attributes(mat) }
	if (!any(class(mat) == 'asc')) { stop('objects must be of class "asc"') } #check to ensure asc 
	
	# get the cell size information
	if (latlon) {
		tt = grid.info(getXYcoords(mat)$y,attr(mat,'cellsize')) #if latlon = true get the length & width of cells
		width = rowMeans(cbind(tt$top,tt$bottom))/1000; height = tt$side/1000 #get the width and height of the cells in km (NOT m)
	} else { width = height = rep(attr(mat,'cellsize'),length(getXYcoords(mat)$y)) } #get the cell width & height 
	
	slop = t(mat[,dim(mat)[2]:1])
	slop = .Call('slope',slop,width,height) #get the slope information
	mat[,] = t(slop[dim(slop)[1]:1,]) #move all slope info to mat
	
	#reset the attributes of the input
	if (any(class(attrib) %in% 'RasterLayer')) { attrib = setValues(attrib, as.vector(t(t(unclass(mat))[dim(mat)[2]:1,]))); return(attrib)
	} else if (any(class(attrib) == 'SpatialGridDataFrame')) { attrib@data[1] = as.vector(unclass(mat)[,dim(mat)[2]:1]); return(attrib)
	} else { attributes(mat) = attrib; return(mat) }
}
