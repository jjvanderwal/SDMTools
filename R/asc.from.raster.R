#these functions add functionality of rasters to be read and written packages dependent on raster and sp package classes
asc.from.raster = function(x) {
	if (!any(class(x) %in% 'RasterLayer')) stop('x must be of class raster or RasterLayer')
	cellsize = (x@extent@ymax-x@extent@ymin)/x@nrows
	yll = x@extent@ymin + 0.5 * cellsize
	xll = x@extent@xmin + 0.5 * cellsize
	tmat = t(matrix(x@data@values,nr=x@nrows,ncol=x@ncols,byrow=T)[x@nrows:1,])
	tmat[which(tmat==x@file@nodatavalue)] = NA
	return(as.asc(tmat,yll=yll,xll=xll,cellsize=cellsize))
}
raster.from.asc = function(x,projs=NA) {
	if (class(x) != 'asc') stop('x must be of class asc')
	require(raster)
	cellsize = attr(x, "cellsize")
	nrows = dim(x)[2]; ncols= dim(x)[1]
	xmin = attr(x, "xll") - 0.5 * cellsize
	ymin = attr(x, "yll") - 0.5 * cellsize
	xmax = xmin + ncols*cellsize
	ymax = ymin + nrows*cellsize
	# if (packageDescription('raster',fields = "Version") > "1.0.4") {
		# r <- raster(ncols=ncols, nrows=nrows, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax, crs=projs)
	# } else {
		# r <- raster(ncols=ncols, nrows=nrows, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax, projs=projs)
	# }
	r <- raster(ncols=ncols, nrows=nrows, xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax)
	projection(r) <- projs
	tvals = as.vector(t(t(unclass(x))[nrows:1,])); tvals[which(is.na(tvals))] = r@file@nodatavalue
	r <- setValues(r, as.vector(t(t(unclass(x))[nrows:1,])))
	return(r)
}
asc.from.sp = function(x) {
	#assumes single band data
	if (!any(class(x) == 'SpatialGridDataFrame')) stop('x must be of class SpatialGridDataFrame')
	cellsize = mean(x@grid@cellsize)
	yll = as.numeric(x@grid@cellcentre.offset[2])
	xll = as.numeric(x@grid@cellcentre.offset[1])
	names(x@data)[1] = 'z'
	tmat = t(matrix(x@data$z,nr=x@grid@cells.dim[2],ncol=x@grid@cells.dim[1],byrow=T)[x@grid@cells.dim[2]:1,])
	return(as.asc(tmat,yll=yll,xll=xll,cellsize=cellsize))
}
sp.from.asc = function(x,projs=CRS(as.character(NA))) {
	if (!inherits(x, "asc")) stop('x must be of class asc')
	require(sp)
	tgrid = GridTopology(c(attr(x, "xll"),attr(x, "yll")),rep(attr(x, "cellsize"),2),dim(x))
	return(SpatialGridDataFrame(tgrid,data.frame(z=as.vector(unclass(x)[,dim(x)[2]:1])),proj4string=projs))
}
as.asc = function(x, xll=1, yll=1, cellsize=1,type=c("numeric", "factor"),lev=levels(factor(x))) {
    #check inputs
    type=match.arg(type)
    if (!inherits(x, "matrix")) stop("x should be a matrix")

    # creates the attributes
    mode(x) = "numeric"; attr(x, "xll") = xll; attr(x, "yll") = yll
    attr(x, "cellsize")=cellsize; attr(x, "type") = type
    if (type=="factor") attr(x, "levels") = lev
    class(x) = "asc"

    #return the object
    return(x)
}
