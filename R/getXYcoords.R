"getXYcoords" <- function(w)
{
	#check if raster from sp or raster package and convert if necessary
	if (any(class(w) %in% 'RasterLayer')) w = asc.from.raster(w)
	if (any(class(w) == 'SpatialGridDataFrame')) w = asc.from.sp(w)
    if (!inherits(w, "asc")) stop("must be of class asc")

    # Gets the attributes
    cs<-attr(w, "cellsize")
    xll<-attr(w, "xll")
    yll<-attr(w, "yll")

    ## Computation of the number of rows and columns of the matrix
    nr<-nrow(w)
    nc<-ncol(w)

    ## The results
    x<-xll+c(0:(nr-1))*cs
    y<-yll+c(0:(nc-1))*cs
    return(list(x=x, y=y))
}

