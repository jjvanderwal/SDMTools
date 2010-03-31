extract.data <-
function(pts, x) {
	#check if raster from sp or raster package and convert if necessary
	if (any(class(x) %in% 'RasterLayer')) x = asc.from.raster(x)
	if (any(class(x) == 'SpatialGridDataFrame')) x = asc.from.sp(x)
	xy <- getXYcoords(x)
	xy$x <- xy$x + attr(x, "cellsize")/2
	xy$x <- c(xy$x[1] - attr(x, "cellsize"),xy$x)
	xy$y <- xy$y + attr(x, "cellsize")/2
	xy$y <- c(xy$y[1] - attr(x, "cellsize"),xy$y)
	xf <- as.numeric(cut(pts[, 1], xy$x))
	yf <- as.numeric(cut(pts[, 2], xy$y))
	return(x[cbind(xf,yf)])
}
