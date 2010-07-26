put.data <-
function(pts, x) {
	if (class(x) != 'asc') stop('matrix must be of class "asc"') #check to ensure x is of class asc
	pts = as.matrix(pts); if (dim(pts)[2]!=3) stop('input pts must be 3 columns of data')
	xy <- getXYcoords(x)
	xy$x <- xy$x + attr(x, "cellsize")/2
	xy$x <- c(xy$x[1] - attr(x, "cellsize"),xy$x)
	xy$y <- xy$y + attr(x, "cellsize")/2
	xy$y <- c(xy$y[1] - attr(x, "cellsize"),xy$y)
	xf <- as.numeric(cut(pts[, 1], xy$x))
	yf <- as.numeric(cut(pts[, 2], xy$y))
	x[cbind(xf,yf)] = pts[,3]
	return(x)
}
