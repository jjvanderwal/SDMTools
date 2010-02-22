extract.data <-
function(pts, x) {
  xy <- getXYcoords(x)
  xy$x <- xy$x + attr(x, "cellsize")/2
  xy$x <- c(xy$x[1] - attr(x, "cellsize"),xy$x)
  xy$y <- xy$y + attr(x, "cellsize")/2
  xy$y <- c(xy$y[1] - attr(x, "cellsize"),xy$y)
  xf <- as.numeric(cut(pts[, 1], xy$x))
  yf <- as.numeric(cut(pts[, 2], xy$y))
  return(x[cbind(xf,yf)])
}
#
#function(pts, x) {
#  xy <- getXYcoords(x)
#  xy$x <- xy$x + attr(x, "cellsize")/2
#  xy$x <- c(xy$x, xy$x[1] - attr(x, "cellsize")/2)
#  xy$y <- xy$y + attr(x, "cellsize")/2
#  xy$y <- c(xy$y, xy$y[1] - attr(x, "cellsize")/2)
#  xf <- as.numeric(cut(pts[, 1], xy$x))
#  yf <- as.numeric(cut(pts[, 2], xy$y))
#  return(x[cbind(xf,yf)])
#}
#
