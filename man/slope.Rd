\name{slope}
\alias{aspect}
\alias{slope}
\title{Slope and aspect calculations}
\usage{
slope(mat, latlon = FALSE)

aspect(mat, latlon = FALSE)
}
\arguments{
  \item{mat}{a matrix of data representing z heights.
  Matrix can be a raster of class 'asc' (adehabitat
  package), 'RasterLayer' (raster package) or
  'SpatialGridDataFrame' (sp package)}

  \item{latlon}{boolean value representing if the data is
  geographic.}
}
\value{
an object of the same class as \code{mat}.
}
\description{
\code{slope} and \code{aspect} calculates the slope and
aspect of raster surfaces of class 'asc' (SDMTools &
adehabitat packages), 'RasterLayer' (raster package) or
'SpatialGridDataFrame' (sp package).\cr\cr Methods are
based on Burrough and McDonell (1998).
}
\details{
Slope returns values representing the 'rise over run' with
"run" units representing cellsize if \code{latlon}=FALSE or
km if \code{latlon}=TRUE. This can be changed to percentage
(multiply by 100) or to degrees by ATAN ( \code{output} ) *
57.29578.\cr\cr Aspect returns the direction (0 to 360)
with North being 0. Values of -1 are flat areas with no
slope or aspect.\cr\cr As this method requires information
from the surrounding cells, missing data (NAs or edges) are
populated with the value from the 'cell-of-interest').
}
\examples{
#define a simple asc with some slope and direction
tasc = as.asc(matrix(1:50,nr=10,nc=5),yll=75); tasc[,]
slope(tasc)[,] #show the output of slope
aspect(tasc)[,] #show the output of the aspect

#define a FLAT simple asc
tasc = as.asc(matrix(10,nr=10,nc=5),yll=75); tasc[,]
slope(tasc)[,] #show the output of slope
aspect(tasc)[,] #show the output of the aspect
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}
}
\references{
Burrough, P. A. and McDonell, R.A., 1998. Principles of
Geographical Information Systems (Oxford University Press,
New York), p. 190.
}

