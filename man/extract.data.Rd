\name{extract.data}
\alias{extract.data}

\title{ Spatial Join of Points with Raster Grids }

\description{
	\code{extract.data} extracts data from raster object of class 'asc' (this and the adehabitat package), 'RasterLayer' (raster package) or 'SpatialGridDataFrame' (sp package) at specified locations. This represents a faster version of 'join.asc' of the adehabitat package that assumes all locations are within the map extents. \cr \cr 
	\bold{Note:} there is no interpolation done here. The values reported are simply the values of the raster cell the point falls into.
}

\usage{ extract.data(pts,x) }

\arguments{
	\item{pts}{a two-column data frame or matrix with the x and y coordinates of the locations of interest.}
	\item{x}{a raster matrix of class 'asc' (this and the adehabitat package), 'RasterLayer' (raster package) or 'SpatialGridDataFrame' (sp package)}
}

\details{
	Implements a faster version of 'join.asc' from the adehabitat package. \cr \cr
	\bold{NOTE:} this assumes all locations are within the extent of the raster map. Values outside the extent will be given a value of NA.
}
\value{ Returns a vector equal in length to the number of locations in pnts. }

\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }

\examples{

#create a simple object of class 'asc'
tasc = as.asc(matrix(1:50,nr=50,nc=50)); print(tasc)

#define some point locations
points = data.frame(x=runif(25,1,50),y=runif(25,1,50))

#extract the data
points$values = extract.data(points,tasc)

#show the data
print(points)

}
