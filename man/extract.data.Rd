\name{extract.data}
\Rdversion{1.1}
\alias{extract.data}

\title{ Spatial Join of Points with Raster Grids }
\description{
\code{extract.data} extracts data from raster object of class 'asc' at 
specified locations. This represents a faster version of 'join.asc' of the 
adehabitat package that assumes all locations are within the map extents.

Note: there is no interpolation done here. The values reported are simply the 
values of the raster cell the point falls into.
}
\usage{
extract.data(pts,x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pts}{a two-column data frame with the x and y coordinates of the 
  locations of interest.}
  \item{x}{an object of class "asc" as defined in the adehabitat package}
}
\details{
Implements a faster version of 'join.asc' from the adehabitat package. 
\bold{NOTE:} this assumes all locations are within the extent of the raster map.
}
\value{
Returns a vector equal in length to the number of locations in pnts.
}
\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}}
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
