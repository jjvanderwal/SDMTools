\name{put.data}
\alias{put.data}

\title{ Spatial Join of Points with Raster Grids - replace data }

\description{
	\code{put.data} replaces data in raster object of class 'asc' (this and adehabitat package) at specified locations.\cr \cr 
	\bold{Note:} there is no interpolation done here. The values given replace the values of the raster cell the point falls into.
}

\usage{ put.data(pts,x) }

\arguments{
	\item{pts}{a three-column data frame or matrix with the x and y coordinates of the locations of interest and the third column being the z values to put in the ascii grid file.}
	\item{x}{a raster matrix of class 'asc' (this and the adehabitat package)}
}

\details{
	Implements a faster version of 'join.asc' from the adehabitat package. \cr \cr
	\bold{NOTE:} this assumes all locations are within the extent of the raster map. Values outside the extent will be given a value of NA.
}
\value{ Returns a raster matrix  of class 'asc' equal in size to input 'x'. }

\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }

\examples{

#create a simple object of class 'asc'
tasc = as.asc(matrix(1:50,nr=50,nc=50)); print(tasc)
\dontrun{image(tasc)}

#define some point locations
points = data.frame(x=runif(25,1,50),y=runif(25,1,50),z=50)

#put the new data
tasc = put.data(points,tasc)

#show the data
\dontrun{image(tasc)}

}
