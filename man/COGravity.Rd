\name{COGravity}
\alias{COGravity}
\title{Centre of Gravity or Mass calculations for spatial data}
\usage{
COGravity(x, y = NULL, z = NULL, wt = NULL)
}
\arguments{
  \item{x}{a vector of e.g., longitudes or eastings, or a
  raster of class 'asc', 'RasterLayer' or
  'SpatialGridDataFrame'.}

  \item{y}{a vector of e.g., latitudes or northings.}

  \item{z}{a vector of e.g., elevations.}

  \item{wt}{a vector or raster of class 'asc',
  'RasterLayer' or 'SpatialGridDataFrame' representing
  weights for data.}
}
\value{
Returns a named vector of data representing the Centre of
Gravity in x, y & z dimensions (depending on data
supplied).
}
\description{
\code{COGravity} calculates the Centre of Gravity (or also
known as Centre of Mass) for point or raster spatial
data.\cr \cr \bold{Note:} NA data is automatically ommitted
from analysis.
}
\details{
For raster-based data, if \code{wt} is missing, the values
of the ascii are assumed to be the weights; otherwise, the
values are assumed to be the \code{z} values.
}
\examples{
#create some points
x = seq(154,110,length=25)
y = seq(-10,-54,length=25)
z = seq(100,200,length=25)
wt = runif(25) #random weights
#calculate the Centre of Gravity for these points
COGravity(x,y,z,wt)

#create a simple objects of class 'asc'
x = as.asc(matrix(1:50,nr=50,nc=50))
wt = as.asc(matrix(runif(50),nr=50,nc=50))

#calculate COG with weighting defined in x
COGravity(x)
#calculate COG with weighting defined in wt (values in x are assumed elevation (z))
COGravity(x,wt=wt)
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}
}

