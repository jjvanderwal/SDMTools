\name{asc.from.raster}
\Rdversion{1.1}
\alias{asc.from.raster}
\alias{raster.from.asc}
\alias{asc.from.sp}
\alias{sp.from.asc}

\title{ Raster conversion functions for adehabitat, raster and sp packages }
\description{
\code{asc.from.raster} and \code{asc.from.sp} extracts data from objects of 
class 'RasterLayer' (raster package) and class 'SpatialGridDataFrame' (sp package)
into an object of class 'asc' (adehabitat package). \code{raster.from.asc}
and \code{sp.from.asc} does the reverse.
}
\usage{
asc.from.raster(x)
raster.from.asc(x,projs=NA)
asc.from.sp(x)
sp.from.asc(x,projs=CRS(as.character(NA)))
}

\arguments{
  \item{x}{is an object of class 'asc', 'RasterLayer' or 'SpatialGridDataFrame'}
  \item{projs}{is a CRS projection string of the Proj4 package}
}
\details{
These functions provide capabilities of using scripts / functions of many packages
including adehabitat (plus e.g, SDMTools), sp (plus e.g., maptools, rgdal) and 
raster.
}
\value{
Returns an object of class requested.
}
\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}}
\examples{

#create a simple object of class 'asc'
tasc = as.asc(matrix(rep(x=1:10, times=1000),nr=100)); print(tasc)
str(tasc)

#convert to RasterLayer
traster = raster.from.asc(tasc)
str(traster)

#convert to SpatialGridDataFrame
tgrid = sp.from.asc(tasc)
str(tgrid)

}