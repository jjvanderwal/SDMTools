\name{asc.from.raster}
\alias{asc.from.raster}
\alias{raster.from.asc}
\alias{asc.from.sp}
\alias{sp.from.asc}
\alias{as.asc}

\title{ Raster conversion functions for adehabitat, raster and sp packages }

\description{
	\code{asc.from.raster} and \code{asc.from.sp} extracts data from objects of class 'RasterLayer' (raster package) and class 'SpatialGridDataFrame' (sp package) into an object of class 'asc' (SDMTools & adehabitat packages). \cr \cr
	\code{raster.from.asc} and \code{sp.from.asc} does the reverse.\cr\cr
	\code{as.asc} creates an object of class 'asc' (SDMTools & adehabitat packages) from a matrix of data. Code & helpfile associated with \code{as.asc} were modified from adehabitat package. 
}

\usage{
asc.from.raster(x)
raster.from.asc(x,projs=NA)
asc.from.sp(x)
sp.from.asc(x,projs=CRS(as.character(NA)))
as.asc(x, xll = 1, yll = 1, cellsize = 1, type = c("numeric", "factor"),
       lev = levels(factor(x)))
}

\arguments{
	\item{x}{is an object of class 'asc', 'RasterLayer' or 'SpatialGridDataFrame'. For the function \code{as.asc}, a matrix}
	\item{projs}{is a CRS projection string of the Proj4 package}
	\item{xll}{the x coordinate of the center of the lower left pixel of the map}
	\item{yll}{the y coordinate of the center of the lower left pixel of the map}
	\item{cellsize}{the size of a pixel on the studied map}
	\item{type}{a character string. Either \code{"numeric"} or \code{"factor"}}
	\item{lev}{if \code{type = "factor"}, either a vector giving the labels of the factor levels, or the name of a file giving the correspondence table of the map (see adehabitat as.asc helpfile details)}
}

\details{ These functions provide capabilities of using scripts / functions from many packages including adehabitat (plus e.g, SDMTools), sp (plus e.g., maptools, rgdal) and raster. }

\value{ Returns an object of class requested. }

\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }

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

#create a basic object of class asc
tasc = as.asc(matrix(rep(x=1:10, times=1000),nr=100)); print(tasc)

}
