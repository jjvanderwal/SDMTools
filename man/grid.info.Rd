\name{grid.info}
\alias{grid.info}
\title{ Grid Information from Geographic (lat lon) Projections }
\description{
Since spatial grids in geographic projections do not have equal area or perimeters, \code{grid.info} extracts perimeter & area related information for latitudinal bands with differing longitudinal widths. \cr\cr
Outputs lengths are in m using Vincenty's equation (\code{distance})and areas in m2. Surface areas are calculated summing surface areas of spherical polygons as estimated using l'Huiller's formula.
}
\usage{
grid.info(lats,cellsize,r=6378137)
}
\arguments{
  \item{lats}{is a vector of latitudes representing the midpoint of grid cells }
  \item{cellsize}{is a single value (assuming square cells) or a two value vector (rectangular cells) representing the height (latitude) and width (longitude) of the cells}
  \item{r}{is a single value representing the radius of the globe in m. Default is for the WGS84 elipsoid}
}
\value{ 
	a data.frame listing:
		\item{lat}{the latitude representing the midpoint of the cell}
		\item{top}{length of the top of the cell (m)}
		\item{bottom}{length of the bottom of the cell (m)}
		\item{side}{length of the side of the cell (m)}
		\item{diagnal}{length of the diagnals of the cell (m)}
		\item{area}{area of the cell (m2)}
		
}
\references{
information on l'Huiller's formula \url{http://williams.best.vwh.net/avform.htm for more info)}
code for estimating area of polygon on sphere was modified from \url{http://forum.worldwindcentral.com/showthread.php?t=20724}
}
\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }
\examples{
#show output for latitudes from -87.5 to 87.5 at 5 degree intervals
grid.info(lats=seq(-87.5,87.5,5), 5)
}