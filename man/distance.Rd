\name{distance}
\alias{distance}

\title{ Vincenty Direct Calculation of Distance and Direction }

\description{
	\code{distance} estimates the distance given a starting & ending latitude and longitude. \cr \cr
	For general information on Vincenty's formula, see e.g., \url{http://en.wikipedia.org/wiki/Vincenty's_formulae}. It states: \cr
	\emph{Vincenty's formulae are two related iterative methods used in geodesy to calculate the distance between two points on the surface of an spheroid, developed by Thaddeus Vincenty in 1975. They are based on the assumption that the figure of the Earth is an oblate spheroid, and hence are more accurate than methods such as great-circle distance which assume a spherical Earth.} \cr \cr
	\bold{Note:} this method assumes a locations are lat & lon given in WGS 84.\cr\cr
	Direction, if requested, is the the initial bearing (sometimes referred to as forward azimuth) for which one would follow as a straight line along a great-circle arc from start to finish.\cr \cr
	\bold{Note:} this will fail if there are NA's in the data.
}

\usage{ distance(lat1, lon1=NULL, lat2=NULL, lon2=NULL, bearing=FALSE)}

\arguments{
	\item{lat1}{a single value or vector of values representing latitude in decimal degrees from -90 to 90 degrees. Alternatively, a data.frame or matrix can be used here with each column representing lat1, lon1, lat2, lon2 (in that order).}
	\item{lon1}{a single value or vector of values representing longitude in decimal degrees from -180 to 180 degrees. If NULL, lat1 is assumed to be a matrix or data.frame.}
	\item{lat2}{a single value or vector of values representing latitude in decimal degrees from -90 to 90 degrees. If NULL, lat1 is assumed to be a matrix or data.frame.}
	\item{lon2}{a single value or vector of values representing longitude in decimal degrees from -180 to 180 degrees. If NULL, lat1 is assumed to be a matrix or data.frame.}
	\item{bearing}{boolean value as to calculate the direction as well as the distance.}
}

\source{
	The source code for the distance algorithm here was modified from \url{http://www.movable-type.co.uk/scripts/latlong-vincenty.html}.\cr \cr
	Distances were validated against Geoscience Australia calculations (\url{http://www.ga.gov.au/geodesy/datums/vincenty_inverse.jsp}).\cr \cr
	Bearings were from multiple sources including \url{http://williams.best.vwh.net/avform.htm#Crs}.
}

\value{ 
	Returns a data.frame with:
		\item{lon1}{the original longitude}
		\item{lat1}{the original latitude}
		\item{lon2}{the destination longitude}
		\item{lat2}{the destination latitude}
		\item{distance}{the distance used}
		\item{bearing}{if requested, the bearing between the two points}
}

\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }

\references{ Vincenty, T. 1975. Direct and Inverse Solutions of Geodesics on the Ellipsoid with application of Nested Equations. Survey Review, vol XXII no 176. \url{http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf} }

\seealso{ \code{\link{destination}} }

\examples{

#get the distance of 1 degree longitude at each 5 degrees latitude from -90 to 90
distance(lat1=seq(-90,90,5),lon1=rep(0,37),lat2=seq(-90,90,5),lon2=rep(1,37),bearing=TRUE)
	
}
