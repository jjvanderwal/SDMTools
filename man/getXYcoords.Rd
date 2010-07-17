\name{getXYcoords}
\alias{getXYcoords}
\title{Computes the X and Y Coordinates of the Pixels of a Raster Map}
\description{
  \code{getXYcoords} computes the geographical coordinates of the rows and columns of pixels of a raster map of class \code{asc}. Code & helpfile were modified from adehabitat package.
}
\usage{
getXYcoords(w)
}
\arguments{
  \item{w}{an object of class \code{asc}.}
}
\value{
  Returns a list with two components:
  \item{x}{the x coordinates of the columns of pixels of the map}
  \item{y}{the y coordinates of the rows of pixels of the map}
}

\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}}

\examples{

tasc = as.asc(matrix(rep(x=1:10, times=1000),nr=100)); print(tasc)
getXYcoords(tasc)

}



