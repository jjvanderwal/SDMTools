\name{read.asc}
\alias{image.asc}
\alias{print.asc}
\alias{read.asc}
\alias{read.asc.gz}
\alias{write.asc}
\alias{write.asc.gz}
\alias{write.asc2}
\alias{write.asc2.gz}
\title{ESRI ASCII Raster File Import And Export}
\usage{
read.asc(file, gz = FALSE)

read.asc.gz(file)

write.asc(x, file, gz = FALSE)

write.asc.gz(x, file)

write.asc2(x, file, sigdig = 0, gz = FALSE)

write.asc2.gz(x, file, sigdig = 0)

\method{image}{asc}(x, col = gray((240:1)/256), clfac = NULL, ...)

\method{print}{asc}(x, ...)
}
\arguments{
  \item{file}{a character string representing the filename
  of the input/output file. The file extension should
  always be '.asc'.}

  \item{gz}{defines if the object is or should be
  compressed using gzip}

  \item{x}{an object of class 'asc' as defined in the
  adehabitat package}

  \item{sigdig}{is the number of significant digits to
  write when creating the ascii grid file}

  \item{col}{for maps of type \code{"numeric"}, the colors
  to be used (see \code{help(par)})}

  \item{clfac}{for maps of type \code{"factor"}, a
  character vector giving the names of colors for each
  level of the factor (see \code{help(colasc)})}

  \item{\dots}{additional arguments to be passed to the
  generic function \code{image} or \code{print}}
}
\value{
Returns a raster matrix of the class 'asc' defined in the
adehabitat package with the following attributes:
\item{xll}{the x coordinate of the center of the lower left
pixel of the map} \item{yll}{the y coordinate of the center
of the lower left pixel of the map} \item{cellsize}{the
size of a pixel on the studied map} \item{type}{either
'numeric' or 'factor'} \item{levels}{if type = 'factor',
the levels of the factor.}
}
\description{
\code{read.asc} and \code{read.asc.gz} reads ESRI ArcInfo
ASCII raster file either uncompressed or compressed using
gzip. \cr \cr \code{write.asc} and \code{write.asc.gz}
writes an asc object to a ESRI ArcInfo ASCII raster file.
The output can be either compressed or uncompressed. \cr
\cr These functions are faster methods based on the
adehabitat import.asc and export.asc.\cr\cr
\code{write.asc2} and \code{write.asc2.gz} are even faster
implementations but have less error checking. \cr \cr
\code{image.asc} and \code{print.asc} are generic methods
associated with plotting & summarizing data of class 'asc';
they were modified from adehabitat package.
}
\details{
Implements a faster version of import.asc or export.asc
from the adehabitat package. In addition, files can be read
in and written to in gzip compressed format.\cr\cr Generic
methods of print and image were modified from adehabitat.
Further details of them are found there.
}
\examples{
#create a simple object of class 'asc'
tasc = as.asc(matrix(rep(x=1:10, times=1000),nr=100)); print(tasc)

#write out the raster grid file
write.asc(tasc,'t.raster.asc')
write.asc.gz(tasc,'t.raster.asc') #actually save file name as t.raster.asc.gz

#read in the raster grid files
tasc2 = read.asc('t.raster.asc')
tasc3 = read.asc.gz('t.raster.asc.gz')

#remove the temporary raster
unlink(c('t.raster.asc','t.raster.asc.gz'))
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}
}

