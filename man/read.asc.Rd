\name{read.asc}
\Rdversion{1.1}
\alias{read.asc}
\alias{read.asc.gz}
\alias{write.asc}
\alias{write.asc.gz}

\title{ESRI ASCII Raster File Import And Export}
\description{\code{read.asc} and \code{read.asc.gz} reads ESRI ArcInfo ASCII raster file 
either uncompressed or compressed using gzip.

\code{write.asc} and \code{write.asc.gz} writes an asc object to a ESRI 
ArcInfo ASCII raster file. The output can be either compressed or uncompressed.

These functions are faster methods based on the adehabitat import.asc and 
export.asc.
}
\usage{
read.asc(file)
read.asc.gz(file)
write.asc(x,file)
write.asc.gz(x,file)
}

\arguments{
  \item{file}{a character string representing the filename of the input/output 
file. The file extension should always be '.asc'.}
  \item{x}{an object of class 'asc' as defined in the adehabitat package}
}
\details{
Implements a faster version of import.asc or export.asc from the adehabitat 
package. In addition, files can be read in and written to in gzip compressed format.
}
\value{
Returns a raster matrix of the class 'asc' defined in the adehabitat package 
with the following attributes:
  \item{xll}{the x coordinate of the center of the lower left pixel of the map}
  \item{yll}{the y coordinate of the center of the lower left pixel of the map}
  \item{cellsize}{the size of a pixel on the studied map}
  \item{type}{either 'numeric' or 'factor'}
  \item{levels}{if type = 'factor', the levels of the factor.}
}
\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}}
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
