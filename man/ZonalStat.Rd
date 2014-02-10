\name{ZonalStat}
\alias{ZonalStat}
\title{Landscape Zonal Statistics}
\usage{
ZonalStat(mat, zones, FUN = "all")
}
\arguments{
  \item{mat}{a matrix of data to be summarized; The matrix
  can be a raster of class 'asc' (adehabitat package),
  'RasterLayer' (raster package) or 'SpatialGridDataFrame'
  (sp package)}

  \item{zones}{a matrix of data with individual patches
  identified as with \code{ConnCompLabel}; The matrix must
  be of the same size & extent as \code{mat}}

  \item{FUN}{a single or vector of functions to be applied
  to each 'zone'; the default of 'all' will calculate min,
  1st quarter, median, 3rd quarter, max, mean, standard
  deviation and n}
}
\value{
a data.frame listing \item{zone}{the unique ID for each
zone.} \item{functions...}{a column for each of the
functions identified}

The data.frame will have an atribute defining the number of
NA values that were excluded from the analysis.
}
\description{
\code{ZonalStat} calculates the statistics of data for
specified zones of two matrices of data. The matrix can be
a raster of class 'asc' (adehabitat package), 'RasterLayer'
(raster package) or 'SpatialGridDataFrame' (sp package).
}
\details{
The code summarizes the data for defined zones. Nearly any
function can be used for summarizing the data. \cr \cr The
FUN defined with 'all' as one of or the only function will
append the functions of min, 1st quarter, median, 3rd
quarter, max, mean, standard deviation and n to what is
being calculated.
}
\examples{
#define a simple binary matrix
tmat = { matrix(c( 0,0,0,1,0,0,1,1,0,1,
                   0,0,1,0,1,0,0,0,0,0,
                   0,1,NA,1,0,1,0,0,0,1,
                   1,0,1,1,1,0,1,0,0,1,
                   0,1,0,1,0,1,0,0,0,1,
                   0,0,1,0,1,0,0,1,1,0,
                   1,0,0,1,0,0,1,0,0,1,
                   0,1,0,0,0,1,0,0,0,1,
                   0,0,1,1,1,0,0,0,0,1,
                   1,1,1,0,0,0,0,0,0,1),nr=10,byrow=TRUE) }

#do the connected component labelling
ccl.mat = ConnCompLabel(tmat)
ccl.mat #this is the zone matrix to be used

#create a random data matrix
data.mat = matrix(runif(100),nr=10,nc=10)
data.mat

#calculate the zonal statistics
zs.data = ZonalStat(data.mat,ccl.mat,FUN='all')
zs.data

#just calculate the sum
zs.data = ZonalStat(data.mat,ccl.mat,FUN='sum')
zs.data

#calculate sum & n & 'all' and show when a function is not defined
zs.data = ZonalStat(data.mat,ccl.mat,
    FUN=c('sum','length','not.a.function','all'))
zs.data
attr(zs.data,'excluded NAs') #show how many NAs were omitted from analysis
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}
}

