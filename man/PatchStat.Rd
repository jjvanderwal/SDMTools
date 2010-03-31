\name{PatchStat}
\Rdversion{1.1}
\alias{PatchStat}

\title{ Landscape Patch Statistics }

\description{
\code{PatchStat} calculates the patch statistics for individual patches
identified in a matrix of data. The matrix can be a raster of class 'asc' 
(adehabitat package), 'RasterLayer' (raster package) or 'SpatialGridDataFrame' 
(sp package).
}

\usage{
PatchStat(mat,cellsize=1)
}
\arguments{
  \item{mat}{a matrix of data with individual patches identified as with \code{ConnCompLabel};
  The matrix can be a raster of class 'asc' (adehabitat package), 'RasterLayer' 
  (raster package) or 'SpatialGridDataFrame' (sp package)}
  \item{cellsize}{cell size (in meters) is a single value representing the width/height
  of cell edges (assuming square cells)}
}

\details{
The patch statistics are based on statistics calculated by fragstats 
\url{http://www.umass.edu/landeco/research/fragstats/fragstats.html}.
}

\value{
a data.frame listing
  \item{patchID}{the unique ID for each patch.}
  \item{n.cell}{the number of cells for each patch, specified in square meters.}
  \item{n.core.cell}{the number of cells in the core area, without the edge area}
  \item{n.edges.perimeter}{the number of outside edges perimeter of the patches (given in number of cell surfaces).}
  \item{n.edges.internal}{the number of internal edges perimeter of the patches}
  \item{area}{the area of each patch comprising a landscape mosaic.}
  \item{core.area}{represents the interior area of the patch, greater than the specified depth-of-edge distance from the perimeter.}
  \item{perimeter}{the perimeter of the patch, including any internal holes in the patch, specified in meters.}
  \item{perim.area.ratio}{the ratio of the patch perimeter (m) to area (m2).}
  \item{shape.index}{the shape complexity, sum of each patches perimeter divided by the square root of patch area.}
  \item{frac.dim.index}{fractal dimension index reflects shape complexity across a range of spatial scales; approaches 2 times the logarithm of patch perimeter (m) divided by the logarithm of patch area (m2).}
  \item{core.area.index}{quantifies core area as a percentage of patch area.}
}

\author{Jeremy VanDerWal \email{jjvanderwal@gmail.com}}

\examples{

#define a simple binary matrix
tmat = { matrix(c(	0,0,0,1,0,0,1,1,0,1,
					0,0,1,0,1,0,0,0,0,0,
					0,1,NA,1,0,1,0,0,0,1,
					1,0,1,1,1,0,1,0,0,1,
					0,1,0,1,0,1,0,0,0,1,
					0,0,1,0,1,0,0,1,1,0,
					1,0,0,1,0,0,1,0,0,1,
					0,1,0,0,0,1,0,0,0,1,
					0,0,1,1,1,0,0,0,0,1,
					1,1,1,0,0,0,0,0,0,1),nr=10,byrow=T) }
#do the connected component labelling
ccl.mat = ConnCompLabel(tmat)
ccl.mat
image(t(ccl.mat[10:1,]),col=c('grey',rainbow(length(unique(ccl.mat))-1)))
#calculate the patch statistics
ps.data = PatchStat(ccl.mat)
ps.data

}
