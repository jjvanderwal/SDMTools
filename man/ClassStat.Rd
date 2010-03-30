\name{ClassStat}
\Rdversion{1.1}
\alias{ClassStat}

\title{ Landscape Class Statistics }

\description{
\code{ClassStat} calculates the class statistics for patch types
identified in a matrix of data.  
}

\usage{
ClassStat(mat,cellsize=1,bkgd=NA)
}
\arguments{
  \item{mat}{a matrix of data with individual patches identified as with
  \code{ConnCompLabel}}
  \item{cellsize}{cell size (in meters) is a single value representing the width/height
  of cell edges (assuming square cells)}
  \item{bkgd}{the background value for which statistics will not be calculated}
}

\details{
The class statistics are based on statistics calculated by fragstats 
\url{http://www.umass.edu/landeco/research/fragstats/fragstats.html}.
}

\value{
a data.frame listing
  \item{class}{measure of landscape composition, how much of the landscape is comprised of a particular patch type.}
  \item{n.patches}{the number of patches of a particular patch type or in a class.}
  \item{total.area}{the sum of the areas (m2) of all patches of the corresponding patch type.}
  \item{prop.landscape}{...}
  \item{patch.density}{the numbers of patches of the corresponding patch type divided by total landscape area (m2).}
  \item{total.edge}{the total edge length of a particular patch type.}
  \item{edge.density}{edge length on a per unit area basis that facilitates comparison among landscapes of varying size.}
  \item{landscape.shape.index}{a standardized measure of total edge or edge density that adjusts for the size of the landscape.}
  \item{largest.patch.index}{Largest patch index quantifies the percentage of total landscape area comprised by the largest patch.}
  \item{mean.patch.area}{average area of patches.}
  \item{sd.patch.area}{Standard deviation of patch areas.}
  \item{min.patch.area}{the minimum patch area of the total patch areas. }
  \item{max.patch.area}{the maximum patch area of the total patch areas.}
  \item{perimeter.area.frac.dim}{perimeter-area fractal dimension equals 2 divided by the slope of regression line obtained by regressing the logarithm of patch area (m2) against the logarithm of patch perimeter (m).}
  \item{mean.perim.area.ratio}{the mean of the ratio patch perimeter. The perimeter-area ratio equals to the ratio of the patch perimeter (m) to area (m2).}
  \item{sd.perim.area.ratio}{standard deviation of the ratio patch perimeter.}
  \item{min.perim.area.ratio}{Minimum perimeter area ratio}
  \item{max.perim.area.ratio}{Maximum perimeter area ratio.}
  \item{mean.shape.index}{ mean of shape index}
  \item{sd.shape.index}{standard deviation of shape index.}
  \item{min.shape.index}{the minimum shape index.}
  \item{max.shape.index}{...}
  \item{mean.frac.dim.index}{...}
  \item{sd.frac.dim.index}{...}
  \item{min.frac.dim.index}{...}
  \item{max.frac.dim.index}{...}
  \item{total.core.area}{...}
  \item{prop.landscape.core}{...}
  \item{mean.patch.core.area}{...}
  \item{sd.patch.core.area}{...}
  \item{min.patch.core.area}{...}
  \item{max.patch.core.area}{...}
  \item{prop.like.adjacencies}{...}
  \item{aggregation.index}{...}
  \item{lanscape.division.index}{...}
  \item{splitting.index}{...}
  \item{effective.mesh.size}{...}
  \item{patch.cohesion.index}{...}
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
#calculate the class statistics
cl.data = ClassStat(tmat)
cl.data
#identify background data is 0
cl.data = ClassStat(tmat,bkgd=0)
cl.data


}
