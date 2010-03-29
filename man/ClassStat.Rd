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
  \item{class}{...}
  \item{n.patches}{...}
  \item{total.area}{...}
  \item{prop.landscape}{...}
  \item{patch.density}{...}
  \item{total.edge}{...}
  \item{edge.density}{...}
  \item{landscape.shape.index}{...}
  \item{largest.patch.index}{...}
  \item{mean.patch.area}{...}
  \item{sd.patch.area}{...}
  \item{min.patch.area}{...}
  \item{max.patch.area}{...}
  \item{perimeter.area.frac.dim}{...}
  \item{mean.perim.area.ratio}{...}
  \item{sd.perim.area.ratio}{...}
  \item{min.perim.area.ratio}{...}
  \item{max.perim.area.ratio}{...}
  \item{mean.shape.index}{...}
  \item{sd.shape.index}{...}
  \item{min.shape.index}{...}
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
