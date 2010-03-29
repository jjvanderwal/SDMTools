\name{PatchStat}
\Rdversion{1.1}
\alias{PatchStat}

\title{ Landscape Patch Statistics }

\description{
\code{PatchStat} calculates the patch statistics for individual patches
identified in a matrix of data.  
}

\usage{
PatchStat(mat,cellsize=1)
}
\arguments{
  \item{mat}{a matrix of data with individual patches identified as with
  \code{ConnCompLabel}}
  \item{cellsize}{cell size (in meters) is a single value representing the width/height
  of cell edges (assuming square cells)}
}

\details{
The patch statistics are based on statistics calculated by fragstats 
\url{http://www.umass.edu/landeco/research/fragstats/fragstats.html}.
}

\value{
a data.frame listing
  \item{patchID}{...}
  \item{n.cell}{...}
  \item{n.core.cell}{...}
  \item{n.edges.perimeter}{...}
  \item{n.edges.internal}{...}
  \item{area core.area}{...}
  \item{perimeter}{...}
  \item{perim.area.ratio}{...}
  \item{shape.index}{...}
  \item{frac.dim.index}{...}
  \item{core.area.index}{...}
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
