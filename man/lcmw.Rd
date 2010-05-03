\name{lcmw}
\alias{lcmw}

\title{ Least Cost Moving Windows Calculation }

\description{ This is a moving window that for each cell returns the minimum 'cost' based on surrounding data cells and some dispersal distance cost. }

\usage{ lcmw(mat, mw, mnc) }

\arguments{
	\item{mat}{a matrix of values that can be based on a raster dataset. Lower values should represent lower cost. The matrix can be a raster of class 'asc' (adehabitat package), 'RasterLayer' (raster package) or 'SpatialGridDataFrame' (sp package)}
	\item{mw}{a distance-cost matrix to be applied to each cell of 'mat'. This matrix can be dispersal costs. Lower values should represent lower cost.}
	\item{mnc}{an integer value representing the radius for 'mw' in number of cells.}
}

\details{ This method moves over the matrix of values, summing the moving window cost \code{mw} and the matrix \code{mat}, returning the minimum cost value. This was created to estimate the least cost path through time for all cells in a matrix (see example). }

\value{ A matrix of values of the same dimensions and class as input \code{mat} }

\author{ Jeremy VanDerWal \email{jjvanderwal@gmail.com} }

\examples{

#create a simple object of class 'asc'
tasc = as.asc(matrix(1:100,nr=10,nc=10)); print(tasc)

#show the input matrix
print(tasc[1:10,1:10])

#vary the moving windows

###no cost window of 2 cell radius
tcost = matrix(0,nr=5,nc=5); print(tcost)
out = lcmw(tasc, tcost, 2); print(out[1:10,1:10])

###no cost with a circular radius of 2
tcost = matrix(NA,nr=5,nc=5)
#populate the distances
for (y in 1:5){
    for (x in 1:5){
        tcost[y,x] = sqrt((3-y)^2 + (3-x)^2)
    }
}

#remove distance values > max.num.cells
tcost[which(tcost>2)]=NA

#no cost matrix
tcost1 = tcost; tcost1[is.finite(tcost1)]=1; print(tcost1)
out = lcmw(tasc, tcost1, 2); print(out[1:10,1:10])

#linear cost
tcost = tcost/2; print(tcost)
out = lcmw(tasc, tcost, 2); print(out[1:10,1:10])

}


