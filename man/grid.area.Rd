\name{grid.area}
\alias{grid.area}
\alias{grid.perimeter}
\title{ Create a Grid of Cell Areas or Perimeters }
\description{
Creates a grid of cell areas or perimeters for spatial grids in geographic (lat-lon) projections.
}
\usage{
grid.area(mat) 
grid.perimeter(mat) 
}
\arguments{
  \item{mat}{a matrix representing a raster of class 'asc' (this & adehabitat package), 'RasterLayer' (raster package) or 'SpatialGridDataFrame' (sp package)}
}
\value{ 
	\item{grid.area}{Returns an ascii grid file which contains the values of the area in each cell.}
	\item{grid.perimter}{Returns an ascii grid file which contains the values of the perimeter in each cell. }
}
\author{ Lorena Falconi \email{lorefalconi@gmail.com} }
\examples{
#Create an ascii file
y=seq(10,50,0.5)
x=seq(140,180,0.5)
cellsize=0.5
data1=sample(160,140)
out1.asc=as.asc(matrix(data1,nc=y, nr=x), xll=min(x), yll=min(y), cellsize=cellsize)

grid.area(out1.asc)[,]

grid.perimeter(out1.asc)[,]
}
