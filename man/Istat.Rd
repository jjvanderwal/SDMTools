\name{Istat}
\alias{Istat}
\title{I Similarity Statistic for Quantifying Niche Overlap}
\usage{
Istat(x, y)
}
\arguments{
  \item{x}{a vector or matrix of data; the matrix can be a
  raster of class 'asc' (adehabitat package), 'RasterLayer'
  (raster package) or 'SpatialGridDataFrame' (sp package)}

  \item{y}{a vector or matrix of data with the same
  dimensions and class of 'x'}
}
\value{
A single value that is the I similarity statistic
}
\description{
\code{Istat} computes the I similarity statistic of Warren
et al. 2009. It is a method for defining niche overlap from
predictions of species' distributions. \cr \cr \bold{NOTE:}
it is assumed the input data are of the same extent and
cellsize, and all values are positive.
}
\details{
The I similarity statistic sums the pair-wise differences
between two predictions to create a single value
representing the similarity of the two distributions. The I
similarity statistic ranges from a value of 0, where two
distributions have no overlap, to 1 where two distributions
are identical (Warren et al., 2008).
}
\examples{
#create some simple objects of class 'asc'
tasc = as.asc(matrix(1:50,nr=50,nc=50)); print(tasc)
#modify the asc objects so that they are slightly different
tasc1 = tasc + runif(n = 2500, min = -1, max = 1)
tasc2 = tasc + rnorm(n = 2500, mean = 1, sd = 1)

#ensure all data is positive
tasc1 = abs(tasc1)
tasc2 = abs(tasc2)

#calculate the I similarity statistic
I = Istat(tasc1,tasc2)
print(I) #high niche overlap

#using a more variable map
tasc2 = tasc + rnorm(n = 2500, mean = 25, sd = 15);tasc2 = abs(tasc2)
I = Istat(tasc1,tasc2)
print(I) #lower niche overlap
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}
}
\references{
Warren, D. L., R. E. Glor, M. Turelli, and D. Funk. 2009.
Environmental Niche Equivalency versus Conservatism:
Quantitative Approaches to Niche Evolution. Evolution
62:2868-2883.
}

