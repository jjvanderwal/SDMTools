\name{compare.matrix}
\Rdversion{1.1}
\alias{compare.matrix}

\title{ Biplot Comparison of Matrices }
\description{
\code{compare.matrix} compares the values within two matrices (e.g., ESRI ArcInfo ASCII 
raster files) and produces a biplot that shows the frequency of each data 
combination shared between the matrices. The plot is overlayed with contour 
lines that demarcate parts of the the plot that share the same frequency of 
data combinations.

\bold{NOTE:} it is assumed the matrices are of the same extent, cell size and 
scaled to be the same units.

}
\usage{
compare.matrix(x,y,nbins,...)
}

\arguments{
  \item{x}{a matrix of data}
  \item{y}{a matrix of data of the same extent as 'x'}
  \item{nbins}{number of equally spaced bins used to partition range of values
  in 'x' & 'y'}
  \item{...}{other graphical parameters defined by image(), contour(), or plot()}
}

\value{
Nothing is returned but images are created.
}

\author{Luke Shoo \email{luke.shoo@jcu.edu.au}}

\examples{

#create some simple objects of class 'asc'
tasc = as.asc(matrix(rep(x=1:10, times=1000),nr=100)); print(tasc)
#modify the asc objects so that they are slightly different
tasc1 = tasc + runif(n = 10000, min = -1, max = 1)
tasc2 = tasc + rnorm(n = 10000, mean = 1, sd = 1)

#create some images
#basic plot showing the density of data combinations shared 
#by the two matrices
compare.matrix(tasc1,tasc2,20)

#same as previous but with data partioned amoung more bins
compare.matrix(tasc1,tasc2,50)

#same as previous but altering the number of contour levels 
#and adding more graphical functions
compare.matrix(tasc1,tasc2,50,nlevels=5, xlab='asc1',ylab='asc2',
  main='Comparison between asc and asc2', bg="grey")

}
