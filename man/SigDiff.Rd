\name{SigDiff}
\Rdversion{1.1}
\alias{SigDiff}
\alias{ImageDiff}

\title{ Identify Regions of Significant Differences }

\description{
\code{SigDiff} computes the significance of the pairwise differences relative to 
the mean and variance of all differences between the two input datasets. This is 
useful for identifying regions of significant difference between two datasets 
(e.g., different DEMs (Januchowski et al. 2010) or different species 
distribution model predictions (Bateman et al 2010)).

\code{ImageDiff} is a wrapper to the image.asc command in adehabitat package that 
uses the result from \code{SigDiff} to create an image mapping the regions of 
significant differences (positive and negative).

\bold{NOTE:} it is assumed the input data are of the same extent and cellsize.

}

\usage{
SigDiff(x,y,pattern=TRUE)
ImageDiff(tasc,sig.levels=c(0.025,0.975),tcol=terrain.colors(3),...)
}

\arguments{
  \item{x}{a vector or matrix of data}
  \item{y}{a vector or matrix of data with the same dimensions of 'x'}
  \item{pattern}{logical value defining if differences are respective to relative
  patterning (TRUE) or absolute values (FALSE)}
  \item{tasc}{a matrix of probability values (0 to 1) likely created by \code{SigDiff}}
  \item{sig.levels}{the significance levels to define significantly above and below. 
  Default settings represent significance at the 0.05 level}
  \item{tcol}{a set of 3 colors for use in the image to represent significantly 
  lower or greater, and not significant}
  \item{...}{other graphical parameters defined by image() or plot()}
}

\value{
\code{SigDiff} returns a vector or matrix of the same dimensions of the input 
representing the significance of the pairwise difference relative to the mean 
and variance of all differences between the two inputs.

\code{ImageDiff} returns nothing but creates an image of the areas of 
significant differences
}

\author{Stephanie Januchowski \email{stephierenee@gmail.com}}

\references{
Januchowski, S., Pressey, B., Vanderwal, J. & Edwards, A. (2010) 
Characterizing errors in topographic models and estimating the financial costs 
of accuracy. International Journal of Geographical Information Science, In Press.

Bateman, B.L., VanDerWal, J., Williams, S.E. & Johnson, C.N. (2010) Inclusion of 
biotic interactions in species distribution models improves predictions under 
climate change: the northern bettong Bettongia tropica, its food resources and 
a competitor. Journal of Biogeography, In Review.
}

\examples{

#create some simple objects of class 'asc'
tasc = as.asc(matrix(1:50,nr=50,nc=50)); print(tasc)
#modify the asc objects so that they are slightly different
tasc1 = tasc + runif(n = 2500, min = -1, max = 1)
tasc2 = tasc + rnorm(n = 2500, mean = 1, sd = 1)

#create graphical representation
par(mfrow=c(2,2),mar=c(1,1,4,1))
image(tasc1,main='first grid',axes=FALSE)
image(tasc2,main='second grid',axes=FALSE)

#get significant difference by spatial patterning
out = SigDiff(tasc1,tasc2)
ImageDiff(out,main="Pattern Differences",axes=FALSE)

#get significant difference by spatial patterning
out = SigDiff(tasc1,tasc2,pattern=FALSE)
ImageDiff(out,main="Absolute Differences",axes=FALSE)
legend('topleft',legend=c('-ve','ns','+ve'),title='significance',
  fill=terrain.colors(3),bg='white')

#close the graphics window resetting the parameters
dev.off()

}
