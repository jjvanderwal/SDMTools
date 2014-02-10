\name{legend.gradient}
\alias{legend.gradient}
\title{Legend Gradient}
\usage{
legend.gradient(pnts, cols = heat.colors(100), limits = c(0, 1),
  title = "Legend", ...)
}
\arguments{
  \item{pnts}{x and y coordinates of the gradient location
  in the plot}

  \item{cols}{a set of 2 or more colors used in the image,
  to create the gradient}

  \item{limits}{to label the min and max values of the
  gradient in the legend}

  \item{title}{to specify the title of the legend}

  \item{...}{other graphical parameters defined by image()
  or plot()}
}
\value{
nothing is returned, a gradient legend is added to a plot
or a image.
}
\description{
\code{legend.gradient} creates and displays a gradient
legend on a plot or image file. The place and size of the
legend is defined by coordinates, previously identified.
}
\examples{
#define a simple binary matrix
tmat = { matrix(c( 0,0,0,1,0,0,1,1,0,1,
                   0,0,1,0,1,0,0,0,0,0,
                   0,1,NA,1,0,1,0,0,0,1,
                   1,0,1,1,1,0,1,0,0,1,
                   0,1,0,1,0,1,0,0,0,1,
                   0,0,1,0,1,0,0,1,1,0,
                   1,0,0,1,0,0,1,0,0,0,
                   0,1,0,0,0,1,0,NA,NA,NA,
                   0,0,1,1,1,0,0,NA,NA,NA,
                   1,1,1,0,0,0,0,NA,NA,NA),nr=10,byrow=TRUE) }

#do the connected component labeling
tasc = ConnCompLabel(tmat)

# Create a color ramp
colormap=c("grey","yellow","yellowgreen","olivedrab1","lightblue4")

#create an image
image(tasc,col=colormap, axes=FALSE, xlab="", ylab="", ann=FALSE)

#points for the gradient legend
pnts = cbind(x =c(0.8,0.9,0.9,0.8), y =c(1.0,1.0,0.8,0.8))

#create the gradient legend
legend.gradient(pnts,colormap,c("Low","High"))
}
\author{
Lorena Falconi \email{lorefalconi@gmail.com}
}

