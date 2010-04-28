\name{quick.map}
\Rdversion{1.1}
\alias{quick.map}

\title{ Quick Map }

\description{
\code{quick.map} creates and displays an image, identifying the threshold as the 
background color, and create the gradient legend in the map.
}

\usage{
quick.map(sdm.asc, threshold, bkgd.col = 'grey',cols=heat.colors(100), axes=TRUE, zlim=NULL)
}
\arguments{
  \item{sdm.asc}{an object of class "asc" as defined in the adehabitat package}
  \item{threshold}{to indicate the thereshold limit of the "asc" object}
  \item{bkgd.col}{to specify the background color}
  \item{cols}{a set of 2 or more colors to be used in the image and the gradient legend}
  \item{zlim}{to specify the upper an lower limits, which are going to be the labels of the gradient legend}
  \item{...}{other graphical parameters defined by image() or plot()}
}

\details{
An image is created with the gradient legend. The gradient legend would be created if the user previously 
specifies the coordinates were the legend has to be done.

}

\value{
nothing is returned, a gradient legend is added to a plot or a image.
}

\author{Lorena Falconi \email{lorefalconi@gmail.com}}

\examples{

#create a matrix
tmat = { matrix(c(	0,0,0,1,0,0,1,1,0,1,
                    0,0,1,0,1,0,0,0,0,0,
                    0,1,NA,1,0,1,0,0,0,1,
					          1,0,1,1,1,0,1,0,0,1,
				            0,1,0,1,0,1,0,0,0,1,
					          0,0,1,0,1,0,0,1,1,0,
					          1,0,0,1,0,0,1,0,0,0,
				            0,1,0,0,0,1,0,0,0,1,
					          0,0,1,1,1,0,0,1,1,1,
				            1,1,1,0,0,0,0,1,1,1),nr=10,byrow=T) }

#do the connected component labeling
tasc = ConnCompLabel(tmat)

#put in the gradient scale
pnts = cbind(x =c(1.1,1.2,1.2,1.1), y =c(0.9,0.9,0.7,0.7))

# Set the map and gradient leyend colors 
tasc.col=colorRampPalette(c("yellow","orange", "red"))(5)

#Create an image with the gradient legend
quick.map(tasc,0.09,bkgd.col = 'darkgrey', cols=tasc.col,axes=F, xlim=c(0.0,1.35))

#########################
# Create an image with two colors: below the threshold and above the threshold
# The next version of SDM Tools will let you create the legend.gradient  at a specific side of your image, and the user would not need to set the coordinates.
# To create the legend.gradient at the bottom left of your image without set up the coordinates at the image you can do this:
xlim=c(-0.5,1)
ylim=c(0,1)
wid = diff(xlim)*0.05
ht = diff(ylim)*0.1

xvals = c(xlim[1]+wid,xlim[1]+2*wid,xlim[1]+2*wid,xlim[1]+wid)
yvals = c(ylim[1]+ht,ylim[1]+ht,ylim[1]+2*ht,ylim[1]+2*ht)
#Create the points for the legend.gradient
pnts=(cbind(xvals,yvals))
# Set the images colors: above the threshold is black and below the threshold is darkgrey.
quick.map(tasc,0.09,bkgd.col = 'darkgrey', cols="black",axes=F, xlim=c(-0.8, 1))
}
