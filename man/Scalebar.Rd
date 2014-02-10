\name{Scalebar}
\alias{Scalebar}
\title{Scalebar for Projected Maps}
\usage{
Scalebar(x, y, distance, unit = "km", scale = 1, t.cex = 0.8)
}
\arguments{
  \item{x}{the x-axis position for the lower left corner of
  the bar}

  \item{y}{the x-axis position for the lower left corner of
  the bar}

  \item{distance}{the distance for which the scale bar
  should represent}

  \item{unit}{the units to report as the scaling}

  \item{scale}{the scaling factor to rescale the distance
  to a different unit. e.g., if your map is in m and want
  the scalebar to be in km, use a scale of 0.01}

  \item{t.cex}{the scaling of the font size to be used for
  the scalebar}
}
\value{
nothing is returned, simply a scalebar is added to a plot.
}
\description{
\code{Scalebar} adds a distance scalebar onto a projected
map. It is not appropriate for geographic projections.
}
\examples{
#create a simple object of class 'asc'
tasc = as.asc(matrix(1:50,nr=50,nc=50)); print(tasc)

#plot the image
image(tasc,axes=FALSE,ann=FALSE)

#add a distance scalebar
Scalebar(x=5,y=5,distance=20) #show values in km
Scalebar(x=5,y=10,distance=20,unit='m',scale=1000) #show values in meters
}
\author{
Jeremy VanDerWal \email{jjvanderwal@gmail.com}
}

