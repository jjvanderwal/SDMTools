#function to create a scale bar for a projected map
Scalebar = function(x,y,distance,unit='km',scale=1,t.cex=0.8) {
  xvals = distance*c(0,0.25,0.5,0.75,1)+x
  yvals = c(0,distance/c(30,20,10))+y
  cols <- c("black","white","black","white")
  for (i in 1:4) rect(xvals[i],yvals[1],xvals[i+1],yvals[2],col=cols[i])
  for (i in 1:5) segments(xvals[i],yvals[2],xvals[i],yvals[3])
  labels <- c((xvals[c(1,3)]-xvals[1])*scale,paste((xvals[5]-xvals[1])*scale,unit))
  text(xvals[c(1,3,5)],yvals[4],labels=labels,adj=.5,cex=t.cex)
}
