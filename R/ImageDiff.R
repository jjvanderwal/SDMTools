ImageDiff = function(tasc,sig.levels=c(0.025,0.975),tcol=terrain.colors(3),...){
  tasc[which(is.finite(tasc) & tasc<=sig.levels[1])] = 9
  tasc[which(is.finite(tasc) & tasc>sig.levels[1] & tasc<sig.levels[2])] = 10
  tasc[which(is.finite(tasc) & tasc<=1)] = 11
  image(tasc,col=tcol,zlim=c(9,11),...)
}
