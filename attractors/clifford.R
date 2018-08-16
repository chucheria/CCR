# Clifford Attractors

# xn+1 = sin(a yn) + c cos(a xn)
# yn+1 = sin(b xn) + d cos(b yn)

# http://paulbourke.net/fractals/clifford/
# https://github.com/petewerner/misc/blob/master/attractor.R
# https://es.wikipedia.org/wiki/Atractor

library(compiler)
library(viridis)

mapxy <- function(x, y, xmin, xmax, ymin=xmin, ymax=xmax) {
  sx <- (width - 1) / (xmax - xmin)
  sy <- (height - 1) / (ymax - ymin)
  row0 <- round( sx * (x - xmin) )
  col0 <- round( sy * (y - ymin) )
  col0 * height + row0 + 1
}


clifford <- function(x, y) {
  ac <- abs(c)+1
  ad <- abs(d)+1
  nidxs <- length(mat)
  counts <- integer(length=nidxs)
  for (i in 1:npoints) {
    xt <- sin(a * y) + c * cos(a * x)
    y <- sin(b * x) + d * cos(b * y)
    x <- xt
    idxs <- mapxy(x, y, -ac, ac, -ad, ad)
    counts <- counts + tabulate(idxs, nbins=nidxs)
  }
  mat <<- mat + counts
}

npoints <- 10e3
n <- 100000
width <- 600
height <- 600

#make some random points
rsamp <- matrix(runif(n * 2, min=-2, max=2), nr=n)

setCompilerOptions(suppressAll=TRUE)
mapxy <- cmpfun(mapxy)
clifford <- cmpfun(clifford)

# Colors
cvec <- viridis(100)

# Initial set up (a, b, c, d)
set.seed(2)
initialvalues <- data.frame(replicate(4,round(runif(10, min=-2, max=2), 2)))

for (i in 1:nrow(initialvalues)) {
  a <- initialvalues[i,1]
  b <- initialvalues[i,2]
  c <- initialvalues[i,3]
  d <- initialvalues[i,4]
  
  cat("Iteration: ", i)
  cat("a: ", a, "b: ", b, "c: ", c, "d: ", d)
  
  mat <- matrix(0, nr=height, nc=width)
  clifford(rsamp[,1], rsamp[,2])
  dens <- log(mat + 1)/log(max(mat))
  par(mar=c(0, 0, 0, 0))
  
  png(filename = paste0("images/NewNewNewNewClifford", i, ".png"), width = 1200, 
      height = 1200, pointsize = 0.1)
  image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')
  dev.off()
  
  cat('Generated image ', i)
}