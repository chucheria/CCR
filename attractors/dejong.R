# DeJong attractors

# xn+1 = sin(a yn) - cos(b xn)
# yn+1 = sin(c xn) - cos(d yn)

# http://paulbourke.net/fractals/peterdejong/
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

dejong <- function(x, y) {
  nidxs <- length(mat)
  counts <- integer(length=nidxs)
  for (i in 1:npoints) {
    xt <- sin(a * y) - cos(b * x)
    yt <- sin(c * x) - cos(d * y)
    x <- xt
    y <- yt
    idxs <- mapxy(x, y, -2, 2)
    counts <- counts + tabulate(idxs, nbins=nidxs)
  }
  mat <<- mat + counts
}

npoints <- 10e3
n <- 100000
width <- 800
height <- 800

#make some random points
rsamp <- matrix(runif(n * 2, min=-2, max=2), nr=n)

setCompilerOptions(suppressAll=TRUE)
mapxy <- cmpfun(mapxy)
dejong <- cmpfun(dejong)

# Colors
cvec <- magma(100)

# Initial set up (a, b, c, d)
set.seed(3)
initialvalues <- data.frame(replicate(4,round(runif(10, min=-3, max=4), 1)))

for (i in 1:nrow(initialvalues)) {
  a <- initialvalues[i,1]
  b <- initialvalues[i,2]
  c <- initialvalues[i,3]
  d <- initialvalues[i,4]
  
  cat("Iteration: ", i)
  cat("a: ", a, "b: ", b, "c: ", c, "d: ", d)
  
  mat <- matrix(0, nr=height, nc=width)
  dejong(rsamp[,1], rsamp[,2])
  dens <- log(mat + 1)/log(max(mat))
  
  par(mar=c(0, 0, 0, 0))
  png(filename = paste0("images/NewDejong", i, ".png"), width = 1200, 
      height = 1200, pointsize = 0.1)
  image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')
  dev.off()
  
  cat('Generated image ', i)
}

