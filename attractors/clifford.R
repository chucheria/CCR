# Cliffors Attractors

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

clifford <- function(x,y) {
  ac <- abs(c)+1
  ad <- abs(d)+1
  nidxs <- length(mat)
  counts <- integer(length=nidxs)
  for (i in 1:npoints) {
    xt <- sin(a * y) + c * cos(a * x)
    yt <- sin(b * x) + d * cos(b * y)
    x <- xt
    y <- yt
    idxs <- mapxy(x, y, -ac, ac, -ad, ad)
    counts <- counts + tabulate(idxs, nbins=nidxs)
  }
  mat <<- mat + counts
}

npoints <- 1000
n <- 100000
width <- 600
height <- 600

#make some random points
rsamp <- matrix(runif(n * 2, min=-2, max=2), nr=n)

setCompilerOptions(suppressAll=TRUE)
mapxy <- cmpfun(mapxy)
clifford <- cmpfun(clifford)

# 0
a <- -1.4
b <- 1.6
c <- 1.0
d <- 0.7

cvec <- viridis(100)

mat <- matrix(0, nr=height, nc=width)
clifford(rsamp[,1], rsamp[,2])
dens <- log(mat + 1)/log(max(mat))
par(mar=c(0, 0, 0, 0))

png(filename = "clifford0.png", width = 1200, height = 1200, pointsize = 0.1)
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')
dev.off()

# 1
a <- 1.7
b <- 1.7
c <- 0.6
d <- 1.2

cvec <- magma(100)

mat <- matrix(0, nr=height, nc=width)
clifford(rsamp[,1], rsamp[,2])
dens <- log(mat + 1)/log(max(mat))
par(mar=c(0, 0, 0, 0))

png(filename = "clifford1.png", width = 1200, height = 1200, pointsize = 0.1)
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')
dev.off()

# 2
a <- 1.5
b <- -1.8
c <- 1.6
d <- 0.9

cvec <- inferno(100)

mat <- matrix(0, nr=height, nc=width)
clifford(rsamp[,1], rsamp[,2])
dens <- log(mat + 1)/log(max(mat))
par(mar=c(0, 0, 0, 0))

png(filename = "clifford2.png", width = 1200, height = 1200, pointsize = 0.1)
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')
dev.off()

# 3
a <- -1.8
b <- -2.0
c <- -0.5
d <- -0.9

cvec <- plasma(100)

mat <- matrix(0, nr=height, nc=width)
clifford(rsamp[,1], rsamp[,2])
dens <- log(mat + 1)/log(max(mat))
par(mar=c(0, 0, 0, 0))

png(filename = "clifford3.png", width = 1200, height = 1200, pointsize = 0.1)
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')
dev.off()
