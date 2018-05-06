# Cliffors Attractors

# xn+1 = sin(a yn) + c cos(a xn)
# yn+1 = sin(b xn) + d cos(b yn)

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

npoints <- 1000
n <- 100000
width <- 600
height <- 600

#make some random points
rsamp <- matrix(runif(n * 2, min=-2, max=2), nr=n)

setCompilerOptions(suppressAll=TRUE)
mapxy <- cmpfun(mapxy)
dejong <- cmpfun(dejong)

# 0
a <- 0.970
b <- -1.899
c <- 1.381
d <- -1.506

cvec <- viridis(100)

mat <- matrix(0, nr=height, nc=width)
dejong(rsamp[,1], rsamp[,2])
dens <- log(mat + 1)/log(max(mat))
par(mar=c(0, 0, 0, 0))

png(filename = "dejong0.png", width = 1200, height = 1200, pointsize = 0.1)
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')
dev.off()

# 1
a <- -2
b <- -2
c <- -1.2
d <- 2

cvec <- magma(100)

mat <- matrix(0, nr=height, nc=width)
dejong(rsamp[,1], rsamp[,2])
dens <- log(mat + 1)/log(max(mat))
par(mar=c(0, 0, 0, 0))

png(filename = "dejong1.png", width = 1200, height = 1200, pointsize = 0.1)
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')
dev.off()

# 2
a <- 1.40
b <- 1.56
c <- 1.40
d <- -6.56

cvec <- inferno(100)

mat <- matrix(0, nr=height, nc=width)
dejong(rsamp[,1], rsamp[,2])
dens <- log(mat + 1)/log(max(mat))
par(mar=c(0, 0, 0, 0))

png(filename = "dejong2.png", width = 1200, height = 1200, pointsize = 0.1)
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')
dev.off()

# 3
a <- 1.4
b <- -2.3
c <- 2.4
d <- -2.1

cvec <- plasma(100)

mat <- matrix(0, nr=height, nc=width)
dejong(rsamp[,1], rsamp[,2])
dens <- log(mat + 1)/log(max(mat))
par(mar=c(0, 0, 0, 0))

png(filename = "dejong3.png", width = 1200, height = 1200, pointsize = 0.1)
image(t(dens), col=cvec, useRaster=T, xaxt='n', yaxt='n')
dev.off()
