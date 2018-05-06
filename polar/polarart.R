# http://mathworld.wolfram.com/Rose.html
# https://en.wikipedia.org/wiki/Rose_(mathematics)
library(viridis)

n = 2
a = 2

cvec <- inferno(10000)


theta <- seq(-2*pi,2*pi, length=10000)
x <- c()
y <- c()

for (i in theta) {
  x <- c(x, a * cos(n*i)*cos(i))
  y <- c(y, a * cos(n*i)*sin(i))
}

png("plot2.png")
plot(x,y, col=cvec, cex=.2,  xaxt='n',  yaxt='n', ann=FALSE)
dev.off()

cvec <- magma(10000)
n = 6

for (i in theta) {
  x <- c(x, a * cos(n*i)*cos(i))
  y <- c(y, a * cos(n*i)*sin(i))
}

png("plot6.png")
plot(x,y, col=cvec, cex=.2,  xaxt='n', yaxt='n', ann=FALSE)
dev.off()

cvec <- viridis(10000)
n = 3/4

for (i in theta) {
  x <- c(x, a * cos(n*i)*cos(i))
  y <- c(y, a * cos(n*i)*sin(i))
}

png("plot34.png")
plot(x,y, col=cvec, cex=.2,  xaxt='n', yaxt='n', ann=FALSE)
dev.off()
