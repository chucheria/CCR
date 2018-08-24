# http://mathworld.wolfram.com/Rose.html
# https://en.wikipedia.org/wiki/Rose_(mathematics)
library(viridis)
library(Rcpp)

n = 2
a = 2

cvec <- inferno(100)

len <- 10e3

theta <- seq(-2*pi,2*pi, length=len)

cppFunction('DataFrame rose(int n, NumericVector theta, double a, double len) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            for(int i = 1; i < len + 1; ++i) {
            x[i] = a * cos(a * theta[i]) * cos(theta[i]);
            y[i] = a * cos(a * theta[i]) * sin(theta[i]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

#rose2 <- function(x, y) {
#  for (i in theta) {
#    x <- c(x, a * cos(n*i)*cos(i))
#    y <- c(y, a * cos(n*i)*sin(i))
#  }
#}

df <- rose(n, theta, a, len)
png("plot2.png", width = 1200, height = 1200, pointsize = 0.1, )
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
