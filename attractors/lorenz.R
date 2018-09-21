library(deSolve)
library(viridis)
library(scatterplot3d)

library(rgl)

myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}


Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- s * (Y - X)
    dY <- X * (r - Z) - Y
    dZ <- X * Y - b * Z
    list(c(dX, dY, dZ))
  })
}

iniState <- c(X = 0.01, Y = 1, Z = 1)

s <- c(10)
r <- c(28)
b <- c(8/3)
params <- data.frame(s = s, r = r, b = b)

t <- seq(0.01, 10000, by = 0.01)
cvec <- myColorRamp(c(viridis(5)[1],viridis(5)[2]), t) 

sequenceLaurence <- function(t, color) {
  out <- as.data.frame(ode(y = iniState, 
                           times = t, 
                           func = Lorenz, 
                           parms = params[1,]))
  
  scatterplot3d(x=out[,2],
                y=out[,3],
                z=out[,4],
                color=color,
                type="p",
                box=FALSE,
                highlight.3d=F,
                grid=F,
                axis=F,
                xlab=NULL,
                ylab=NULL,
                zlab=NULL,
                main=NULL)
}

#This is a comment



png(filename = "images/lorenz1.png", width = 1200, height = 1200, pointsize = 0.1)
sequenceLaurence(t,cvec)
dev.off()

