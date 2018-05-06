library(deSolve)
library(ggplot2)
library(gridExtra)
library(ggthemes)

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

t <- seq(0.01, 1000, by = 0.01)
out <- as.data.frame(ode(y = iniState, 
                         times = t, 
                         func = Lorenz, 
                         parms = params[1,]))


xplot <- ggplot(data = out, aes(time, X)) +
  geom_line() +
  theme_tufte(ticks=FALSE)

yplot <- ggplot(data = out, aes(time, Y)) +
  geom_line() +
  theme_tufte(ticks=FALSE)

zplot <- ggplot(data = out, aes(time, Z)) +
  geom_line() +
  theme_tufte(ticks=FALSE)

lauplot <- ggplot(data = out, aes(Y, Z)) + 
  geom_point(size = 0.2) +
  theme_tufte(ticks=FALSE)

grid.arrange(xplot, yplot, zplot, lauplot, nrow = 2)


## 3D

library(scatterplot3d)

sequenceLaurence <- function(t) {
  out <- as.data.frame(ode(y = iniState, 
                           times = t, 
                           func = Lorenz, 
                           parms = params[1,]))
  
  scatterplot3d(x=out[,2],
                y=out[,3],
                z=out[,4],
                color="red",
                type="l",
                box=FALSE,
                highlight.3d=F,
                grid=F,
                axis=F,
                xlab=NULL,
                ylab=NULL,
                zlab=NULL,
                main=NULL)
}

sequenceLaurence(t)
