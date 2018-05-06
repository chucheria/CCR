## WITH MATHART

#install.packages(c("devtools", "mapproj", "tidyverse", "ggforce"))
#devtools::install_github("marcusvolz/mathart")
#devtools::install_github("marcusvolz/ggart")

library(mathart)
library(ggart)
library(ggforce)
library(tidyverse)

df <- rbind(parrot() %>% mutate(id = 1),
            stork() %>% mutate(id = 2),
            magpie() %>% mutate(id = 3))

p <- ggplot() +
  geom_point(aes(x, y, alpha = r), df, size = 0.03) +
  facet_wrap(~id, nrow = 2, scales = "free") +
  scale_alpha_continuous(range = c(0.03, 0.06)) +
  theme_blankcanvas(margin_cm = 1)

p

df <- rbind(olive_branch() %>% mutate(id = 1),
            palm_branch() %>% mutate(id = 2),
            branch() %>% mutate(id = 3))

p <- ggplot() +
  geom_circle(aes(x0 = x, y0 = y, r = r), df, size = 0.03, alpha = 0.1) +
  coord_equal() +
  facet_wrap(~id, nrow = 3) +
  theme_blankcanvas(margin_cm = 1)

p

df <- mollusc()
df1 <- df %>% mutate(id = 1)
df2 <- df %>% mutate(id = 2)
df3 <- df %>% mutate(id = 3)

p <- ggplot() +
  geom_point(aes(x, y), df1, size = 0.03, alpha = 0.03) +
  geom_path( aes(x, y), df1, size = 0.03, alpha = 0.03) +
  geom_point(aes(x, z), df2, size = 0.03, alpha = 0.03) +
  geom_path( aes(x, z), df2, size = 0.03, alpha = 0.03) +
  geom_point(aes(y, z), df3, size = 0.03, alpha = 0.03) +
  geom_path( aes(y, z), df3, size = 0.03, alpha = 0.03) +
  facet_wrap(~id, nrow = 2, scales = "free") +
  theme_blankcanvas(margin_cm = 0.5)

p

## BEZIER

install.packages('bezier')
library(bezier)

t <- seq(0, 1, length=1000)
p <- matrix(c(0,0,0, 1,4,3, 2,2,0, 3,0,2, 5,5,0), nrow=5, ncol=3, byrow=TRUE)
bezier_points <- bezier(t=t, p=p[, 1:2])

ggplot() +
  geom_point(aes(bezier_points[,1], bezier_points[,2]))

### LORENZ ATTRACTOR

params <- c(s = 10, r = 28, b = 8/3)
state <- c(X = 0, Y = 1, Z = 1)

Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- s * (Y - X)
    dY <- X * (r - Z) - Y
    dZ <- X * Y - b * Z
    list(c(dX, dY, dZ))
  })
}

times <- seq(0, 50, by = 0.01)
library(deSolve)
out <- ode(y = state, times = times, func = Lorenz, parms = params)
par(oma = c(0, 0, 3, 0))
plot(out, xlab = "time", ylab = "-")
plot(out[, "Y"], out[, "Z"], pch = ".", type = "l")
mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)
