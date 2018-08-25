# http://mathworld.wolfram.com/Rose.html
# https://en.wikipedia.org/wiki/Rose_(mathematics)
library(viridis)
library(ggplot2)

custom_theme <- theme(legend.position  = "none",
            panel.background = element_rect(fill="white"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())

params <- data.frame(n = c(7, 3/4, pi, sqrt(2), 500, 500), 
                     m = c(7, 3/4, pi, sqrt(2), 500, 250))
len <- 10e5
theta <- seq(-2*pi,2*pi, length=len)

for (i in range(1:nrow(params))) {
  n <- params[i, 1]
  m <- params[i, 2]
  df <- data.frame(x = 2 * cos(n*theta)*cos(theta), y = 2 * cos(m*theta)*sin(theta))
  ggplot(df, aes(x, y, color=theta)) + 
    geom_point(size = 0.1) + 
    custom_theme + 
    scale_color_viridis(option = sample(c("A", "B", "C", "D", "E"), 1))
  ggsave(paste0('images/rose', n, m, '.png'), dpi = 'retina')
}
