library(ggplot2)
library(viridis)
library(dplyr)

custom_theme <- theme(legend.position  = "none",
                      panel.background = element_rect(fill="white"),
                      axis.ticks       = element_blank(),
                      panel.grid       = element_blank(),
                      axis.title       = element_blank(),
                      axis.text        = element_blank())

seq(from=-10, to=10, by = 0.01)  %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(2 * cos(x) + cos(y), 2 * cos(y) + sin(x), color = cos(x))) +
  geom_point(alpha=.1, shape='.', size=0.05) +
  scale_color_viridis(option = sample(c("A", "B", "C", "D", "E"), 1)) +
  custom_theme
ggsave(paste0('images/seq', 'cilinder', '.png'), dpi = 'retina')

seq(from=-10, to=10, by = 0.01)  %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(cos(x) + sin(y), cos(y) + sin(x), color = cos(y))) +
  geom_point(alpha=.1, shape=20, size=1) +
  scale_color_viridis() +
  custom_theme
ggsave(paste0('images/seq', 'oval', '.png'), dpi = 'retina')

seq(from=-2*pi, to=2*pi, by = 0.005)  %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(cos(x) + sqrt(y), cos(y) + sqrt(x), color = sin(y))) +
  geom_point(alpha=.1, shape='.', size=1) +
  scale_color_viridis(option="A") +
  custom_theme
ggsave(paste0('images/seq', 'superficie', '.png'), dpi = 'retina')

seq(from=-2*pi, to=2*pi, by = 0.005)  %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(sin(x) + log(y), cos(y) + log(x), color = sin(y))) +
  geom_point(alpha=.1, shape='.', size=1) +
  scale_color_viridis(option="D") +
  custom_theme
ggsave(paste0('images/seq', 'superficielog', '.png'), dpi = 'retina')

