
# ============================== beyond bar and boxplots by Cedric Scherer
library(tidyverse)
library(ggplot2)
library(ggdark)
library(lubridate)

library(tidyverse)     ## data wrangling + ggplot2
library(colorspace)    ## adjust colors
library(rcartocolor)   ## Carto palettes
library(ggforce)       ## sina plots
library(ggdist)        ## halfeye plots
library(ggridges)      ## ridgeline plots
library(ggbeeswarm)    ## beeswarm plots
library(gghalves)      ## off-set jitter
library(systemfonts)   ## custom fonts



url <- "https://raw.githubusercontent.com/z3tt/DataViz-Teaching/master/data/weissgerber-data.csv"
data <- read_csv(url)



# To use a custom font, one needs to install the .ttf or .otf 
# font files on the local machine.
## general theme
theme_set(theme_void(base_family = "Roboto"))

theme_update(
  axis.text.x = element_text(color = "white", 
                             face = "bold", 
                             size = 16, 
                             margin = margin(t = 6)),
  axis.text.y = element_text(color = "white", 
                             size = 16, 
                             hjust = 1, 
                             margin = margin(r = 6), 
                             family = "Roboto"),
  axis.line.x = element_line(color = "white", size = 1),
  panel.grid.major.y = element_line(color = "grey60", size = .6),
  plot.background = element_rect(fill = "black", color = "black"),
  plot.margin = margin(rep(20, 4))
)
## theme for horizontal charts
theme_flip <-
  theme(
    axis.text.x = element_text(face = "plain", family = "Roboto", size = 16),
    axis.text.y = element_text(face = "bold", family = "Roboto", size = 16),
    panel.grid.major.x = element_line(color = "grey20", size = .6),
    panel.grid.major.y = element_blank(),
    legend.position = "top", 
    legend.text = element_text(family = "Roboto", size = 18),
    legend.title = element_text(face = "ag_Sunset", size = 18, margin = margin(b = 25))
  )

## custom colors
my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2)]



ggplot(data, aes(x = group, y = value, color = group, fill = group)) +
  stat_summary(
    geom = "errorbar",
    fun.max = function(x) mean(x) + sd(x),
    fun.min = function(x) mean(x) - sd(x),
    width = .3, size = 1.2
  ) +
  geom_bar(stat = "summary", width = .8, size = .8) +
  scale_y_continuous(expand = c(0, 0), breaks = 1:9) +
  scale_fill_manual(values = my_pal, guide = "none") +
  scale_color_manual(values = my_pal, guide = "none")







g <- ggplot(data, aes(x = group, 
                      y = value, 
                      color = group, 
                      fill = group)) +
  scale_y_continuous(breaks = 1:9) +
  scale_color_manual(values = my_pal, guide = "none") + 
  scale_fill_manual(values = my_pal, guide = "none") 

g + 
  geom_boxplot(alpha = .5, size = 1.5, outlier.size = 10)

g + 
  geom_violin(
    aes(fill = group, 
        fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2
  )


## custom colors
my_pal <- rcartocolor::carto_pal(n = 7, name = "SunsetDark")[c(1, 3, 7, 2)]


ggplot(data, aes(x = group, y = value, fill = group)) +
  geom_bar(stat = "summary", width = .8) +
  scale_y_continuous(expand = c(0, 0), breaks = 0:4) + 
  scale_fill_manual(values = my_pal) 

ggplot(data, aes(x = group, fill = group)) +
  geom_bar(width = .8) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = my_pal, guide = "none")





# ggplot(data, aes(x = group, 
#                  y = value, 
#                  color = group, 
#                  fill = group)) +
#   stat_summary(
#     geom = "errorbar",
#     fun.max = function(x) mean(x) + sd(x),
#     fun.min = function(x) mean(x) - sd(x),
#     width = .3, size = 1.2
#   ) +
#   geom_bar(stat = "summary", width = .8, size = .8) +
#   scale_y_continuous(expand = c(0, 0), breaks = 1:9) +
#   scale_fill_manual(values = my_pal, guide = "none") +
#   scale_color_manual(values = my_pal, guide = "none")


g <- ggplot(data, aes(x = group, y = value, color = group, fill = group)) +
  scale_y_continuous(breaks = 1:9) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")

g + 
  geom_boxplot(alpha = .5, 
               size = 1.5, 
               outlier.size = 10)

g + 
  geom_boxplot(
    aes(fill = group, fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.5, outlier.size = 5
  )




g + 
  geom_violin(
    aes(fill = group, fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2
  )

g + 
  geom_violin(
    aes(fill = group, fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2, bw = .2
  )

g + 
  geom_violin(
    aes(fill = group, fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2, bw = .8
  )

g + 
  geom_violin(
    aes(fill = group, fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2, bw = .05
  )

g + 
  geom_violin(
    aes(fill = group, fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2, bw = .2, scale = "count"
  )









# ======== HALF EYE 
g + 
  ggdist::stat_halfeye(
    aes(fill = group, 
        fill = after_scale(colorspace::lighten(fill, .3)))
  )

g + 
  ggdist::stat_halfeye(
    aes(fill = group, fill = after_scale(colorspace::lighten(fill, .7)) ),
    adjust = .2, position = position_nudge(x = -.3)
  )

g + 
  ggdist::stat_halfeye(
    aes(fill = group,
        fill = after_scale(colorspace::lighten(fill, .7))),
    .width = 1, 
    point_size = 5, 
    adjust = .2, 
    position = position_nudge(x = -.3)
  )







g_ridges <- 
  ggplot(data, aes(value, fct_rev(group), 
                   color = group, 
                   fill = group)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07)) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none") +
  theme_flip

g_ridges +
  ggridges::geom_density_ridges(
    alpha = .7, 
    size = 1.5
  )


g_ridges +
  ggridges::geom_density_ridges(
    alpha = .8, size = 1.5, 
    rel_min_height = 0.01
  )


# =============== ******
g_ridges + 
  ggridges::geom_density_ridges_gradient(
    aes(fill = stat(x)), 
    color = "#00ffcc", # black
    size = 1.5, 
    rel_min_height = 0.01
  ) +
  scale_fill_viridis_c(
    option = "mako", 
    direction = -1, 
    guide = "none", 
    end = .9
  )


g_ridges + 
  ggridges::stat_density_ridges(
    quantile_lines = TRUE, quantiles = 2, 
    color = "#cc33ff", alpha = .8, size = 1.5
  ) 



# =============
g + geom_point(size = 10, alpha = .33)

g + geom_point(shape = 95, 
               size = 50, 
               alpha = .33)

# ==== i like this jitter 
g + geom_jitter(size = 7, alpha = .5)

g + 
  geom_point(
    position = position_jitter(width = .2, seed = 0),
    size = 7, alpha = .5
  )


g + 
  geom_point(
    position = position_jitter(width = .2, seed = 0),
    size = 7, alpha = .5
  ) +
  geom_point(
    position = position_jitter(width = .2, seed = 0),
    size = 7, 
    stroke = .9, 
    shape = 1, 
    color = "#00ffff"
  )


g + 
  ggforce::geom_sina(
    maxwidth = .6, 
    scale = "count", 
    seed = 1,
    size = 7, 
    alpha = .5
  ) + 
  ggforce::geom_sina(
    maxwidth = .6, scale = "count", seed = 1, 
    size = 7, shape = 1, 
    color = "#00ffff", 
    stroke = .8
  )


# ================ ggdist
g + 
  ggdist::stat_dots(position = position_nudge(x = -.25), size=3)

g + 
  ggdist::stat_dots(side = "both")

g + 
  ggdist::stat_dots(layout = "weave", 
                    position = position_nudge(x = -.25)) # x= -.25





# === beeswarm
g + 
  ggdist::stat_dots(layout = "swarm", side = "both")

g + 
  ggbeeswarm::geom_beeswarm(size = 8, cex = 3)


g + 
  ggbeeswarm::geom_quasirandom(size = 8, width = .33, alpha = .7) + 
  ggbeeswarm::geom_quasirandom(size = 8, width = .33, shape = 1, 
                               color = "#cc99ff", stroke = .8)




# Beeswarm with Median Indicator
g + 
  ggbeeswarm::geom_quasirandom(
    size = 8, 
    width = .33, 
    alpha = .3
  ) +
  stat_summary(
    fun = median, geom = "point", 
    shape = 95, 
    size = 50
  ) + 
  ggbeeswarm::geom_quasirandom(
    size = 8, 
    width = .33, 
    shape = 1, 
    color = "black", stroke = .8
  )



# Box Plot x Jitter Strips
g + 
  geom_boxplot(
    aes(fill = group, 
        fill = after_scale(colorspace::lighten(fill, .8))),
    size = 1.5, 
    outlier.shape = NA
  ) +
  geom_jitter(width = .1, size = 7, alpha = .6)


# boxplot-jitter-outlines
g + 
  geom_boxplot(
    aes(fill = group, 
        fill = after_scale(colorspace::lighten(fill, .7))),
    size = 1.5, 
    outlier.shape = NA
  ) +
  geom_point(
    position = position_jitter(width = .1, seed = 0),
    size = 7, alpha = .5
  ) +
  geom_point(
    position = position_jitter(width = .1, seed = 0),
    size = 7, stroke = .9, shape = 1, color = "white"
  )




# Box Plot x Violin Plot
g + 
  geom_violin(
    aes(fill = group, 
        fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2, 
    bw = .2
  ) + 
  geom_boxplot(
    fill = "white",  size = 1.2, width = .2, outlier.size = 5
  )



g + 
  geom_violin(
    aes(fill = group, 
        fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2, 
    bw = .2
  ) + 
  geom_boxplot(
    fill = "white",  size = 1.2, width = .2, 
    outlier.shape = NA, coef = 0
  )



# ======= very clean and nice
g + 
  geom_violin(
    aes(fill = group, 
        fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2, 
    bw = .2, 
    color = "#00cc99"
  ) +
  geom_boxplot(
    width = .1, 
    size = 1.2, 
    outlier.shape = NA
  ) +
  stat_summary(
    geom = "point",
    fun = median,
    color = "#00cc99",
    size = 5
  )




# Box Plot x Violin Plot x Jitter Strips
g + 
  geom_violin(
    aes(fill = group, 
        fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2, 
    bw = .2
  ) + 
  geom_boxplot(
    fill = "black",  
    size = 1.2, 
    width = .2, 
    outlier.shape = NA, 
    coef = 0
  ) +
  geom_point(
    position = position_jitter(width = .03, seed = 0),
    size = 5, 
    alpha = .8
  ) +
  geom_point(
    position = position_jitter(width = .03, seed = 0),
    size = 6, 
    stroke = .7, 
    shape = 1, 
    color = "#9966ff"
  )


# Box Plot x Violin Plot x Beeswarm Plot
g + 
  geom_violin(
    aes(fill = group, 
        fill = after_scale(colorspace::lighten(fill, .5))),
    size = 1.2, 
    bw = .2
  ) + 
  geom_boxplot(
    fill = "black",  
    size = 1.2, 
    width = .2, 
    outlier.shape = NA, 
    coef = 0
  ) + 
  ggdist::stat_dots(
    layout = "swarm", 
    side = "both", 
    stackratio = .25, 
    dotsize = .2, 
    alpha = .5
  ) + 
  ggdist::stat_dots(
    layout = "swarm", 
    side = "both",
    stackratio = .25, 
    dotsize = .2,
    shape = 1, 
    color = "#9966ff", 
    stroke = .8
  )



# Violin Plot x Sina Plots
g + 
  geom_violin(
    aes(fill = group, 
        fill = after_scale(colorspace::lighten(fill, .7))),
    size = 1.2, 
    bw = .2, 
    width = .6, 
    scale = "count"
  ) +
  stat_summary(
    geom = "point", 
    fun = median,
    shape = 23, 
    size = 6, 
    color = "#00ffcc", # #aa80ff
    stroke = 1.5
  ) + 
  ggforce::geom_sina(
    maxwidth = .5, 
    scale = "count", 
    size = 3, 
    alpha = .5, 
    seed = 0
  ) + 
  ggforce::geom_sina(
    maxwidth = .5, 
    scale = "count", 
    size = 7, 
    alpha = .5, 
    seed = 0,
    shape = 1, 
    color = "#00ffcc", 
    stroke = .8
  )







## Raincloud Plots
g + 
  geom_boxplot(
    width = .2, 
    fill = "black",
    size = 1.5, 
    outlier.shape = NA
  ) +
  ggdist::stat_halfeye(
    adjust = .33, ## bandwidth
    width = .67, 
    color = NA, ## remove slab interval
    position = position_nudge(x = .15)
  ) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .3, 
    alpha = .5, 
    size = 3
  )




ggplot(data, aes(x = forcats::fct_rev(group), y = value, 
                 color = group, fill = group)) +
  geom_boxplot(
    width = .2, 
    fill = "black",
    size = 1.5, 
    outlier.shape = NA
  ) +
  ggdist::stat_halfeye(
    adjust = .33,
    width = .67, 
    color = NA,
    position = position_nudge(x = .15)
  ) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .3, 
    alpha = .5, 
    size = 5
  ) +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07)) +
  scale_y_continuous(breaks = 1:9) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none") +
  theme_flip





g + 
  geom_boxplot(
    width = .2, 
    fill = "black",
    size = 1.5, 
    outlier.shape = NA
  ) +
  ggdist::stat_halfeye(
    adjust = .53, 
    width = .55, 
    color = NA, 
    position = position_nudge(x = .14)
  ) +
  geom_point(
    position = position_nudge(x = -.22),
    shape = 95, size = 24, alpha = .25
  )



































