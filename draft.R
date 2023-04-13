library(tibble)
library(dplyr)
data("women")
women2 <- women
women2$height[4] <- 155
women2$weight[4] <- 56
women2

install.packages("hexSticker")
library(hexSticker)
p <- visualize_pps(swiss)
s <- sticker(
  p + guides(fill = 'none') + theme(text = element_blank()),
  package = "ppsr",
  h_color = '#'
  # p_size = 20,
  # s_x = .8,
  # s_y = .6,
  # s_width = 1.4,
  # s_height = 1.2,
  filename = "images/ppsr_plot.png"
)
