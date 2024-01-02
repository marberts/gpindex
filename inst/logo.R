library(ggplot2)
library(hexSticker)

gg <- ggplot(data.frame(x = 1:4, y = c(1, 3, 2, 4))) +
  geom_line(
    aes(x, y),
    arrow = arrow(angle = 20, ends = "last", type = "closed",
                  length = unit(0.1, "inch")),
    linewidth = 1) +
  theme_void()

sticker(gg,
        package = "gpindex",
        filename = "man/figures/logo.png",
        s_x = 1,
        s_width = 1,
        p_size = 18,
        p_family = "mono",
        p_color = "#6355e0",
        h_fill = "#d2e055",
        h_color = "#6355e0")
