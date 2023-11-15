library(ggplot2)
library(hexSticker)

sticker(ggplot() + theme_void(),
        package = "gpindex",
        filename = "man/figures/logo.png",
        p_y = 1.2,
        p_size = 18,
        p_family = "mono",
        p_color = "#97b3b3",
        h_fill = "#f0f99f",
        h_color = "#97b3b3")
