# Load the required packages
library(ggplot2)
library(grid)
library(ggtext)
library(Cairo)

# Define the coordinates of the hexagon vertices
hexagon_coor <- data.frame(
  x = sin(seq(0, 2 * pi, length.out = 7)),
  y = -cos(seq(0, 2 * pi, length.out = 7))
)

# Function to create the rounded rectangle
rounded_rect <- roundrectGrob(x = 0.5, y = 0.55,
                              width = 0.55, height = 0.3,
                              r = unit(0.1, "snpc"),
                              gp = gpar(fill = "white", col = "black",
                                        lwd = 5))

# Function to draw the down arrow
down_arrow <- function() {
  polygonGrob(
    x = 0.5 + (c(0, 0.2, 0.08, 0.08, -0.08, -0.08, -0.2) * 0.5),
    y = 0.45 + (c(-0.25, -0.05, -0.05, 0.25, 0.25, -0.05, -0.05) * 0.5),
    gp = gpar(fill = "black", col = NA)
  )
}

# Create the hexagon plot
logo <- ggplot() +
  geom_polygon(data = hexagon_coor,
               aes(x = x, y = y),
               fill = "slategrey",
               color = "black",
               linewidth = 2) +
  coord_fixed() +
  theme_void() +
  annotation_custom(grobTree(rounded_rect),
                    xmin = -1, xmax = 1,
                    ymin = -1, ymax = 1) +
  geom_text(aes(x = -0.2, y = 0.1),
            label = "S", color = "black",
            size = 26, fontface = "bold") +
  annotation_custom(grobTree(down_arrow()),
                    xmin = -0.6, xmax = 1.05,
                    ymin = -0.7, ymax = 1.05) +
  geom_richtext(aes(x = 0.015, y = -0.395),
                label = "<span style='color:black;'>survey</span><span style='color:white;'>down</span>",
                size = 12, family = "Verdana",
                fill = NA, label.color = NA) +
  geom_richtext(aes(x = 0, y = -0.38),
                label = "<span style='color:white;'>survey</span><span style='color:black;'>down</span>",
                size = 12, family = "Verdana",
                fill = NA, label.color = NA)
# Call the plot
logo

# Save
ggsave('man/figures/logo.pdf',
       logo, height = 4, width = 4,
       bg = "transparent", device = cairo_pdf)
renderthis::to_png('man/figures/logo.pdf',
                   'man/figures/logo.png', density = 300)
