library(ggplot2)

# Data frame for the big trunk (rectangle)
trunk <- data.frame(xmin = -1, xmax = 1, ymin = 0, ymax = 2)

# Data frames for branches
first_branches <- data.frame(x = c(-5, 0, 5), y = c(2, 4, 2))
second_branches <- data.frame(x = c(-4, 0, 4), y = c(3.5, 5.5, 3.5))
third_branches <- data.frame(x = c(-3, 0, 3), y = c(5, 6.5, 5))
fourth_branches <- data.frame(x = c(-2, 0, 2), y = c(6.25, 7.5, 6.25))

# Set seed for reproducibility
set.seed(123)

# Function to create ornaments
create_ornaments <- function(x_range, y_position, n = 4) {
  data.frame(
    x = runif(n, x_range[1], x_range[2]),
    y = rep(y_position, n),
    col = sample(c("blue", "red"), size = n, replace = TRUE)
  )
}

# Create data frames for ornaments
first_ornaments <- create_ornaments(c(-5, 5), 2)
second_ornaments <- create_ornaments(c(-4, 4), 3.5)
third_ornaments <- create_ornaments(c(-3, 3), 5)
fourth_ornaments <- create_ornaments(c(-2, 2), 6.25)

# Generate data for presents
xPres <- runif(10, -4.5, 4.5)
xWidth <- runif(10, 0.1, 0.5)
xHeight <- runif(10, 0, 1)
colors <- sample(c("blue", "red"), size = 10, replace = TRUE)
ribbon_colors <- sample(c("gold", "grey87"), size = 10, replace = TRUE)

# Create a data frame for presents
presents <- data.frame(
  xleft = xPres - xWidth,
  xright = xPres + xWidth,
  ybottom = rep(0, 10),
  ytop = xHeight,
  col = colors,
  ribbon_col = ribbon_colors
)

# Plot using ggplot2
Christmas_tree <- ggplot() +
  geom_rect(data = trunk, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "tan3", color = "tan4", linewidth = 2) +
  geom_rect(data = presents, aes(xmin = xleft, xmax = xright, ymin = ybottom, 
                                 ymax = ytop, fill = col)) +
  geom_rect(data = presents, aes(xmin = xleft + 0.2 * (xright - xleft), 
                                 xmax = xright - 0.2 * (xright - xleft), 
                                 ymin = ybottom, ymax = ytop, fill = ribbon_col)) +
  geom_polygon(data = first_branches, aes(x = x, y = y), fill = "palegreen3", 
               color = "palegreen4", linewidth = 1) +
  geom_point(data = first_ornaments, aes(x = x, y = y, color = col), size = 5) +
  geom_polygon(data = second_branches, aes(x = x, y = y), fill = "palegreen4", 
               color = "palegreen3", linewidth = 1) +
  geom_point(data = second_ornaments, aes(x = x, y = y, color = col), size = 4) +
  geom_polygon(data = third_branches, aes(x = x, y = y), fill = "palegreen3", 
               color = "palegreen4", linewidth = 1) +
  geom_point(data = third_ornaments, aes(x = x, y = y, color = col), size = 3) +
  geom_polygon(data = fourth_branches, aes(x = x, y = y), fill = "palegreen4", 
               color = "palegreen3", linewidth = 1) +
  geom_point(data = fourth_ornaments, aes(x = x, y = y, color = col), size = 3) +
  geom_point(aes(x = 0, y = 7.5), shape = 24, size = 7, fill="gold", color="darkred") +
  geom_point(aes(x = 0, y = 7.5), shape = 25, size = 7, fill="gold", color="darkred") + 
  coord_cartesian(xlim = c(-5, 5), ylim = c(0, 7.5)) +
  theme_void() +
  theme(legend.position = "none") +                      
  ggtitle("Christmas Tree")

ggsave(paste("images\\tree", ".png", sep = "_"),
       units = "in", dpi = 300, width = 4, height = 8
)
