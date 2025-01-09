# Load in code
CODERA_THEME_DIRECORY <- file.path(getwd(), "codera_theme")
source("codera_theme/init.R")
library(ggplot2)

ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of Cars by Cylinder Count and Transmission",
    subtitle = "From mtcars dataset",
    x = "Number of Cylinders",
    y = "Count",
    fill = "Transmission Type",
    caption = "0 = Automatic, 1 = Manual"
  ) +
  theme_codera(
    legend.position = c(.2, .95),
    legend.background=element_blank()
  ) +
  scale_fill_codera()
ggsave(
  "plots/plot_1.png",
  width = 7.7,
  height = 7.7,
  dpi = 400
)

ggplot(economics, aes(x = date, y = unemploy/1000)) +
  geom_line(
    color = codera_colors["codera_red"],
    linewidth = 1
  ) +
  labs(
    title = "US Unemployment Over Time",
    subtitle = "Data from economics dataset",
    x = "Year",
    y = "Unemployment (thousands)",
    caption = "Source: economics dataset"
  ) +
  theme_codera()
ggsave(
  "plots/plot_2.png",
  width = 7.7,
  height = 7.7,
  dpi = 400
)

ggplot(iris, aes(x = Sepal.Length, color = Species)) +
  geom_density(linewidth = 1) +
  labs(
    title = "Distribution of Sepal Length by Species",
    subtitle = "From iris dataset",
    x = "Sepal Length",
    y = "Density",
    color = "Species",
    caption = "Source: economics dataset"
  ) +
  theme_codera(
    legend.position = c(.85, .8),
    legend.background=element_blank()
  ) +
  scale_color_codera()
ggsave(
  "plots/plot_3.png",
  width = 7.7,
  height = 7.7,
  dpi = 400
)
