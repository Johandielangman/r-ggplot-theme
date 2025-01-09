
# ============== // DEFINE THE PALETTE // ==============

codera_color_palette <- c(
  "codera_red" = "#be1e2d",
  "codera_blue" = "#273b8d",
  "codera_light_blue" = "#02acba",
  "codera_grey" = "#3f3a34",
  "codera_purple" = "#7030a0",
  "codera_green" = "#306b34",
  "codera_orange" = "#ff8c00",
  "codera_black" = "#000000"
)

# ============== // HELPER FUNCTIONS // ==============

codera_color <- function (color_name) {
  unname(codera_color_palette[color_name])
}

get_codera_colors <- function(...) {
  colors <- c(...)
  if (is.null(colors))
    return (codera_color_palette)
  codera_color_palette[colors]
}


get_codera_palette <- function(
    palette = "five",
    reverse = FALSE,
    ...
  ) {
  
  codera_color_palettes <- list(
    two =   get_codera_colors(
      "codera_red",
      "codera_blue"
    ),
    three =  get_codera_colors(
      "codera_red",
      "codera_blue",
      "codera_light_blue"
    ),
    four = get_codera_colors(
      "codera_red",
      "codera_blue",
      "codera_light_blue",
      "codera_grey"
    ),
    five = get_codera_colors(
      "codera_red",
      "codera_blue",
      "codera_light_blue",
      "codera_grey",
      "codera_purple"
    ),
    six = get_codera_colors(
      "codera_red",
      "codera_blue",
      "codera_light_blue",
      "codera_grey",
      "codera_purple",
      "codera_green"
    ),
    seven = get_codera_colors(
      "codera_red",
      "codera_blue",
      "codera_light_blue",
      "codera_grey",
      "codera_purple",
      "codera_green",
      "codera_orange"
    ),
    eight = get_codera_colors(
      "codera_red",
      "codera_blue",
      "codera_light_blue",
      "codera_grey",
      "codera_purple",
      "codera_green",
      "codera_orange",
      "codera_black"
    )
  )
  
  p <- codera_color_palettes[[palette]]
  if (reverse) p <- rev(p)
  grDevices::colorRampPalette(p, ...)
}

# ============== // GGPLOT SCALE PALETTES // ==============

scale_color_codera <- function(
    palette = "seven",
    discrete = TRUE,
    reverse = FALSE,
    ...
  ) {
  
  p <- get_codera_palette(
    palette = palette,
    reverse = reverse
  )
  
  if (discrete) {
    ggplot2::discrete_scale(
      "color", paste0("pilot_", palette), palette = p, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = p(256), ...)
  }
}


scale_fill_codera <- function(
    palette = "seven",
    discrete = TRUE,
    reverse = FALSE,
    ...
  ) {
  
  p <- get_codera_palette(
    palette = palette,
    reverse = reverse
  )
  
  if (discrete) {
    ggplot2::discrete_scale(
      "fill", paste0("pilot_", palette), palette = p, ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = p(256), ...)
  }
}