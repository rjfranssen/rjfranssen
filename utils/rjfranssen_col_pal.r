# creating a custom color palette
# based on source: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# load libraries
library(scales)
library(dplyr)

# define colors
rjfranssen_colors <- c(
  `ebony` = "#1E2225",
  `blue_grotto` = "#05263B",
  `charcoal` = "#43758A",
  `light_sea_green` = "#36B4AF",
  `pewter` = "#D3DADA",
  `carafe` = "#55423A",
  `goldenrod` = "#F1B416",
  `burnt_sienna` = "#E56A19",
  `crimson` = "#900000")
  
  # function to extract colors
  rjfranssen_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (rjfranssen_colors)

  rjfranssen_colors[cols]
}

# define palettes
rjfranssen_palettes <- list(
  
  `primary`  = rjfranssen_cols("ebony", "blue_grotto", "charcoal", "light_sea_green", "pewter"),

  `secondary`  = rjfranssen_cols("carafe", "goldenrod", "burnt_sienna", "crimson"),

  `all`   = rjfranssen_cols("ebony", "blue_grotto", "charcoal", "light_sea_green", "pewter", "carafe", "goldenrod", "burnt_sienna", "crimson")
)

# function to interpolate palettes
rjfranssen_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- rjfranssen_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

# scale() functions for ggplot2
scale_color_rjfranssen <- function(palette = "main",
           discrete = TRUE,
           reverse = FALSE,
           ...) {
    pal <- rjfranssen_pal(palette = palette, reverse = reverse)
    
    if (discrete) {
      discrete_scale("colour", paste0("rjfranssen_", palette), palette = pal, ...)
    } else {
      scale_color_gradientn(colours = pal(256), ...)
    }
}

scale_fill_rjfranssen <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- rjfranssen_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("rjfranssen_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# Example plots

#ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#    geom_point(size = 4, alpha = .6) +
#    scale_color_rjfranssen(discrete = FALSE, palette = "primary")

#ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#    geom_point(size = 4) +
#    scale_color_rjfranssen("secondary")

#ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#    geom_bar() +
#    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#    scale_fill_rjfranssen(palette = "all", guide = "none")


