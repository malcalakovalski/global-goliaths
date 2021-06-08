
goliath_colors <- 
  c(`light blue` = '#8AC6FF',
    `dark blue` ='#003A70',
    'orange' = '#F26D00',
    black = "#000000",
    gray = "#d2d2d2",
    `space gray` = "#5c5859",
    yellow = "#fdbf11",
    magenta = "#ec008b"
  )
#' Function to extract goliath colors as hex codes
#'
#' @param ... Character names of goliath_colors 
#'
goliath_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (goliath_colors)
  
  goliath_colors[cols]
}


goliath_palettes <- list(
  `main`  = goliath_cols("dark blue", "light blue", "orange"),
  
  `cool`  = goliath_cols("light blue", "dark blue"),
  
  `hot`   = goliath_cols("yellow", "orange", "magenta"),
  
  `mixed` = goliath_cols("dark blue", "light blue", "yellow", "orange", "magenta"),
  
  `grey`  = goliath_cols("space gray", "gray")
)

#' Return function to interpolate a goliath color palette
#'
#' @param palette Character name of palette in goliath_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
goliath_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- goliath_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Color scale constructor for goliath colors
#'
#' @param palette Character name of palette in goliath_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_goliath <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- goliath_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("goliath_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for goliath colors
#'
#' @param palette Character name of palette in goliath_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_goliath <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- goliath_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("goliath_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}