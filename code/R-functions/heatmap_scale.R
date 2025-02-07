# Function to plot data resulting from scater::plotDots.
# Function from scatter::.heatmap_scale (in heatmap_scale.R)
# default color scale for gradient changed to dodgerblue3:firebrick3
heatmap_scale <- function(x, center, scale, colour=NULL, zlim=NULL, symmetric=NULL) {

  if (center) {
    x <- x - rowMeans(x)
  }
  if (scale) {
    if (!center & any(rowSums(x) == 0)) {
      stop("Cannot include non-expressed genes when scale=TRUE.")
    }
    x <- x / sqrt(rowSums(x^2) / (ncol(x) - 1))
  }
  if (is.null(zlim)) {
    if (center) {
      extreme <- max(abs(x))
      zlim <- c(-extreme, extreme)
    } else {
      zlim <- range(x)
    }
  }
  if (is.null(colour)) {
    if (center) {
      # colour <- rev(RColorBrewer::brewer.pal(9, "RdYlBu"))
      colour <- colorRampPalette(c("dodgerblue3", "white", 'firebrick3'))(10)
    } else {
      colour <- viridis::viridis(9)
    }
  }
  x[x < zlim[1]] <- zlim[1]
  x[x > zlim[2]] <- zlim[2]
  list(
    x = x,
    colour = colour,
    colour_breaks = seq(zlim[1], zlim[2], length.out=length(colour) + 1L),
    colour_scale = scale_colour_gradientn(colours = colour, limits = zlim),
    fill = colour,
    fill_breaks = seq(zlim[1], zlim[2], length.out=length(colour) + 1L),
    fill_scale = scale_fill_gradientn(colours = colour, limits = zlim),
    zlim = zlim
  )
}
