#' ggplot color selection
#' 
#' @param n number to ID
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#' Colorblind friendly pallete
#' 
#' Define a color-blind friendly palette for up to 8 items.
#' Colors are from the
#' \href{http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette}{r-cookbook}
#' More than 8 will be defined by ggplot defaults.
#' 
#' 
#' @examples 
#' gg_cbb_col(3)
#' 
#' @export
gg_cbb_col <- function(n) {
  
  cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                  "#0072B2", "#D55E00", "#CC79A7")
  
  if(n > 8) {
    cbbPalette <- c(cbbPalette,gg_color_hue(n-8))
  }
  return(cbbPalette[1:n])
}
