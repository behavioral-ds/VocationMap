## here live the extra / additional functions for plotting

#' Add text with background box to a plot
#'
#' \code{boxtext} places a text given in the vector \code{labels} 
#' onto a plot in the base graphics system and places a coloured box behind 
#' it to make it stand out from the background.
#' 
#' @param x numeric vector of x-coordinates where the text labels should be 
#' written. If the length of \code{x} and \code{y} differs, the shorter one 
#' is recycled.
#' @param y numeric vector of y-coordinates where the text labels should be 
#' written. 
#' @param labels a character vector specifying the text to be written.
#' @param col.text the colour of the text 
#' @param col.bg color(s) to fill or shade the rectangle(s) with. The default 
#' \code{NA} means do not fill, i.e., draw transparent rectangles.
#' @param border.bg color(s) for rectangle border(s). The default \code{NA}
#' omits borders. 
#' @param adj one or two values in [0, 1] which specify the x (and optionally 
#' y) adjustment of the labels. 
#' @param pos a position specifier for the text. If specified this overrides 
#' any adj value given. Values of 1, 2, 3 and 4, respectively indicate 
#' positions below, to the left of, above and to the right of the specified 
#' coordinates.
#' @param offset when \code{pos} is specified, this value gives the offset of 
#' the label from the specified coordinate in fractions of a character width.
#' @param padding factor used for the padding of the box around 
#' the text. Padding is specified in fractions of a character width. If a 
#' vector of length two is specified then different factors are used for the
#' padding in x- and y-direction.    
#' @param cex numeric character expansion factor; multiplied by 
#' code{par("cex")} yields the final character size. 
#' @param font the font to be used
#'
#' @return Returns the coordinates of the background rectangle(s). If 
#' multiple labels are placed in a vactor then the coordinates are returned
#' as a matrix with columns corresponding to xleft, xright, ybottom, ytop. 
#' If just one label is placed, the coordinates are returned as a vector.
#' @author Ian Kopacka
#' @examples
#' ## Create noisy background
#' plot(x = runif(1000), y = runif(1000), type = "p", pch = 16, 
#' col = "#40404060")
#' boxtext(x = 0.5, y = 0.5, labels = "some Text", col.bg = "#b2f4f480", 
#'     pos = 4, font = 2, cex = 1.3, padding = 1)
#' @export
boxtext <- function(x, y, labels = NA, col.text = NULL, col.bg = NA, 
                    border.bg = NA, adj = NULL, pos = NULL, offset = 0.5, 
                    padding = c(0.5, 0.5), cex = 1, font = graphics::par('font')){
  
  ## The Character expansion factro to be used:
  theCex <- graphics::par('cex')*cex
  
  ## Is y provided:
  if (missing(y)) y <- x
  
  ## Recycle coords if necessary:    
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]           
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }       
  }
  
  ## Width and height of text
  textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  textWidth <- graphics::strwidth(labels, cex = theCex, font = font)
  
  ## Width of one character:
  charWidth <- graphics::strwidth("e", cex = theCex, font = font)
  
  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      adj <- c(adj[1], 0.5)            
    }        
  } else {
    adj <- c(0.5, 0.5)
  }
  
  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }       
  } else {
    offsetVec <- c(0, 0)
  }
  
  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }
  
  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]
  
  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth    
  graphics::rect(xleft = xMid - rectWidth/2, 
                 ybottom = yMid - rectHeight/2, 
                 xright = xMid + rectWidth/2, 
                 ytop = yMid + rectHeight/2,
                 col = col.bg, border = border.bg)
  
  ## Place the text:
  graphics::text(xMid, yMid, labels, col = col.text, cex = theCex, font = font, 
                 adj = c(0.5, 0.5))    
  
  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }    
}

## Add an alpha value to a colour -- source: https://magesblog.com/post/2013-04-30-how-to-change-alpha-value-of-colours-in/
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

library(ggplot2)
library(MASS)
require(fields)
require("car")
require("colorspace")
require("grDevices")
require(gridExtra)
require(scales)
library(readr)

## function that defines my colors
## Version 1: an enhanced matlab jet color scheme, containing cyan also
myColors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
## Version 2: based on the orange color scheme
myColors <- function(n, color_scheme = "data/blues.csv") { #"magenta.csv" "blues.csv" "orange.csv"
  my_col <- rgb(read.csv(color_scheme, header=FALSE))
  return(colorRampPalette(colors = my_col)(n))
}
