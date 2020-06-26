#' Create Gradient
#'
#' @description
#' p.gradientcolor is a function to make easy create gradient pallet.
#' Recommended to color graphics created with metools p.functions.
#'
#' @param color1 First gradient color
#' @param color2 Last gradient color
#' @param n Number of colors
#'
#' @return Return a vector with colors.
#' @export
#'
#' @examples
#' p.gradientcolor(color1="white",color2="blue",n=10)
#'
#' v = p.gradientcolor("white","blue",n=20)
#' barplot(seq.int(from=1,to=20,by=1),col=v)
#'
p.gradientcolor<-function(color1,color2, n){
    colfunc=grDevices::colorRampPalette(c(color1,color2))
    colfunc=colfunc(n)
    return(colfunc)}
