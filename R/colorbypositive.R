#'Color by positive or negative
#'
#' @description
#' p.colorbypositive is a function to create a vector with colors by positive or negative.
#' Recommended to color graphics created with metools p.functions.
#'
#' @param x a numeric vector
#' @param colorp Positive values color (default=Green)
#' @param colorn Negative values color (default=Red)
#'
#' @return Return a vector with colors.
#' @export
#'
#' @examples
#' v=c(-3,-2,2,-2,3,2)
#' p.colorbypositive(x=v,colorp="blue",colorn="grey")
#'
#' barplot(v,col=p.colorbypositive(v))
#'
p.colorbypositive=function(x,colorp='#17B221',colorn='#B21717'){
    c=as.numeric(x)
    c = data.frame('c1'=c,"c2"=c)
    c$c2 = c$c1>=0
    c$c2=stringr::str_replace_all(string = c$c2,pattern = 'TRUE',replacement = colorp)
    c$c2=stringr::str_replace_all(string = c$c2,pattern = 'FALSE',replacement = colorn)
    return(c$c2)
}
