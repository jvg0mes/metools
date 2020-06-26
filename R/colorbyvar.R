#'Color by variation
#'
#' @description
#' p.colorbyvar is a function to create a vector with colors by variation.
#' Recommended to color graphics created with metools p.functions.
#'
#' @param x a numeric vector
#' @param colorp Positive changes color (default=Green)
#' @param colorn Negative changes color (default=Red)
#' @param lag Lag to comparison (default=1)
#'
#' @return Return a vector with colors.
#' @export
#'
#' @examples
#' v=c(3,2,5,6,5,4)
#' p.colorbyvar(x=v,colorp="blue",colorn="grey")
#'
#' barplot(v,col=p.colorbyvar(v))
#'
p.colorbyvar=function(x,colorp='#17B221',colorn='#B21717',lag=1){
    c=as.numeric(x)
    c = data.frame('c1'=c,"c2"=metools::me.lag(c,t=lag,nafill=0))
    c[[2]] = c[[1]]>=c[[2]]
    c[[2]]=stringr::str_replace_all(string = c[[2]],pattern = 'TRUE',replacement = colorp)
    c[[2]]=stringr::str_replace_all(string = c[[2]],pattern = 'FALSE',replacement = colorn)
    return(c[[2]])
}
