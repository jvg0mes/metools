#' Round defined columns
#'
#' @description
#' colround round defined columns.
#'
#' @param x a dataframe
#' @param start number of start column
#' @param end number of last column (default=last)
#' @param digits number of round digits
#'
#' @return Return a dataframe with transformed columns.
#' @export
#'
#' @examples
#' v=data.frame(c(3.255,5.826,4.567,2.462))
#' v=colround(v,1,digits=1)
#'
colround=function(x,start,end=ncol(x),digits){
    a=x
    a[,start:end]=mapply(round,x=a[start:end],digits=digits)
    return(a)
}
