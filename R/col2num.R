#'Transform defined columns to numeric.
#'
#' @description
#' col2num transform columns type to numeric.
#'
#' @param x a dataframe
#' @param start number of start column
#' @param end number of last column (default=last)
#'
#' @return Return a dataframe with transformed columns.
#' @export
#'
#' @examples
#' v=data.frame(c('3','2','5','6','5','4'))
#' class(v[,1]) #here class is factor
#' v=col2num(v,1)
#' class(v[,1]) #now class is character
#'
col2num=function(x,start,end=ncol(x)){
    a=x
    a[,start:end]=sapply(a[,start:end],FUN = as.numeric)
    return(a)
}
