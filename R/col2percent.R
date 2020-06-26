#'Add percent in column
#'
#' @description
#' col2percent transform columns to percent.
#'
#' @param x a dataframe
#' @param start number of start column
#' @param end number of last column (default=last)
#' @param mult100 multiply by 100 if the number is a decimal fraction(T or F)(default=F)
#'
#' @return Return a dataframe with transformed columns.
#' @export
#'
#' @examples
#' v=data.frame(c(15,5,20,50,10))
#' col2percent(v,start=1)
#'
#' v=data.frame(c(0.15,0.05,0.2,0.5,0.1))
#' col2percent(v,start=1,mult100=TRUE)
#'
col2percent=function(x,start,end=ncol(x),mult100=FALSE){
    a=x
    if(mult100==TRUE){
        a=metools::col2num(a,start,end)
        a[,start:end]=a[,start:end]*100
        a[,start:end]=mapply(paste0,a[,start:end],"%")
    }else{
        a[,start:end]=mapply(paste0,a[,start:end],"%")
    }
    return(a)
}
