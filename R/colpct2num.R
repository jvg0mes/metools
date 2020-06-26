#' Remove percent from a column, and transform in number
#'
#' @description
#' When use col2percent function to add a percent in a column, the type of this column now is character,
#' colpct2num function remove percent from this column and transform in number.
#'
#' @param x a dataframe
#' @param start number of start column
#' @param end number of last column (default=last)
#' @param div100 division by 100 (T or F)(default=T)
#'
#' @return Return a dataframe with transformed columns.
#' @export
#'
#' @examples
#' v=data.frame(c(15,5,20,50,10))
#' v=col2percent(v,start=1)
#' v=colpct2num(v,start=1,div100=TRUE)
#'
colpct2num=function(x,start,end=ncol(x),div100=TRUE){
    a=x
    if(div100==TRUE){
        a[,start:end]=mapply(stringr::str_remove_all,a[,start:end],"%")
        a=metools::col2num(a,start,end)
        a[,start:end]=a[,start:end]/100
    }else{
        a[,start:end]=mapply(stringr::str_remove_all,a[,start:end],"%")
        a=metools::col2num(a,start,end)
    }
    return(a)
}
