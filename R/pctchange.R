#' Percentual change
#'
#' @description
#' pct_change calculate the percentual change in t periods of a serie.
#' We can use this function to calculate the acumulated variation of an index,
#' for example to calculate the accumulated variation in 12 months just
#' set t parameter to 12
#'
#' @param data a dataframe
#' @param colnum number of column
#' @param t number of periods to accumulate (default= number of rows)
#' @param nafill set value to fill NA's before first t value
#'
#' @return Return a dataframe.
#' @export
#'
#' @examples
#' v=data.frame(test=c(1,2,3,4,5,6,7,8,9,10,11,12,13))
#' pct_change(v)
#'
pct_change=function(data,colnum,t=nrow(data[colnum])-1,nafill=NA){
    a=metools::col2num(data[colnum],1,1)
    b=a
    n=t+1
    while(n<=(nrow(a))){
        a[[1]][n]=(b[[1]][n]-b[[1]][n-t])/b[[1]][n-t]
        n=n+1
    }
    a[[1]][1:t]= nafill

    return(a)
}

