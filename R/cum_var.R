#' Accumulated variation
#'
#' @description
#' cum_var calculates an accumulated variation of a rate,
#'  _var means the data must be a percentage variation.
#'
#' @param data a dataframe
#' @param colnum number of column
#' @param t number of periods to accumulate
#' @param div100 divide data by 100, use if data is not fraction
#'
#' @return Return a dataframe.
#' @export
#'
#' @examples
#' v=data.frame(c(0.03,0.02,0.05))
#' cum_var(v,colnum=1,t=3)
#'
#' v=data.frame(c('3%','2%','5%'))
#' v=colpct2num(v,start=1,div100=TRUE)
#' cum_var(v,colnum=1,t=3)
#'
cum_var=function(data,colnum,t,div100=FALSE){
    if(div100==TRUE){
        a=metools::col2num(data[colnum],1,1)/100
        b=1+a
        t=t-1;n=t
        while(n<=(nrow(b))){
            a[n,1]=prod(b[[1]][(n-t):(n)])
            n=n+1
        }
        a[,1]=a[,1]-1
    }
    else{
        a=metools::col2num(data[colnum],1,1)
        b=1+a
        t=t-1;n=t
        while(n<=(nrow(b))){
            a[n,1]=prod(b[[1]][(n-t):(n)])
            n=n+1
        }
        a[,1]=a[,1]-1
    }
    a[(1:t),]=NA
    return(a)
}
