#' Accumulated variation in year
#'
#' @description
#' cuminyear_var calculates an accumulated variation in year of a rate,
#'  _var means the data must be a percentage variation.
#'  Data must be start in january, if you data don't start in january and you need
#'  use this values, consider complete the previous months with 0.
#'
#' @param data a dataframe
#' @param colnum number of values column
#' @param coldate number of date column
#' @param div100 divide data by 100, use if data is not fraction
#'
#' @return Return a dataframe.
#' @export
#'
#' @examples
#' v=data.frame(
#' "Date"=c(seq.Date(as.Date("2018-01-01"),as.Date("2019-12-01"),by='month'))
#' ,"Value"=c(rep(0.02,12),rep(0.03,12)))
#' cuminyear_var(v,coldate=1,colnum=2)
#'
#' v=data.frame(
#' "Date"=c('january','february','march')
#' ,"Value"=c('1%','3%','2%'))
#' v=colpct2num(v,start=2,div100=TRUE)
#' v[[1]]=month2num(v[[1]])
#' v[[1]]=paste('2018',v[[1]],'01',sep="-")
#' v[[1]]=as.Date(v[[1]])
#' cuminyear_var(v,coldate=1,colnum=2)
#'
cuminyear_var=function(data,coldate,colnum,div100=FALSE){
    if(div100==TRUE){a=metools::col2num(data[colnum],1,1)/100}
    if(div100==FALSE){a=metools::col2num(data[colnum],1,1)}
    b=1+a
    t=1
    n=1
    while(n<=(nrow(b))){
        if(lubridate::month(data[[coldate]][[n]])==1){t=0}
        if(lubridate::month(data[[coldate]][[n]])==2){t=1}
        if(lubridate::month(data[[coldate]][[n]])==3){t=2}
        if(lubridate::month(data[[coldate]][[n]])==4){t=3}
        if(lubridate::month(data[[coldate]][[n]])==5){t=4}
        if(lubridate::month(data[[coldate]][[n]])==6){t=5}
        if(lubridate::month(data[[coldate]][[n]])==7){t=6}
        if(lubridate::month(data[[coldate]][[n]])==8){t=7}
        if(lubridate::month(data[[coldate]][[n]])==9){t=8}
        if(lubridate::month(data[[coldate]][[n]])==10){t=9}
        if(lubridate::month(data[[coldate]][[n]])==11){t=10}
        if(lubridate::month(data[[coldate]][[n]])==12){t=11}
        a[n,1]=prod(b[[1]][(n-t):(n)])
        n=n+1
    }
    a[,1]=a[,1]-1
    return(a)
}
