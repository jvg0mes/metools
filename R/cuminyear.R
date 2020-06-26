#' Accumulated variation in year
#'
#' @description
#' cuminyear calculates an accumulated variation in year of a index.
#'  Data must be start in january, use start to set this,
#'  if you data don't start in january and you need
#'  use this values, consider complete the previous months with 0.
#'
#' @param data a dataframe
#' @param colnum number of values column
#' @param coldate number of date column
#' @param start number of start row
#'
#' @return Return a dataframe.
#' @export
#'
#' @examples
#' v=data.frame(
#' "Date"=c(seq.Date(as.Date("2018-01-01"),as.Date("2019-12-01"),by='month'))
#' ,"Value"=c(rep(2,6),rep(3,6),rep(1,6),rep(5,6)))
#' cuminyear(v,coldate=1,colnum=2)
#'
#' v=data.frame(
#' "Date"=c(seq.Date(as.Date("2018-06-01"),as.Date("2019-12-01"),by='month'))
#' ,"Value"=c(rep(3,7),rep(1,6),rep(5,6)))
#'
#' #this case, we can start in january 2019
#' cuminyear(v,coldate=1,colnum=2,start=8)
#'
#' #or if we need the previous values i can complete january 2018 to may 2018 with 0.
#' v1=data.frame(Date=c(seq.Date(as.Date("2018-01-01"),as.Date("2018-05-01"),by='month')),
#' "Value"=c(rep(0,5)))
#' v=rbind(v1,v)
#' cuminyear(v,coldate=1,colnum=2)
#'
cuminyear=function(data,coldate,colnum,start=1){
    a=NULL
    data[colnum]=metools::col2num(data[colnum],1,1)
    n=start
    while(n<=nrow(data[coldate])){
        if(lubridate::month(data[[coldate]][[n]])==1){a[n]=data[[colnum]][n]/data[[colnum]][n]-1}
        if(lubridate::month(data[[coldate]][[n]])==2){a[n]=data[[colnum]][n]/data[[colnum]][n-1]-1}
        if(lubridate::month(data[[coldate]][[n]])==3){a[n]=data[[colnum]][n]/data[[colnum]][n-2]-1}
        if(lubridate::month(data[[coldate]][[n]])==4){a[n]=data[[colnum]][n]/data[[colnum]][n-3]-1}
        if(lubridate::month(data[[coldate]][[n]])==5){a[n]=data[[colnum]][n]/data[[colnum]][n-4]-1}
        if(lubridate::month(data[[coldate]][[n]])==6){a[n]=data[[colnum]][n]/data[[colnum]][n-5]-1}
        if(lubridate::month(data[[coldate]][[n]])==7){a[n]=data[[colnum]][n]/data[[colnum]][n-6]-1}
        if(lubridate::month(data[[coldate]][[n]])==8){a[n]=data[[colnum]][n]/data[[colnum]][n-7]-1}
        if(lubridate::month(data[[coldate]][[n]])==9){a[n]=data[[colnum]][n]/data[[colnum]][n-8]-1}
        if(lubridate::month(data[[coldate]][[n]])==10){a[n]=data[[colnum]][n]/data[[colnum]][n-9]-1}
        if(lubridate::month(data[[coldate]][[n]])==11){a[n]=data[[colnum]][n]/data[[colnum]][n-10]-1}
        if(lubridate::month(data[[coldate]][[n]])==12){a[n]=data[[colnum]][n]/data[[colnum]][n-11]-1}
        n=n+1
    }
    return(a)
}
