#' Lag a data
#'
#' @description
#' me.lag lag a vector if t>0 or lead a vector if t<0.
#'
#' @param x a vector
#' @param t number of times to lag (default=1)
#' @param nafill set value to fill NA's before first t value
#' @param extrapolate if TRUE extrapolate excedent values, only if t>0 (default=FALSE)
#'
#' @return Return a vector.
#' @export
#'
#' @examples
#' v=c(3,2,5,6,5,4)
#' me.lag(v)
#'
#' #now lead
#'
#' me.lag(v,t=-1)
#'
me.lag=function(x,t=1,nafill=NA,extrapolate=FALSE){
    a=x
    b=x
    n=1
    p=t+1

    if(t>=1){
    while(n<=ifelse(extrapolate==FALSE,length(b)-t,length(b))){
        a[[p]]=b[[n]]
        n=n+1
        p=p+1
    }
    a[1:t]=nafill
    }

    if(t<=-1){
        t=t*-1
        p=t+1
        while(n<=length(b)){
            a[[n]]=b[p]
            n=n+1
            p=p+1
        }
        if(is.na(nafill)==FALSE){a=tidyr::replace_na(a,nafill)}
    }

    return(a)
}


