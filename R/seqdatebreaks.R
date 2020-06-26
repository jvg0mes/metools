#' Create Date Interval
#'
#' @description
#' p.seqdatebreaks is a function to break a time axis from graphic in specific interval.
#' This function are recommended to select timeinterval of graphics created with metools p.functions.
#'
#' @param x Time data from a Timeserie
#' @param periodicity Time interval (string)
#'
#' @return Return a vector with timeinterval.
#' @export
#'
#' @examples
#' x <- seq.Date(from=as.Date("2019-01-01"),to=as.Date("2020-01-01"),by=1)
#'  p.seqdatebreaks(x,periodicity= "2 month")
#'
p.seqdatebreaks <- function(x,periodicity){
    a<-x
    a<-seq.Date(from=x[[1]],to=x[[length(x)]],by = periodicity)
    return(a)
}
