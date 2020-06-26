#'Transform month numbers to month names
#'
#' @description
#' num2month transform month numbers to month names
#'
#' @param date a month numbers vector
#' @param abbreviate abbreviate months name
#' @param ptbr transalate result to "Portugues (Brasil)".
#'
#'
#' @return Return a month names.
#' @export
#'
#' @examples
#' v=c(01,02,03,04,05,06,07,08,09,10,11,12)
#' num2month(v)
#' num2month(v,abbreviate=TRUE)
#' num2month(v,abbreviate=FALSE,ptbr=TRUE)
#' num2month(v,abbreviate=TRUE,ptbr=TRUE)
#'
#' v=data.frame('date'=c(01,02,03,04),'values'=c(18,27,10,48))
#' num2month(v$date)
#' #or
#' num2month(v[[1]])
#'
#' #you can substitute column with function:
#' v$date = num2month(v$date)
#' v[[1]] = num2month(v[[1]])
#'
#' #The data can be a string, but is recommended use numbers,
#' #see a string examples:
#' v=c('01','02','03','04','05','06','07','08','09','10','11','12')
#' num2month(v)
#'
#' v=c('1','2','3','4','5','6','7','8','9','10','11','12')
#' num2month(v)
#'
#'
#'
num2month=function(date,abbreviate=FALSE,ptbr=FALSE){
    a=as.character(date)
    n=1
    if(ptbr==FALSE & abbreviate==FALSE){
        while(n<=length(a)){
            if(a[n]==01 | a[n]=="01" | a[n]=="1"){a[n]='january'}
            if(a[n]==02 | a[n]=="02" | a[n]=="2"){a[n]='february'}
            if(a[n]==03 | a[n]=="03" | a[n]=="3"){a[n]='march'}
            if(a[n]==04 | a[n]=="04" | a[n]=="4"){a[n]='april'}
            if(a[n]==05 | a[n]=="05" | a[n]=="5"){a[n]='may'}
            if(a[n]==06 | a[n]=="06" | a[n]=="6"){a[n]='june'}
            if(a[n]==07 | a[n]=="07" | a[n]=="7"){a[n]='july'}
            if(a[n]==08 | a[n]=="08" | a[n]=="8"){a[n]='august'}
            if(a[n]==09 | a[n]=="09" | a[n]=="9"){a[n]='september'}
            if(a[n]==10 | a[n]=="10"){a[n]='october'}
            if(a[n]==11 | a[n]=="11"){a[n]='november'}
            if(a[n]==12 | a[n]=="12"){a[n]='december'}
        n=n+1
    }}
    if(ptbr==FALSE & abbreviate==TRUE){
        while(n<=length(a)){
            if(a[n]==01 | a[n]=="01" | a[n]=="1"){a[n]='jan'}
            if(a[n]==02 | a[n]=="02" | a[n]=="2"){a[n]='feb'}
            if(a[n]==03 | a[n]=="03" | a[n]=="3"){a[n]='mar'}
            if(a[n]==04 | a[n]=="04" | a[n]=="4"){a[n]='apr'}
            if(a[n]==05 | a[n]=="05" | a[n]=="5"){a[n]='may'}
            if(a[n]==06 | a[n]=="06" | a[n]=="6"){a[n]='jun'}
            if(a[n]==07 | a[n]=="07" | a[n]=="7"){a[n]='jul'}
            if(a[n]==08 | a[n]=="08" | a[n]=="8"){a[n]='aug'}
            if(a[n]==09 | a[n]=="09" | a[n]=="9"){a[n]='sep'}
            if(a[n]==10 | a[n]=="10"){a[n]='oct'}
            if(a[n]==11 | a[n]=="11"){a[n]='nov'}
            if(a[n]==12 | a[n]=="12"){a[n]='dec'}
            n=n+1
            }}
    if(ptbr==TRUE & abbreviate==FALSE){
        while(n<=length(a)){
            if(a[n]==01 | a[n]=="01" | a[n]=="1"){a[n]='janeiro'}
            if(a[n]==02 | a[n]=="02" | a[n]=="2"){a[n]='fevereiro'}
            if(a[n]==03 | a[n]=="03" | a[n]=="3"){a[n]='mar\u00e7o'}
            if(a[n]==04 | a[n]=="04" | a[n]=="4"){a[n]='abril'}
            if(a[n]==05 | a[n]=="05" | a[n]=="5"){a[n]='maio'}
            if(a[n]==06 | a[n]=="06" | a[n]=="6"){a[n]='junho'}
            if(a[n]==07 | a[n]=="07" | a[n]=="7"){a[n]='julho'}
            if(a[n]==08 | a[n]=="08" | a[n]=="8"){a[n]='agosto'}
            if(a[n]==09 | a[n]=="09" | a[n]=="9"){a[n]='setembro'}
            if(a[n]==10 | a[n]=="10"){a[n]='outubro'}
            if(a[n]==11 | a[n]=="11"){a[n]='novembro'}
            if(a[n]==12 | a[n]=="12"){a[n]='dezembro'}
            n=n+1
        }}
    if(ptbr==TRUE & abbreviate==TRUE){
        while(n<=length(a)){
            if(a[n]==01 | a[n]=="01" | a[n]=="1"){a[n]='jan'}
            if(a[n]==02 | a[n]=="02" | a[n]=="2"){a[n]='fev'}
            if(a[n]==03 | a[n]=="03" | a[n]=="3"){a[n]='mar'}
            if(a[n]==04 | a[n]=="04" | a[n]=="4"){a[n]='abr'}
            if(a[n]==05 | a[n]=="05" | a[n]=="5"){a[n]='mai'}
            if(a[n]==06 | a[n]=="06" | a[n]=="6"){a[n]='jun'}
            if(a[n]==07 | a[n]=="07" | a[n]=="7"){a[n]='jul'}
            if(a[n]==08 | a[n]=="08" | a[n]=="8"){a[n]='ago'}
            if(a[n]==09 | a[n]=="09" | a[n]=="9"){a[n]='set'}
            if(a[n]==10 | a[n]=="10"){a[n]='out'}
            if(a[n]==11 | a[n]=="11"){a[n]='nov'}
            if(a[n]==12 | a[n]=="12"){a[n]='dez'}
            n=n+1
        }}
    return(a)
}
