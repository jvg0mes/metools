#'Transform month names to month numbers
#'
#' @description
#' month2num transform month names to month numbers
#'
#' @param date a month names vector
#'
#' @return Return a month numbers.
#' @export
#'
#' @examples
#' v=c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")
#' month2num(v)
#'
#' v=data.frame('date'=c("janeiro","fevereiro",'março','abril'),'values'=c(18,27,10,48))
#' month2num(v$date)
#' #or
#' month2num(v[[1]])
#'
#' #you can substitute column with function:
#' v$date = month2num(v$date)
#' v[[1]] = month2num(v[[1]])
#'
month2num=function(date){
    a=as.character(date)
    n=1
    while(n<=length(a)){
        if(a[n]=='janeiro' | a[n]=='Janeiro' | a[n]=='Jan' | a[n]=='jan'
           | a[n]=='January' | a[n]=='january'){a[n]='01'}
        if(a[n]=='fevereiro' | a[n]=='Fevereiro' | a[n]=='Fev' | a[n]=='fev'
           | a[n]=='February' | a[n]=='february' | a[n]=='feb'){a[n]='02'}
        if(a[n]=='mar\u00e7o' | a[n]=='Mar\u00e7o' | a[n]=='Mar' | a[n]=='mar'
           | a[n]=='March' | a[n]=='march' | a[n]=='marco' | a[n]=='Marco'){a[n]='03'}
        if(a[n]=='abril' | a[n]=='Abril' | a[n]=='Abr' | a[n]=='abr'
           | a[n]=='April' | a[n]=='april' | a[n]=='apr'){a[n]='04'}
        if(a[n]=='maio' | a[n]=='Maio' | a[n]=='Mai' | a[n]=='mai'
           | a[n]=='May' | a[n]=='may'){a[n]='05'}
        if(a[n]=='junho' | a[n]=='Junho' | a[n]=='Jun' | a[n]=='jun'
           | a[n]=='June' | a[n]=='june'){a[n]='06'}
        if(a[n]=='julho' | a[n]=='Julho' | a[n]=='Jul' | a[n]=='jul'
           | a[n]=='July' | a[n]=='july'){a[n]='07'}
        if(a[n]=='agosto' | a[n]=='Agosto'  | a[n]=='Ago' | a[n]=='ago'
           | a[n]=='August' | a[n]=='august' | a[n]=='aug'){a[n]='08'}
        if(a[n]=='setembro' | a[n]=='Setembro' | a[n]=='Set' | a[n]=='set'
           | a[n]=='September' | a[n]=='september' | a[n]=='sep'){a[n]='09'}
        if(a[n]=='outubro' | a[n]=='Outubro' | a[n]=='Out' | a[n]=='out'
           | a[n]=='October' | a[n]=='october'| a[n]=='oct'){a[n]='10'}
        if(a[n]=='novembro' | a[n]=='Novembro' | a[n]=='Nov' | a[n]=='nov'
           | a[n]=='November' | a[n]=='november'){a[n]='11'}
        if(a[n]=='dezembro' | a[n]=='Dezembro' | a[n]=='Dez' | a[n]=='dez'
           | a[n]=='December' | a[n]=='december' | a[n]=='dec'){a[n]='12'}
        n=n+1
    }
    return(a)
}
