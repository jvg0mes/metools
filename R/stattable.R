#' Descritive statistic table
#'
#' @description
#' stattable make a descritive statistic table.
#'
#' @param data a dataframe
#' @param horiz defines table be a horizontal table (default=FALSE)
#' @param translate if TRUE translate table to PT-BR (default=FALSE)
#'
#' @return Return a dataframe with descritive statistics.
#' @export
#'
#' @examples
#' v=data.frame(dataone=c(3,2,5,6,5,4),datatwo=c(33,22,55,66,55,44)
#' ,datathree=c(133,122,155,166,155,144))
#' stattable(v) #vertical table
#' stattable(v,translate=TRUE) #vertical table translated
#' stattable(v,horiz=TRUE) #horizontal table
#' stattable(v,horiz=TRUE,translate=TRUE) #horizontal table translated
#'
stattable = function(data,horiz=FALSE,translate=FALSE){
    if(horiz==FALSE){
        a=as.data.frame(data)
        a= as.data.frame(summary(a))
        a=a[,-1]
        a[,1]=as.character(a[,1])
        a[,2]=as.character(a[,2])
        a= tidyr::separate(a,2,into=c("Estatistica","Valor"),sep = ':')
        a= tidyr::pivot_wider(a,names_from= 1,values_from= 3)
        names(a)[1]= 'Statistic'
    if(translate==TRUE){
        names(a)[1]= 'Estatistica'
        a[1,1]= 'Minimo'
        a[2,1]= '1 Quartil'
        a[3,1]= 'Mediana'
        a[4,1]= 'Media'
        a[5,1]= '3 Quartil'
        a[6,1]= 'Maximo'
    }
    }
    if(horiz==TRUE){
        a=as.data.frame(data)
        a= as.data.frame(summary(a))
        a=a[,-1]
        a[,1]=as.character(a[,1])
        a[,2]=as.character(a[,2])
        a= tidyr::separate(a,2,into=c("Estatistica","Valor"),sep = ':')
        a= tidyr::pivot_wider(a,names_from= 2,values_from= 3)
        names(a)[1]= ''

        if(translate==TRUE){
            names(a)[1]= ' '
            names(a)[2]= 'Minimo'
            names(a)[3]= '1 Quartil'
            names(a)[4]= 'Mediana'
            names(a)[5]= 'Media'
            names(a)[6]= '3 Quartil'
            names(a)[7]= 'Maximo'
        }
    }

  a=col2num(a,start=2)

    return(a)
}
