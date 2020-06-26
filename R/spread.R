#' Spread an dataframe.
#'
#' @description
#' Transforms columns into rows and rows into columns.
#'
#'
#' @param data a dataframe
#' @param namenc name of new column (first column) (default="")
#' @param mode if results are incorretly try set this to TRUE
#'
#' @return Return a dataframe.
#' @export
#'
#' @examples
#' v=data.frame('date'=c('2016','2017'),'value1'=c(12,10),'value2'=c(8,6))
#' me.spread(v,namenc='old header')
#'
me.spread=function(data,namenc=" ",mode=FALSE){

    if(mode==FALSE){

        a=data.frame(1)
        while(ncol(a)<=length(data[[1]])){
            a=cbind(a,1)
        }
        a=a[-1,-1]
        n=1
        while(n<=length(names(data))){
            a[n,]=data[[n]]
            n=n+1
        }

        names(a)=data[[1]]
        a=tibble::rownames_to_column(a,var=namenc)
        a[[1]]=names(data)
        a=a[-1,]
        rownames(a)=NULL

    }

    if(mode==TRUE){

        a=data.frame(1)

        a=cbind(a,names(data));a=data.frame(a[[2]])
        a[[1]]=as.character(a[[1]])

        n=1
        c=2

        while(ncol(a)<=nrow(data)){
            a[,c]=as.matrix(data[n,])[1,]
            c=c+1
            n=n+1
        }
        names(a)=as.character(a[1,]);a=a[-1,]
        names(a)[1]=namenc
    }

    return(a)
}


