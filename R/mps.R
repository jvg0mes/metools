#' Multi serie plot
#'
#' @description
#' mp.s make a plot with one or more series.
#' The object parameter require a ggplot object (Look at the examples).
#'
#' @param object a ggplot graphic object
#' @param xaxis x axis of one of your graphics
#' @param yaxis y axis of one of your graphics
#' @param ybreaks number of y axis breaks (default=10)
#' @param percent If TRUE y axis in percent (default=F)
#' @param yaccuracy a round for y axis (default=0.01)
#' @param ydecimalmark y decimal mark (default=".")
#' @param title title of plot
#' @param xlab x axis label
#' @param ylab y axis label
#' @param stitle subtitle
#' @param note note
#' @param ctitles color of titles (title,xlab,ylab)
#' @param cscales color of the scales (default= same ctitles)
#' @param cbgrid color of grid background
#' @param clgrid color of grid lines
#' @param cplot color of plot background
#' @param cticks color of axis ticks
#' @param pnote position of note (default=1) (only numbers)
#' @param cbord color of plot border (default= same cplot)
#' @param titlesize size of title (default=20) (only numbers)
#' @param wordssize size of words (default=12) (only numbers)
#' @param snote size of note (default=11) (only numbers)
#' @param xlim limit of x axis (default=NULL)
#'
#' @return Return a graphic.
#' @export
#'
#' @examples
#' v=data.frame("x"=c('a','b','c','d','e'),"y"=c(5,3,7,10,9),"y2"=c(7,2,5,8,7))
#'
#' g= ggplot2::ggplot()+ggplot2::geom_line(mapping=ggplot2::aes(x=v$x,y=v$y,group=1),lwd=2)+
#' ggplot2::geom_line(mapping=ggplot2::aes(x=v$x,y=v$y2,group=1),color='blue',lwd=2)
#'
#' mp.s(object=g,xaxis=v$x,yaxis=v$y,title="Simple example")
#'
#' mp.s(g,v$x,v$y,percent=TRUE,title="Example with percent data",xlab=NULL,ylab=NULL)
#'
#' mp.s(g,v$x,v$y,percent=TRUE,yaccuracy=1,title="y accuracy set",xlab=NULL,ylab=NULL)
#'
#' g= ggplot2::ggplot()+ggplot2::geom_area(mapping=ggplot2::aes(x=v$x,y=v$y),
#' fill='red',lwd=2,group=1)+
#' ggplot2::geom_area(mapping=ggplot2::aes(x=v$x,y=v$y2),fill='blue',lwd=2,group=1)
#'
#' mp.s(g,v$x,v$y,title="Example with area plot")
#'
#' v=data.frame("x"=c('a','b','c','d','e'),"y"=c(5,-3,-6,10,7))
#'
#' g= ggplot2::ggplot()+ggplot2::geom_col(ggplot2::aes(x=v$x,y=v$y,group=1),
#' fill=p.colorbypositive(v$y),color='black',lwd=1)+
#' ggplot2::geom_line(ggplot2::aes(x=v$x,y=v$y,group=1),color='black',lwd=1)
#'
#' mp.s(g,v$x,v$y,title="Example with colorbypositive",xlab=NULL,ylab=NULL)
#'
mp.s=function(object,xaxis,yaxis,ybreaks= 10,percent=FALSE,
               yaccuracy=0.01,ydecimalmark='.',
               title='Title',xlab='X axis',ylab='Y axis',stitle=NULL,note=NULL,
               ctitles = 'black' ,cscales=ctitles,cbgrid='white',clgrid=cbgrid,
               cplot='white', cticks='black',
               pnote=1,cbord=cplot,titlesize=20,wordssize=12,snote=11,xlim=NULL){
    g=object
    if(percent==TRUE){
        g= g + ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = yaccuracy,decimal.mark = ydecimalmark)
                                        ,breaks=scales::breaks_extended(ybreaks)) +
            ggplot2::labs(title = title, y=ylab, x=xlab,subtitle = stitle,caption=note) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1,size= wordssize,face = 'bold',color = cscales),
                           title= ggplot2::element_text(angle = 0, hjust = 0.5,size = wordssize,face = 'bold',color=ctitles),
                           axis.text.y = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=1,size = wordssize,face = 'bold',color = cscales),
                           plot.title= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = titlesize,face = 'bold',color = ctitles),
                           plot.subtitle = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = wordssize,face = 'bold',color = ctitles),
                           plot.caption= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=pnote,size = snote,face = 'bold',color = ctitles),
                           plot.background = ggplot2::element_rect(fill=cbgrid,colour=cbord,color=cbord), panel.background = ggplot2::element_rect(fill=cplot),
                           panel.grid = ggplot2::element_line(colour=clgrid),axis.ticks =  ggplot2::element_line(color=cticks),
                           axis.line=ggplot2::element_line(colour=cticks))

    }
    if(percent==FALSE){
        g= g + ggplot2::scale_y_continuous(breaks=scales::breaks_extended(ybreaks)) +
            ggplot2::labs(title = title, y=ylab, x=xlab,subtitle = stitle,caption=note) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1,size= wordssize,face = 'bold',color = cscales),
                           title= ggplot2::element_text(angle = 0, hjust = 0.5,size = wordssize,face = 'bold',color=ctitles),
                           axis.text.y = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=1,size = wordssize,face = 'bold',color = cscales),
                           plot.title= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = titlesize,face = 'bold',color = ctitles),
                           plot.subtitle = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = wordssize,face = 'bold',color = ctitles),
                           plot.caption= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=pnote,size = snote,face = 'bold',color = ctitles),
                           plot.background = ggplot2::element_rect(fill=cbgrid,colour=cbord,color=cbord), panel.background = ggplot2::element_rect(fill=cplot),
                           panel.grid = ggplot2::element_line(colour=clgrid),axis.ticks =  ggplot2::element_line(color=cticks),
                           axis.line=ggplot2::element_line(colour=cticks))
    }
    return(g)
}
