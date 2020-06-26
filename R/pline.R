#' Line plot
#'
#' @description
#' p.line make a line plot.
#'
#' @param data a dataframe
#' @param xaxis x axis data
#' @param yaxis y axis data
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
#' @param cserie color of serie
#' @param cticks color of axis ticks
#' @param lwdserie size of serie
#' @param pnote position of note (default=1) (only numbers)
#' @param cbord color of plot border (default= same cplot)
#' @param titlesize size of title (default=20) (only numbers)
#' @param wordssize size of words (default=12) (only numbers)
#' @param snote size of note (default=11) (only numbers)
#' @param xlim limit of x axis (default=NULL)
#'
#'
#' @return Return a line graphic.
#' @export
#'
#' @examples
#' v=data.frame("x"=1:5,"y"=c(10,4,8,5,2))
#' p.line(v,xaxis= v$x,yaxis=v$y)
#' #or
#' p.line(v,xaxis= v[[1]],yaxis=v[[2]])
#'
p.line=function(data,xaxis,yaxis,ybreaks= 10,percent=FALSE,yaccuracy=0.01,ydecimalmark='.',
               title='Title',xlab='X axis',ylab='Y axis',stitle=NULL,note=NULL,
               ctitles = 'black' ,cscales=ctitles,cbgrid='white',clgrid=cbgrid,
               cplot='white',cserie='black',cticks='black',
               lwdserie= 1,pnote=1,cbord=cplot,titlesize=20,wordssize=12,snote=11,xlim=NULL){

    if(percent==FALSE){
        g=(ggplot2::ggplot(stats::na.exclude(data), ggplot2::aes(x = xaxis , y = yaxis ,group=1)) + ggplot2::geom_line(color=cserie,lwd=lwdserie) +
               ggplot2::scale_y_continuous(breaks=scales::breaks_extended(ybreaks)) +
               ggplot2::labs(title = title, y=ylab, x=xlab,subtitle = stitle,caption=note) +
               ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1,size= wordssize,face = 'bold',color = cscales),
                              title= ggplot2::element_text(angle = 0, hjust = 0.5,size = wordssize,face = 'bold',color=ctitles),
                              axis.text.y = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=1,size = wordssize,face = 'bold',color = cscales),
                              plot.title= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = titlesize,face = 'bold',color = ctitles),
                              plot.subtitle = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = wordssize,face = 'bold',color = ctitles),
                              plot.caption= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=pnote,size = snote,face = 'bold',color = ctitles),
                              plot.background = ggplot2::element_rect(fill=cbgrid,colour=cbord,color=cbord), panel.background = ggplot2::element_rect(fill=cplot),
                              panel.grid = ggplot2::element_line(colour=clgrid),axis.ticks =  ggplot2::element_line(color=cticks),
                              axis.line=ggplot2::element_line(colour=cticks)))
    } else{}
    if(percent==TRUE){
        g=(ggplot2::ggplot(stats::na.exclude(data), ggplot2::aes(x = xaxis , y = yaxis ,group=1)) + ggplot2::geom_line(color=cserie,lwd=lwdserie) +
               ggplot2::scale_y_continuous(labels=scales::label_percent(accuracy = yaccuracy,decimal.mark=ydecimalmark),breaks=scales::breaks_extended(ybreaks)) +
               ggplot2::labs(title = title, y=ylab, x=xlab,subtitle = stitle,caption=note) +
               ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1,size= wordssize,face = 'bold',color = cscales),
                              title= ggplot2::element_text(angle = 0, hjust = 0.5,size = wordssize,face = 'bold',color=ctitles),
                              axis.text.y = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=1,size = wordssize,face = 'bold',color = cscales),
                              plot.title= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = titlesize,face = 'bold',color = ctitles),
                              plot.subtitle = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = wordssize,face = 'bold',color = ctitles),
                              plot.caption= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=pnote,size = snote,face = 'bold',color = ctitles),
                              plot.background = ggplot2::element_rect(fill=cbgrid,colour=cbord,color=cbord), panel.background = ggplot2::element_rect(fill=cplot),
                              panel.grid = ggplot2::element_line(colour=clgrid),axis.ticks =  ggplot2::element_line(color=cticks),
                              axis.line=ggplot2::element_line(colour=cticks)))
    } else{}

    return(g)
}
