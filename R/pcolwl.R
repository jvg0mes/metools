#' Bar plot with legend
#'
#' @description
#' p.col_wl make a bar plot with legend.
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
#' @param cbserie color of serie border (default= same cserie)
#' @param cticks color of axis ticks
#' @param lwdserie size of serie
#' @param legtitle title of legend box
#' @param legsize size of legend
#' @param cleg color of legend words
#' @param legheight height of legend box
#' @param pnote position of note (default=1) (only numbers)
#' @param cbord color of plot border (default= same cplot)
#' @param titlesize size of title (default=20) (only numbers)
#' @param wordssize size of words (default=12) (only numbers)
#' @param snote size of note (default=11) (only numbers)
#' @param legpos legend position
#' @param legdir legend direction
#' @param legcol color of legend box
#' @param legspa spacement in legend box
#' @param legvjust vertical adjust in legend box
#' @param colors colors of bars, need same number of correspondencies.
#'
#' @return Return a dataframe with transformed columns.
#' @export
#'
#' @examples
#' v=data.frame("x"=1:5,"y"=c(10,4,8,5,2))
#' p.col_wl(v,xaxis= v$x,yaxis=v$y)
#'
#' p.col_wl(v,xaxis= v$x,yaxis=v$y,colors=c('red','blue','green','grey','yellow'))
#'
p.col_wl= function(data,xaxis,yaxis,ybreaks= 10,percent=FALSE,yaccuracy=0.01,ydecimalmark='.',
                        title='title',xlab='X axis',
                        ylab='Y axis',stitle=NULL,note=NULL,ctitles = 'black' ,
                        cscales=ctitles,cbgrid='white',clgrid=cbgrid,
                        cplot='white',cbserie= 'black', cticks='black', lwdserie= 1,
                        legtitle='Legend',legsize=8,cleg=ctitles,legheight=0.5
                        ,pnote=1,cbord=cplot,titlesize=20,wordssize=12,snote=11,legpos='right',
                        legdir='horizontal',legcol='white',legspa=1,legvjust=0.5,colors=grDevices::rainbow(length(xaxis),v = 0.7)){
    if(percent==FALSE){
        g=(ggplot2::ggplot(data,ggplot2::aes(x=xaxis,y=yaxis))+
               ggplot2::geom_col(mapping=ggplot2::aes(fill = base::factor(xaxis,levels=xaxis)),color=cbserie,lwd=lwdserie)
           +ggplot2::labs(title = title, y=ylab, x=xlab,subtitle = stitle,caption=note,fill=legtitle)
           +ggplot2::scale_x_discrete(name=NULL,labels=NULL)+ggplot2::scale_y_continuous(breaks = scales::breaks_extended(ybreaks))
           + ggplot2::theme(legend.title.align = 0.5, legend.text=ggplot2::element_text(face = 'bold',colour = cleg,size = legsize,margin=ggplot2::margin(b=legspa,unit='cm'),vjust = legvjust),
                   legend.key.height = ggplot2::unit(legheight,'cm'),legend.position = legpos,legend.background = ggplot2::element_rect(fill = legcol),
                   legend.key=ggplot2::element_rect(fill= legcol,colour=legcol,color=legcol),
                   title= ggplot2::element_text(angle = 0, hjust = 0.5,size = wordssize,face = 'bold',color=ctitles),
                   plot.title= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = titlesize,face = 'bold',color = ctitles),
                   plot.subtitle = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = wordssize,face = 'bold',color = ctitles),
                   plot.caption= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=pnote,size = snote,face = 'bold',color = ctitles),
                   plot.background = ggplot2::element_rect(fill=cbgrid,colour=cbord,color=cbord), panel.background = ggplot2::element_rect(fill=cplot),
                   panel.grid = ggplot2::element_line(colour=clgrid),axis.ticks =  ggplot2::element_line(color=cticks),
                   axis.line=ggplot2::element_line(colour=cticks),
                   axis.text.y = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=1,size = wordssize,face = 'bold',color = cscales)
           ) + ggplot2::scale_fill_manual(values = scales::alpha(colors))) }else{}
    if(percent==TRUE){
        g=(ggplot2::ggplot(data,ggplot2::aes(x=xaxis,y=yaxis))+
               ggplot2::geom_col(mapping=ggplot2::aes(fill = base::factor(xaxis,levels=xaxis)),color=cbserie,lwd=lwdserie)
           +ggplot2::labs(title = title, y=ylab, x=xlab,subtitle = stitle,caption=note,fill=legtitle)
           +ggplot2::scale_x_discrete(name=NULL,labels=NULL)+ggplot2::scale_y_continuous(labels=scales::label_percent(accuracy=yaccuracy,decimal.mark=ydecimalmark)
                                                                       ,breaks = scales::breaks_extended(ybreaks))
           + ggplot2::theme(legend.title.align = 0.5, legend.text=ggplot2::element_text(face = 'bold',colour = cleg,size = legsize,margin=ggplot2::margin(b=legspa,unit='cm'),vjust = legvjust),
                   legend.key.height = ggplot2::unit(legheight,'cm'),legend.position = legpos,legend.background = ggplot2::element_rect(fill = legcol),
                   legend.key=ggplot2::element_rect(fill= legcol,colour=legcol,color=legcol),
                   title= ggplot2::element_text(angle = 0, hjust = 0.5,size = wordssize,face = 'bold',color=ctitles),
                   plot.title= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = titlesize,face = 'bold',color = ctitles),
                   plot.subtitle = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=0.5,size = wordssize,face = 'bold',color = ctitles),
                   plot.caption= ggplot2::element_text(angle = 0, vjust = 0.5, hjust=pnote,size = snote,face = 'bold',color = ctitles),
                   plot.background = ggplot2::element_rect(fill=cbgrid,colour=cbord,color=cbord), panel.background = ggplot2::element_rect(fill=cplot),
                   panel.grid = ggplot2::element_line(colour=clgrid),axis.ticks =  ggplot2::element_line(color=cticks),
                   axis.line=ggplot2::element_line(colour=cticks),
                   axis.text.y = ggplot2::element_text(angle = 0, vjust = 0.5, hjust=1,size = wordssize,face = 'bold',color = cscales)
           ) + ggplot2::scale_fill_manual(values = scales::alpha(colors)))
    }else{}
    return(g)
}
