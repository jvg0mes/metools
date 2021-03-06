#'  Line Graphic Model
#'
#' @description
#' gm.line make a line plot. Graphic models function family do graphic creation
#' easy, is recommended for new programers, they have less and easyful parameters then p.line but
#' the graphic customize is more limited.
#'
#' @param data a dataframe
#' @param ncolx number of x column in data frame
#' @param ncoly number of y column in data frame
#' @param ntimes number of observations to plot (count by tail)
#' @param title title of plot
#' @param xlab x axis label
#' @param ylab y axis label
#' @param div100 If data percent are not in decimal format set TRUE.
#' @param percent If TRUE, y axis in percent (default=F)
#' @param fontsize change size of all words in graphic (only numbers)
#' @param lwdserie size of serie
#' @param cserie change color of serie
#' @param clines color of lines in graphic
#' @param ctext color of words in graphic
#' @param cbackground color of graphic background
#'
#' @return Return a graphic.
#' @export
#'
#' @examples
#' v=data.frame("x"=seq(from=1,to=4,by=1),"y"=c(5,3,7,2))
#'
#' gm.line(v,1,2,title="Simple example",ntimes=3)
#'
gm.line=function(data,ncolx,ncoly,ntimes,title,xlab=NULL,ylab=NULL,div100=FALSE,
                percent=FALSE,fontsize=0,lwdserie=1.5,cserie='white',clines='white',
                ctext='white',cbackground='#141414'){
    (g=metools::p.line(utils::tail(data,n=ntimes),utils::tail(data[[ncolx]],n=ntimes),
                yaxis=utils::tail(metools::col2num(data,ncoly,ncoly)[[ncoly]]/ifelse(div100==TRUE,100,1),n=ntimes),
                ylab= ylab, title=title,percent=percent,xlab=xlab,
                cserie=cserie,cbgrid = cbackground,ctitles = ctext,
                cplot = cbackground,cscales = clines,cticks = clines,lwdserie = lwdserie,
                titlesize = (24+fontsize),wordssize = (18+fontsize)))
    return(g)
}
