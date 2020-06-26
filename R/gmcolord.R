#'  Ordered Bar Graphic Model
#'
#' @description
#' gm.col_ord make a ordered bar plot. Graphic models function family do graphic creation
#' easy, is recommended for new programers, they have less and easyful parameters then p.col_ord but
#' the graphic customize is more limited.
#'
#' @param data a dataframe
#' @param ncolx number of x column in data frame
#' @param ncoly number of y column in data frame
#' @param ntimes number of observations to plot (count by tail)
#' @param title title of plot
#' @param xlab x axis label
#' @param ylab y axis label
#' @param percent If TRUE, y axis in percent (default=F)
#' @param dec If TRUE, bars plot in decrescent order.
#' @param div100 If data percent are not in decimal format set TRUE.
#' @param fontsize change size of all words in graphic (only numbers)
#' @param cserie change color of serie
#' @param clines color of lines in graphic
#' @param ctext color of words in graphic
#' @param cbackground color of graphic background
#' @param cbserie color of serie border (default= same cbackground)
#'
#' @return Return a graphic.
#' @export
#'
#' @examples
#' v=data.frame("x"=seq(from=1,to=4,by=1),"y"=c(5,3,7,2))
#'
#' gm.col_ord(v,1,2,title="Simple example",ntimes=3)
#'
gm.col_ord=function(data,ncolx,ncoly,ntimes,title,xlab=NULL,ylab=NULL,percent=FALSE,
                    div100=FALSE,dec=FALSE,fontsize=0,cserie='#17B221',clines='white',
                      ctext='white',cbackground='#141414',cbserie=cbackground){
    (g=metools::p.col_ord(utils::tail(data,ntimes),utils::tail(data[[ncolx]],n=ntimes)
                      ,yaxis=utils::tail(metools::col2num(data,ncoly,ncoly)[[ncoly]],ntimes)/ifelse(div100==TRUE,100,1),
                      ylab= ylab, title=title,percent=percent,xlab=xlab,
                      cserie=cserie,cbgrid = cbackground,ctitles = ctext,
                      cbserie=cbserie,cplot = cbackground,cscales = clines,cticks = clines,lwdserie = 0.35,
                      titlesize = (24+fontsize),wordssize = (18+fontsize),dec=dec))
        return(g)
}
