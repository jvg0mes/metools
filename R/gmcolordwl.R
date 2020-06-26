#'  Ordered Bar Graphic with Legend Model
#'
#' @description
#' gm.col_ord_wl make a bar plot. Graphic models function family do graphic creation
#' easy, is recommended for new programers, they have less and easyful parameters then p.col_ord but
#' the graphic customize is more limited.
#'
#' @param data a dataframe
#' @param ncolx number of x column in data frame
#' @param ncoly number of y column in data frame
#' @param ntimes number of observations to plot (count by tail)
#' @param title title of plot
#' @param legtitle title of legendbox
#' @param xlab x axis label
#' @param ylab y axis label
#' @param dec If TRUE serie come be decrescent,if FALSE crescent(default=F)
#' @param div100 If data percent are not in decimal format set TRUE.
#' @param percent If TRUE, y axis in percent (default=F)
#' @param fontsize change size of all words in graphic (only numbers)
#' @param colors colors of bars
#' @param clines color of lines in graphic
#' @param ctext color of words in graphic
#' @param cbackground color of graphic background
#' @param cbserie color of serie border (default= same cbackground)
#' @param legwpos legend words position (numeric)
#' @param legheight height of legend box
#'
#' @return Return a graphic.
#' @export
#'
#' @examples
#' v=data.frame("x"=seq(from=1,to=4,by=1),"y"=c(5,3,7,2))
#'
#' gm.col_ord_wl(v,1,2,title="Simple example",ntimes=3,legwpos=-2.5)
#'
gm.col_ord_wl=function(data,ncolx,ncoly,ntimes,title,legtitle,xlab=NULL,ylab=NULL,dec=FALSE,div100=FALSE,
                   percent=FALSE,fontsize=0,colors=grDevices::rainbow(n=ntimes,v=0.7),clines='white',
                   ctext='white',cbackground='#141414',cbserie=cbackground,legwpos=0,legheight=0.5){
    (g=metools::p.col_ord_wl(utils::tail(data,n=ntimes),
                         xaxis=utils::tail(data[[ncolx]],n=ntimes),
                         yaxis=utils::tail(metools::col2num(data,ncoly,ncoly)[[ncoly]]/ifelse(div100==TRUE,100,1),n=ntimes),
                         ylab= ylab, title=title,percent=percent,dec=dec,xlab=xlab,
                         cbgrid = cbackground,ctitles = ctext, colors= colors,
                         cbserie=cbserie,cplot = cbackground,cscales = clines,cticks = clines,lwdserie = 0.35,
                         titlesize = (24+fontsize),wordssize = (18+fontsize),legcol = cbackground,
                         legvjust = legwpos,legsize = (18+fontsize),legheight = legheight))
    return(g)
}
