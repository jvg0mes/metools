#' Time serie bar Graphic Model
#'
#' @description
#' gm.tscol2 make a bar plot in time serie format. The difference between gm.tscol2 and gm.tscol
#' is possibility to select serie color.
#' Graphic models function family make graphic creation easy, is recommended for new programers,
#' they have less and easyful parameters then p.tscol but the graphic customize is more limited.
#' The data don't need be a ts object.
#'
#' @param data a dataframe
#' @param ncolx number of x column in data frame
#' @param ncoly number of y column in data frame
#' @param ntimes number of observations to plot (count by tail)
#' @param title title of plot
#' @param ylab y axis label
#' @param percent If TRUE y axis in percent (default=F)
#' @param div100 If data percent are not in decimal format set TRUE.
#' @param fontsize change size of all words in graphic (only numbers)
#' @param datebreaks datebreaks in x axis (default="1 month")
#' @param dateformat format of date in x axis (need a dataformat string) (default ="\%Y-\%m")
#' @param cserie color of serie
#' @param clines color of lines in graphic
#' @param ctext color of words in graphic
#' @param cbackground color of graphic background
#' @param cbserie color of serie border (default= same cbackground)
#'
#' @return Return a graphic.
#' @export
#'
#' @examples
#' v=data.frame("x"=seq.Date(as.Date('2020-01-01'),
#' to = as.Date('2020-04-01'),by='month'),"y"=c(5,3,7,2))
#'
#' gm.tscol2(v,1,2,title="Simple example",ntimes=3)
#'
gm.tscol2=function(data,ncolx,ncoly,ntimes,title,ylab=NULL,percent=FALSE,div100=FALSE,
                  fontsize=0,datebreaks='1 months',dateformat="%b/%y",cserie='white',
                  clines='white',ctext='white',cbackground='#141414',cbserie=cbackground){
    g=metools::p.tscol(utils::tail(data,n=ntimes),percent=percent,
                       as.Date(utils::tail(data[[ncolx]],n=ntimes)),
                       utils::tail(metools::col2num(data,ncoly,ncoly)[[ncoly]]/ifelse(div100==TRUE,100,1),n=ntimes)
                       ,dateformat = dateformat,xlab = NULL,ylab=ylab,
                       title = title,datebreaks = datebreaks
                       ,lwdserie = 0.35,ctitles = ctext,cbgrid=cbackground,cserie = cserie,
                       cplot = cbackground,cbord=cbackground, cbserie = cbserie,cscales = clines,
                       cticks = clines,titlesize = (24+fontsize), wordssize = (18+fontsize))
    return(g)
}
