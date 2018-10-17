#' Spaghetti Plot
#'
#' Generate a flexible facet spaghetti plot for endpoint analysis
#'
#' @inheritParams longplot
#' @param symb optional parameter to shape markers by a factor type column
#'
#' @return produces a spaghetti plot with study endpoint measurements over time that can be facet by a factor-class column variable
#' @export
#' @examples
#' spgtplot(data = AZA, group_by = "CHRTGRP", x = "ADY", y = "AVAL", symb = "EPOCH")
spgtplot <- function(data, group_by, x, y, subj = "USUBJID", title = "", xlab = "Visit Day", ylab = "AVAL", symb = NULL, caption = ""){
  # check argument parameters
  checkmate::assertDataFrame(data)
  checkmate::assertString(group_by)
  checkmate::assertString(x)
  checkmate::assertString(y)
  checkmate::assertString(subj)
  checkmate::assertString(title)
  checkmate::assertString(xlab)
  checkmate::assertString(ylab)
  checkmate::assertString(symb, null.ok = TRUE)
  checkmate::assertString(caption)

  plotdata <- data
  plotdata[[subj]] = factor(as.character(plotdata[[subj]]))
  plotdata[[group_by]] = factor(as.character(plotdata[[group_by]]))

  gbreaks<-c(levels(plotdata[[group_by]]))
  glabels<-gbreaks

  ggplot2::ggplot(data=plotdata,aes_string(x=x,y=y,group=subj))+
    theme_classic()+
    facet_wrap(stats::as.formula(paste("~",group_by)),nrow=length(gbreaks))+
    geom_line(aes_string(color=subj))+
    geom_point(aes_string(color=subj))+
    {if(!is.null(symb))geom_point(aes_string(color=subj,shape=symb))}+
    guides(colour=F,
           linetype=F)+
    theme(plot.title = element_text(size=10),
          strip.text = element_text(size=8),
          legend.key.size = unit(2,'lines'),
          legend.position = "bottom",
          legend.title=element_blank(),
          legend.text = element_text(size = 8),
          axis.text = element_text(size=10),
          axis.title = element_text(size=10),
          plot.caption = element_text(size = 8,color = "red")
    )+
    labs(title=title,
         y=ylab,
         x=xlab,
         caption=caption)

}
