#' Box Plot
#'
#' Generate a flexible box plot for endpoint analysis
#'
#' @inheritParams spgtplot
#' @param group_by x-axis grouping variable
#' @param color_by variable to color by
#' @param facet optional variable to facet plot by
#' @return produces a box plot with optional facet parameters
#' @export
#'
#' @examples
#' boxplot(data = AZA, group_by = "REGION", y = "PDCR", color_by = "BASEUA", facet = "TRT")
boxplot <- function(data, group_by, y, color_by, facet = NULL, subj = "USUBJID", title = "", xlab = "Visit Day", ylab = "AVAL", caption = ""){
  # check argument parameters
  checkmate::assertDataFrame(data)
  checkmate::assertString(group_by)
  checkmate::assertString(y)
  checkmate::assertString(color_by)
  checkmate::assertString(facet, null.ok = TRUE)
  checkmate::assertString(subj)
  checkmate::assertString(title)
  checkmate::assertString(xlab)
  checkmate::assertString(ylab)
  checkmate::assertString(caption)

  plotdata <- data
  plotdata[[subj]] = factor(as.character(plotdata[[subj]]))
  plotdata[[group_by]] = factor(as.character(plotdata[[group_by]]))
  plotdata[[color_by]] = factor(as.character(plotdata[[color_by]]))
  if(!is.null(facet)){
    plotdata[[facet]] = factor(as.character(plotdata[[facet]]))
  }

  glabels <- c(levels(plotdata[[color_by]]))
  gbreaks <- glabels
  gcolors <- hue_pal()(length(gbreaks))

  ggplot2::ggplot(data=plotdata,aes_string(x=group_by,y=y))+
    theme_classic()+
    {if(!is.null(facet))facet_wrap(stats::as.formula(paste("~", facet)))}+
    geom_boxplot(outlier.color = "white", outlier.size = 2)+
    stat_summary(fun.y=mean,color="black",geom="point",shape=18,size=3,show.legend=FALSE)+
    geom_jitter(aes_string(y=y, x=group_by,color=color_by), width=0.10)+
    scale_color_manual(name="",
                       values=gcolors,
                       breaks=gbreaks,
                       labels=glabels)+
    theme(plot.title = element_text(size=12),
          strip.text = element_text(size=10),
          legend.title = element_text(size=8),
          legend.text = element_text(size=10),
          legend.position = "top",
          axis.text = element_text(size=10),
          axis.title = element_text(size=10),
          plot.caption = element_text(size = 8,color = "red")
    )+
    labs(title=title,
         y=ylab,
         x=xlab,
         fill = "",
         caption=caption)+
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))
}
