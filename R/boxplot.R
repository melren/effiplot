#' Box Plot
#'
#' Generate a flexible box plot for endpoint analysis
#'
#' @inheritParams lineplot
#' @param group_by x-axis grouping factor variable (string)
#' @param color_by factor variable to color by (string)
#' @return produces a box plot with optional facet parameters
#' @export
#'
#' @examples
#' boxplot(
#'   data = AZA, group_by = "REGION", y = "PDCR", color_by = "BASEUA", facet = "TRT",
#'   title = "Comparison of Treatment", xlab = "Region", ylab = "% Decrease in Ulcer Area",
#'   caption = "Colored by baseline ulcer area", size = "large"
#' )
boxplot <- function(data, group_by, y, color_by, facet = NULL, title = "", xlab = "Visit Day", ylab = "AVAL", caption = "", size = "small"){
  # check argument parameters
  checkmate::assertDataFrame(data)
  checkmate::assertString(group_by)
  checkmate::assertString(y)
  checkmate::assertString(color_by)
  checkmate::assertString(facet, null.ok = TRUE)
  checkmate::assertString(title)
  checkmate::assertString(xlab)
  checkmate::assertString(ylab)
  checkmate::assertString(caption)
  checkmate::assertChoice(size, choices = c("small", "large"))
  
  # Fetch sizes to use
  sz <- sizes[sizes[["func"]]=="boxplot", ]
  
  plotdata <- data
  
  glabels <- c(levels(plotdata[[color_by]]))
  gbreaks <- glabels
  gcolors <- hue_pal()(length(gbreaks))
  
  ggplot2::ggplot(data=plotdata,aes_string(x=group_by,y=y))+
    theme_classic()+
    {if(!is.null(facet))facet_wrap(stats::as.formula(paste("~", facet)))}+
    geom_boxplot(outlier.color = "white", outlier.size = 2)+
    stat_summary(fun.y=mean,color="black",geom="point",shape=18,size=3,show.legend=FALSE)+
    geom_jitter(aes_string(y=y, x=group_by,color=color_by), width=0.10)+
    scale_color_manual(name=color_by,
                       values=gcolors,
                       breaks=gbreaks,
                       labels=glabels)+
    theme(plot.title = element_text(size=sz[sz["elements"]=="title",][[size]]),
          strip.text = element_text(size=sz[sz["elements"]=="axis",][[size]]),
          legend.title = element_text(size=sz[sz["elements"]=="text",][[size]]),
          legend.text = element_text(size=sz[sz["elements"]=="text",][[size]]),
          legend.position = "top",
          axis.text = element_text(size=sz[sz["elements"]=="axis",][[size]]),
          axis.title = element_text(size=sz[sz["elements"]=="axis",][[size]]),
          plot.caption = element_text(size = sz[sz["elements"]=="caption",][[size]],color = "red")
    )+
    labs(title=title,
         y=ylab,
         x=xlab,
         fill = "",
         caption=caption)+
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))
}

