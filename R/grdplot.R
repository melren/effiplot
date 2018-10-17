#' Gradient Plot
#'
#' Generate a flexible gradient plot (heatmap or bubble plot) for efficacy analysis
#'
#' @inheritParams boxplot
#' @inheritParams longplot
#' @param type controls the type of plot produced
#' @param ord column name for parameter to order color gradient by
#' @param ncol optional parameter to control number of facets to display
#' in a row if facet variable is provided, default is 3
#' @param leglab descriptive label for heat map legend
#' @return produces a gradient plot with optional facet parameters
#' @export
#'
#' @examples
#' grdplot(type = "bubble", data = AZA, x = "RANK", y = "ADY", facet = "CHRT", ord = "PDCR")
grdplot <- function(type = "heat", data, x, y, facet = NULL, ord, ncol = 3, subj = "USUBJID", title = "", xlab = "Subjects", ylab = "Study Day", leglab = "",caption = ""){
  # check argument parameters
  checkmate::assertChoice(type, c("heat", "bubble"))
  checkmate::assertDataFrame(data)
  checkmate::assertString(x)
  checkmate::assertString(y)
  checkmate::assertString(facet, null.ok = TRUE)
  checkmate::assertString(ord)
  checkmate::assertNumber(ncol, lower = 1, finite = TRUE)
  checkmate::assertString(subj)
  checkmate::assertString(title)
  checkmate::assertString(xlab)
  checkmate::assertString(ylab)
  checkmate::assertString(leglab)
  checkmate::assertString(caption)

  plotdata <- data

  ggplot(plotdata, aes_string(x=x, y=y))+
    {if(type=="bubble")geom_point(aes_string(size = ord, color = ord))}+
    {if(type=="bubble")guides(colour = guide_colourbar(order = 2),size = guide_legend(order = 1))}+
    {if(type=="bubble")scale_color_gradient(low = "red", high = "steelblue")}+
    {if(type=="heat")geom_tile(aes_string(fill = ord),color = "white")}+
    {if(type=="heat")scale_fill_gradient(low = "red", high = "steelblue", na.value = "white")}+
    {if(!is.null(facet))facet_wrap(stats::as.formula(paste("~", facet)), scales = "free_x", ncol=ncol)}+
    scale_x_discrete(breaks=plotdata[[x]],labels=plotdata[[subj]])+
    scale_y_continuous(trans = "reverse",
                       breaks = unique(plotdata[[y]]))+
    scale_size_continuous(labels=c(100,75,50,25,0),range = c(0.1, 3)) +

    labs(x = xlab,
         y = ylab,
         title = title,
         size = leglab,
         fill = leglab,
         caption=caption) +
    theme_bw()+
    theme(
      legend.key.size = unit(1,'lines'),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      plot.title = element_text(size=12),
      axis.title=element_text(size=10,face="bold"),
      axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 8),
      axis.line = element_line(size=1, colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      strip.background = element_rect(fill="lightgrey"),
      strip.text.x = element_text(size = 7),
      legend.position = "bottom",
      plot.caption = element_text(size = 8,color = "red")
    )

}
