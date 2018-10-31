#' Gradient Plot
#'
#' Generate a flexible gradient plot (heatmap or bubble plot) for efficacy analysis
#'
#' @inheritParams lineplot
#' @param type use either "heat" or "bubble", controls the type of plot produced (string)
#' @param ord column name for factor level parameter to order color gradient by (string)
#' @param ord_lbl column name for parameter that contains x-axis labels (ord column labels) (string)
#' @param reverse optional boolean parameter for flipping the size of the bubbles
#' and for "negative" measurements like decrease/percent decrease (logic)
#' @param ncol optional parameter to control number of facets to display
#' in a row if facet variable is provided, must be at least 1, default value is 3 (integer)
#' @param leglab descriptive label for heat map legend (string)
#' @return produces a gradient plot with optional facet parameters
#' @export
#'
#' @examples
#' grdplot(
#'   type = "bubble", data = AZA, x = "RANK", y = "ADY", facet = "CHRTGRP", ord = "PDCR",
#'   reverse=TRUE, ncol = 2, xlab = "Subject ID", leglab = "% Decrease in UA"
#' )
#'
#' grdplot(
#'   type = "heat", data = AZA, x = "RANK", y = "ADY", facet = "CHRTGRP", ord = "PDCR",
#'   reverse=TRUE, ncol = 2, xlab = "Subject ID", leglab = "% Decrease in UA"
#' )
#'
#' grdplot(
#'   type = "bubble", data = AZA, x = "RANK", y = "ADY", facet = "CHRTGRP", ord = "AVAL",
#'   ncol = 2, xlab = "Subject ID", leglab = "Ulcer Area"
#' )
grdplot <- function(type = "heat", data, x, y, facet = NULL, ord, ord_lbl = "USUBJID", reverse = FALSE, ncol = 3, title = "", xlab = "Subjects", ylab = "Study Day", leglab = "", caption = "", size = "small"){
  # check argument parameters
  checkmate::assertChoice(type, c("heat", "bubble"))
  checkmate::assertDataFrame(data)
  checkmate::assertString(x)
  checkmate::assertString(y)
  checkmate::assertString(facet, null.ok = TRUE)
  checkmate::assertString(ord)
  checkmate::assertString(ord_lbl)
  checkmate::assertLogical(reverse)
  checkmate::assertNumber(ncol, lower = 1, finite = TRUE)
  checkmate::assertString(title)
  checkmate::assertString(xlab)
  checkmate::assertString(ylab)
  checkmate::assertString(leglab)
  checkmate::assertString(caption)
  checkmate::assertChoice(size, choices = c("small", "large"))
  
  # Fetch sizes to use
  sz <- sizes[sizes[["func"]]=="grdplot", ]
  
  # Create a column for reversed values for decrease type of plots
  plotdata <- data %>%
    dplyr::mutate(ord2 = -!!as.name(ord))
  
  glabels = round(seq(min(plotdata[[ord]], na.rm = TRUE), max(plotdata[[ord]], na.rm = TRUE), by= max(plotdata[[ord]], na.rm = TRUE)/4),0)
  gbreaks = glabels
  if(reverse){
    glabels = round(seq(max(plotdata[[ord]], na.rm = TRUE), min(plotdata[[ord]], na.rm = TRUE), by= -max(plotdata[[ord]], na.rm = TRUE)/4),0)
    gbreaks = -1*glabels
  }
  
  ggplot(plotdata, aes_string(x=x, y=y))+
  {if(type=="bubble" & reverse)geom_point(aes_string(size = "ord2", color = ord))}+
  {if(type=="bubble" & !reverse)geom_point(aes_string(size = ord, color = ord))}+
  {if(type=="bubble")guides(colour = guide_colourbar(order = 2, title = ""), size = guide_legend(order = 1))}+
  {if(type=="bubble")scale_color_gradient(low = "red", high = "steelblue")}+
  {if(type=="heat")geom_tile(aes_string(fill = ord),color = "white")}+
  {if(type=="heat")scale_fill_gradient(low = "red", high = "steelblue", na.value = "white")}+
  {if(!is.null(facet))facet_wrap(stats::as.formula(paste("~", facet)), scales = "free_x", ncol=ncol)}+
    scale_x_discrete(breaks=plotdata[[x]],labels=plotdata[[ord_lbl]])+
    scale_y_continuous(trans = "reverse",
                       breaks = unique(plotdata[[y]]))+
    scale_size_continuous(breaks = gbreaks, labels = glabels, range = c(0.1, 3))+
    labs(x = xlab,
         y = ylab,
         title = title,
         size = leglab,
         fill = leglab,
         caption=caption) +
    theme_bw()+
    theme(
      legend.key.size = unit(sz[sz["elements"]=="legend",][[size]],'lines'),
      legend.title = element_text(size = sz[sz["elements"]=="text",][[size]]),
      legend.text = element_text(size = sz[sz["elements"]=="text",][[size]]),
      plot.title = element_text(size=sz[sz["elements"]=="title",][[size]]),
      axis.title=element_text(size=sz[sz["elements"]=="text",][[size]],face="bold"),
      axis.text.x = element_text(size = sz[sz["elements"]=="axis",][[size]], angle = 90, hjust = 1),
      axis.text.y = element_text(size = sz[sz["elements"]=="axis",][[size]]),
      axis.line = element_line(size=1, colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      strip.background = element_rect(fill="lightgrey"),
      strip.text.x = element_text(size = sz[sz["elements"]=="caption",][[size]]),
      legend.position = "bottom",
      plot.caption = element_text(size = sz[sz["elements"]=="caption",][[size]],color = "red")
    )
  
}

