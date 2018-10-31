#' Box Plot
#'
#' Generate a flexible box plot for endpoint analysis
#'
#' @inheritParams lineplot
#' @param group_by x-axis grouping factor variable (string)
#' @param color_by factor variable to color by (string)
#' @param color_lbl boolean to determine whether or not to display the column name used as color label, default is TRUE (logical)
#' @return produces a box plot with optional facet parameters
#' @export
#'
#' @examples
#' boxplot(
#'   data = AZA, group_by = "REGION", y = "PDCR", color_by = "BASEUA", facet = "TRT",
#'   show_n = FALSE, title = "Comparison of Treatment", xlab = "Region",
#'   ylab = "% Decrease in Ulcer Area",
#'   caption = "Colored by baseline ulcer area", size = "large"
#' )
boxplot <- function(data, group_by, y, color_by, color_lbl = TRUE, facet = NULL, aggr = "USUBJID", show_n = TRUE, title = "", xlab = "Visit Day", ylab = "AVAL", caption = "", size = "small"){
  # check argument parameters
  checkmate::assertDataFrame(data)
  checkmate::assertString(group_by)
  checkmate::assertString(y)
  checkmate::assertString(color_by)
  checkmate::assertLogical(color_lbl)
  checkmate::assertString(facet, null.ok = TRUE)
  checkmate::assertString(aggr)
  checkmate::assertLogical(show_n)
  checkmate::assertString(title)
  checkmate::assertString(xlab)
  checkmate::assertString(ylab)
  checkmate::assertString(caption)
  checkmate::assertChoice(size, choices = c("small", "large"))
  
  # Fetch sizes to use
  sz <- sizes[sizes[["func"]]=="boxplot", ]
  
  plotdata <- data
  plotdata[[aggr]] = factor(as.character(plotdata[[aggr]]))
  
  npatient <- data %>%
    dplyr::distinct_(color_by, aggr) %>%
    dplyr::group_by_(color_by) %>%
    dplyr::count_(color_by)
  
  # Dynamically populate legend labels
  cbreaks<-c(levels(plotdata[[color_by]]))
  clabels <- c()
  ccolors <- hue_pal()(length(cbreaks))
  lwidth = ifelse(size == "small", 15, 20)
  
  i = 1
  for(cat in cbreaks){
    if(show_n) {
      clabels[i] = stringr::str_wrap(paste(cat," (n=", npatient[npatient[[color_by]]==cat,]$n,")",sep=""), lwidth)
    } else {
      clabels[i] = cat
    }
    i = i+1
  }
  
  cname = ""
  if(color_lbl){
    cname = color_by
  }
  
  ggplot2::ggplot(data=plotdata,aes_string(x=group_by,y=y))+
    theme_classic()+
    {if(!is.null(facet))facet_wrap(stats::as.formula(paste("~", facet)))}+
    geom_boxplot(outlier.color = "white", outlier.size = 2)+
    stat_summary(fun.y=mean,color="black",geom="point",shape=18,size=3,show.legend=FALSE)+
    geom_jitter(aes_string(y=y, x=group_by,color=color_by), width=0.10)+
    scale_color_manual(name=cname,
                       values=ccolors,
                       breaks=cbreaks,
                       labels=clabels)+
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

