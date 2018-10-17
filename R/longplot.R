#' Longitudinal Plot
#'
#' Generate a flexible longitudinal plot for endpoint analysis
#'
#' @inheritParams tteplot
#' @param x column name for x-axis parameter
#' @param y column name for y-axis parameter
#' @param subj column name for unique subject identifier, default is "USUBJID"
#' @param stat type of summary statistic to compute on y-variable, choose "mean" or "median" if y is not already summary-level data
#' @param markers optional parameter, list of special interest x-axis values to display vertical dashed marker lines
#' @param caption optional string to include as a caption on bottom of plot
#' @return produces a line plot with study endpoint measurements over time
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @importFrom stringr str_wrap
#' @export
#' @examples
#' longplot(data = AZA, group_by = "CHRT", x = "ADY", y = "AVAL", stat = "mean", markers = c(45,85))
longplot <- function(data, group_by, x, y, subj = "USUBJID", stat = NULL, markers = NULL, title = "", xlab = "Days", ylab = "AVAL", caption = ""){
  # check argument parameters
  checkmate::assertDataFrame(data)
  checkmate::assertString(group_by)
  checkmate::assertString(x)
  checkmate::assertString(y)
  checkmate::assertString(subj)
  checkmate::assertChoice(stat, c("mean", "median"), null.ok = TRUE)
  checkmate::assertNumeric(markers, null.ok = TRUE)
  checkmate::assertString(title)
  checkmate::assertString(xlab)
  checkmate::assertString(ylab)
  checkmate::assertString(caption)

  plotdata <- data

  if(!is.null(stat)){
    exprs=paste0(stat,"(",y,")")

    plotdata <- data %>%
      dplyr::group_by_(group_by, x) %>%
      dplyr::summarise(y = eval(parse(text=exprs)))

    y = "y"
  }

  plotdata[[group_by]] = factor(as.character(plotdata[[group_by]]))

  npatient <- data %>%
    dplyr::distinct_(group_by, subj) %>%
    dplyr::count_(group_by)

  # Dynamically populate legend labels
  gbreaks<-c(levels(factor(plotdata[[group_by]])))
  glabels <- c()
  glines<-c(1:length(gbreaks))
  gcolors <- hue_pal()(length(gbreaks))
  gwidths<-rep(0.8,length(gbreaks))

  i = 1
  for(cat in levels(factor(data[[group_by]]))){
    glabels[i] = stringr::str_wrap(paste(cat," (n=", nrow(npatient[npatient[[group_by]]==cat,]),")",sep=""),15)
    i = i+1
  }

  lvls = length(levels(plotdata[[group_by]]))
  ggplot2::ggplot(data = plotdata, aes_string(x = x, y = y))+
    theme_classic()+
    geom_point(aes_string(color=group_by))+
    geom_line(aes_string(color=group_by, linetype = group_by))+
    scale_color_manual(values=gcolors,
                      breaks=gbreaks,
                      labels=glabels)+
    scale_linetype_manual(values=glines,
                          breaks=gbreaks,
                          labels=glabels)+
    scale_size_manual(values=gwidths,
                      breaks=gbreaks,
                      labels=glabels)+
    theme(plot.title = element_text(size=16),
          legend.position = ifelse(lvls > 8,"none","top"),
          legend.key.size = unit(3,'lines'),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          axis.text = element_text(size=9),
          axis.title = element_text(size=10),
          plot.caption = element_text(size = 8,color = "red")
    )+
    {if(!is.null(markers))geom_vline(xintercept=markers,color="black",linetype=2)}+
    labs(title=title,
         y=ylab,
         x=xlab,
         caption = caption
    )
}
