#' Line Plot
#'
#' Generate a flexible longitudinal/spaghetti plot for endpoint analysis with options to facet and add marker lines
#'
#' @inheritParams tteplot
#' @param x column name for x-axis parameter (string)
#' @param y column name for y-axis parameter (string)
#' @param facet optional parameter, column name containing a subgrouping factor to facet the plot by (string)
#' @param stat optional parameter, type of summary statistic to compute on y-variable, choose "mean" or "median" if y is not already summary-level data (string)
#' @param subj column name for unique subject identifier used to compute counts in legend, default is "USUBJID" (string)
#' @param log_y optional parameter, boolean for whether to use a log scale for y-axis, default is FALSE (logical)
#' @param symb optional parameter to shape markers by a factor type column (string)
#' @param markers optional parameter, list of special interest x-axis values to display vertical dashed marker lines (numeric list/series)
#' @param max_lvl optional parameter, maximum levels group_by can have before legend is hidden (integer)
#'
#' @return produces a line plot with study endpoint measurements over time
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @importFrom stringr str_wrap
#'
#' @export
#' @examples
#' lineplot(data = AZA, x = "ADY", y = "AVAL", group_by = "USUBJID", facet = "CHRTGRP",
#'   symb = "EPOCH", markers = c(45, 85), ylab = "Ulcer Area")
#' lineplot(data = AZA, x = "ADY", y = "PDCR", group_by = "USUBJID", facet = "CHRTGRP", log_y = TRUE,
#'   symb = "EPOCH", ylab = "Percent Decrease in Ulcer Area")
#' lineplot(data = AZA, x = "ADY", y = "PDCR", group_by = "CHRT",
#'   stat = "mean", ylab = "Mean % Decrease in Area", size = "large")
lineplot <- function(data, x, y, group_by = subj, facet = NULL, stat = NULL, subj = "USUBJID", log_y = FALSE, title = "", xlab = "Visit Day", ylab = "AVAL", symb = NULL, markers = NULL, caption = "", size = "small", max_lvl = 8){
  # check argument parameters
  checkmate::assertDataFrame(data)
  checkmate::assertString(x)
  checkmate::assertString(y)
  checkmate::assertString(group_by)
  checkmate::assertString(facet, null.ok = TRUE)
  checkmate::assertChoice(stat, choices = c("mean", "median", "min", "max"), null.ok = TRUE)
  checkmate::assertString(subj)
  checkmate::assertLogical(log_y)
  checkmate::assertString(title)
  checkmate::assertString(xlab)
  checkmate::assertString(ylab)
  checkmate::assertString(symb, null.ok = TRUE)
  checkmate::assertNumeric(markers, null.ok = TRUE)
  checkmate::assertString(caption)
  checkmate::assertNumber(max_lvl, lower = 1, finite = TRUE)
  checkmate::assertChoice(size, choices = c("small", "large"))
  
  # Transform data to avoid log(0) values
  if(log_y){
    df <- data %>%
      dplyr::mutate(mod_y = !!as.name(y) + 0.01)
    data[[y]] <- df[["mod_y"]]
  }
  
  plotdata <- data
  
  # Fetch sizes to use
  sz <- sizes[sizes[["func"]]=="lineplot", ]
  
  if(!is.null(stat)){
    exprs=paste0(stat,"(",y,", na.rm = TRUE)")
    
    if(is.null(facet)){
      statdata <- data %>%
        dplyr::group_by_(group_by, x) %>%
        dplyr::summarise(y = eval(parse(text=exprs))) %>%
        dplyr::ungroup()
      
      plotdata <- plotdata %>%
        dplyr::left_join(statdata, by = c(group_by, x))
    } else {
      statdata <- data %>%
        dplyr::group_by_(group_by, facet, x) %>%
        dplyr::summarise(y = eval(parse(text=exprs))) %>%
        dplyr::ungroup()
      
      plotdata <- plotdata %>%
        dplyr::left_join(statdata, by = c(group_by, facet, x))
    }
    
    y = "y"
  }
  
  plotdata[[subj]] = factor(as.character(plotdata[[subj]]))
  
  npatient <- data %>%
    dplyr::distinct_(group_by, subj) %>%
    dplyr::group_by_(group_by) %>%
    dplyr::count_(group_by)
  
  
  # Dynamically populate legend labels
  gbreaks<-c(levels(plotdata[[group_by]]))
  glabels <- c()
  glines<-c(1:length(gbreaks))
  gcolors <- hue_pal()(length(gbreaks))
  gwidths<-rep(0.8,length(gbreaks))
  lwidth = ifelse(size == "small", 15, 20)
  
  i = 1
  for(cat in gbreaks){
    glabels[i] = stringr::str_wrap(paste(cat," (n=", npatient[npatient[[group_by]]==cat,]$n,")",sep=""), lwidth)
    i = i+1
  }
  
  lvls = length(levels(plotdata[[group_by]]))
  
  ggplot2::ggplot(data=plotdata,aes_string(x=x,y=y,group=group_by))+
    theme_classic()+
    {if(!is.null(facet))facet_wrap(stats::as.formula(paste("~",facet)),nrow=length(c(levels(plotdata[[facet]]))))}+
    geom_line(aes_string(color=group_by, linetype = group_by))+
    {if(is.null(symb))geom_point(aes_string(color=group_by))}+
    {if(!is.null(symb))geom_point(aes_string(color=group_by,shape=symb))}+
    scale_color_manual(values=gcolors,
                       breaks=gbreaks,
                       labels=glabels)+
    scale_linetype_manual(values=glines,
                          breaks=gbreaks,
                          labels=glabels)+
    scale_size_manual(values=gwidths,
                      breaks=gbreaks,
                      labels=glabels)+
                      {if(lvls > max_lvl)guides(colour=F, linetype=F)}+
    theme(plot.title = element_text(size=sz[sz["elements"]=="title",][[size]]),
          strip.text = element_text(size=sz[sz["elements"]=="text",][[size]]),
          legend.key.size = unit(2,'lines'),
          legend.position = ifelse(lvls <= max_lvl & !is.null(symb), "right", "top"),
          legend.title=element_blank(),
          legend.text = element_text(size = sz[sz["elements"]=="text",][[size]]),
          axis.text = element_text(size=sz[sz["elements"]=="axis",][[size]]),
          axis.title = element_text(size=sz[sz["elements"]=="axis",][[size]]),
          plot.caption = element_text(size = sz[sz["elements"]=="caption",][[size]],color = "red")
    )+
    {if(!is.null(markers))geom_vline(xintercept=markers,color="black",linetype=2)}+
    scale_x_continuous(breaks=seq(min(plotdata[[x]], na.rm = TRUE),max(plotdata[[x]], na.rm = TRUE), 7)) +
    {if(log_y)scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000, 10000, 100000, 1000000))}+
    labs(title=title,
         y=ylab,
         x=xlab,
         caption=caption)
}

