#' Time to Event Plot
#'
#' Generate a flexible KM plot with a risk table
#'
#' @param data a dataframe containing the input data to plot (dataframe)
#' @param group_by a column name containing a subgrouping factor to split the plot by (string)
#' @param param the time-to-event parameter to plot (e.g. "OS") (string)
#' @param paramcd the column name containing the parameter, default is "PARAMCD" (string)
#' @param aval the column name containing the numeric analysis value, default is "AVAL" (string)
#' @param cnsr the column name containing the censor value, default is "CNSR" (string)
#' @param reverse boolean value controls whether the survival formula should be flipped, default is FALSE: Surv(CNSR - 1), if set to TRUE then behaves like Surv(1 - CNSR) (logical)
#' @param confint confidence interval to calculate with, default is 0.8 (float between 0 and 1)
#' @param title optional parameter, custom plot title (string)
#' @param xlab optional parameter, custom x-axis plot label (string)
#' @param ylab optional parameter, custom y-axis plot label (string)
#' @param caption optional string to include as a caption on bottom of plot (string)
#' @param size optional string to specify font size schema ("small" for PDF usage or "large" for Shiny apps)
#' to use for plot, default is "small" (string)
#'
#' @return produces a Kaplan-Meier type survival plot
#' @import ggplot2
#' @import survminer
#' @import survival
#' @importFrom stringr str_wrap
#' @export
#' @examples
#' tteplot(
#'   data = ATE, group_by = "CHRT", param = "TFUC", reverse = TRUE,
#'   xlab = "Day on Study", ylab = "Proportion of Ulcer Closure"
#' )
tteplot <- function(data, group_by, param, paramcd = "PARAMCD", aval = "AVAL", cnsr = "CNSR", reverse = FALSE, confint = 0.8, title = "", xlab = "", ylab = "", caption = "", size = "small"){
  # check argument parameters
  checkmate::assertDataFrame(data)
  checkmate::assertString(group_by)
  checkmate::assertString(param)
  checkmate::assertString(paramcd)
  checkmate::assertString(aval)
  checkmate::assertString(cnsr)
  checkmate::assertLogical(reverse)
  checkmate::assertNumber(confint, lower = 0, upper = 1, finite = TRUE)
  checkmate::assertString(title)
  checkmate::assertString(xlab)
  checkmate::assertString(ylab)
  checkmate::assertString(caption)
  checkmate::assertChoice(size, choices = c("small", "large"))
  
  # Fetch sizes to use
  sz <- sizes[sizes[["func"]]=="tteplot", ]
  
  censor = ifelse(reverse, paste0("1 - ", cnsr), paste0(cnsr, " - 1"))
  
  form <- stats::as.formula(paste0("Surv(", aval, ", ", censor, ") ~ ", group_by))
  
  fit <- do.call(survfit, args = list(formula = form, data = data[data[[paramcd]]==param,], conf.type = "log-log", conf.int = confint))
  
  ggsurvplot(
    fit,                     # survfit object with calculated statistics.
    data = data[data[[paramcd]]==param,],
    fun="event",
    censor=TRUE,
    censor.shape="+",
    ylim = c(0,1),
    xlab = xlab,    # customize X axis label.
    ylab = ylab,
    break.time.by = 5,       # break X axis in time intervals by 500.
    ########## theme #########,
    ggtheme = theme_classic(),    # theme_light(), # customize plot and risk table with a theme.
    font.x = sz[sz["elements"]=="axis",][[size]],
    font.y = sz[sz["elements"]=="axis",][[size]],
    font.tickslab = sz[sz["elements"]=="text",][[size]],
    ########## legend #########,
    #legend=c(0.1,0.75),
    legend.labs = stringr::str_wrap(levels(data[[group_by]]), 15),
    font.legend = c(sz[sz["elements"]=="text",][[size]],"plain", "black"),
    legend.title=" ",
    font.title = c(sz[sz["elements"]=="title",][[size]], "plain", "black"),
    ########## risk table #########,
    risk.table = TRUE,       # show risk table.
    risk.table.y.text.col = T,# colour risk table text annotations.
    risk.table.y.text = F, #FALSE, # show bars instead of names in text annotations in legend of risk table.
    risk.table.fontsize=sz[sz["elements"]=="risk",][[size]],
    risk.table.height = sz[sz["elements"]=="riskh",][[size]],
    ########## plot title ######,
    title=title,
    caption = caption,
    font.caption = c(sz[sz["elements"]=="caption",][[size]], "plain", "red")
  )
}

