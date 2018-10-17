#' Time to Event Plot
#'
#' Generate a flexible KM plot with a risk table
#'
#' @param data a dataframe containing the input data to plot
#' @param group_by a column name containing factors to split the plot by
#' @param param the time-to-event parameter to plot (e.g. "OS")
#' @param paramcd the column name containing the parameter, default is "PARAMCD"
#' @param aval the column name containing the numeric analysis value, default is "AVAL"
#' @param cnsr the column name containing the censor value, default is "CNSR"
#' @param reverse boolean value controls whether the survival formula should be flipped, default is FALSE: Surv(CNSR - 1), if set to TRUE then behaves like Surv(1 - CNSR)
#' @param confint confidence interval to calculate with, default is 0.8
#' @param title custom plot title, passed as a string
#' @param xlab custom x-axis plot label, passed as string
#' @param ylab custom y-axis plot label, passed as a string
#'
#' @return produces a Kaplan-Meier type survival plot
#' @import ggplot2
#' @import survminer
#' @import survival
#' @importFrom stringr str_wrap
#' @export
#' @examples
#' tteplot(data = ATE, group_by = "CHRT", param = "TFUC", reverse = TRUE)
tteplot <- function(data, group_by, param, paramcd = "PARAMCD", aval = "AVAL", cnsr = "CNSR", reverse = FALSE, confint = 0.8, title = "", xlab = "", ylab = ""){
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

  censor = ifelse(reverse, paste0("1 - ", cnsr), paste0(cnsr, " - 1"))

  form <- stats::as.formula(paste0("Surv(", aval, ", ", censor, ") ~ ", group_by))
  substitute(survfit(form,
                     data = data[data[[paramcd]]==param,],
                     conf.type = "log-log",
                     conf.int = confint))
  fit<- eval(substitute(survfit(form, data = data[data[[paramcd]]==param,])))

  ggsurvplot(
    fit,                     # survfit object with calculated statistics.
    data = data[data[[paramcd]]==param,],
    fun="event",
    censor=TRUE,
    censor.shape="+",
    #xlim=c(0,135),  #xlim = c(0,fit$maxtime+1),
    ylim = c(0,1),
    xlab = xlab,    # customize X axis label.
    ylab = ylab,
    break.time.by = 5,       # break X axis in time intervals by 500.
    ########## theme #########,
    ggtheme = theme_classic(),    # theme_light(), # customize plot and risk table with a theme.
    font.x = 12,
    font.y = 12,
    font.tickslab = 9,
    ########## legend #########,
    #legend=c(0.1,0.75),
    legend.labs = stringr::str_wrap(levels(factor(data[[group_by]])), 15),
    font.legend = c(9,"plain", "black"),
    legend.title=" ",
    ########## risk table #########,
    risk.table = TRUE,       # show risk table.
    risk.table.y.text.col = T,# colour risk table text annotations.
    risk.table.y.text = F, #FALSE, # show bars instead of names in text annotations in legend of risk table.
    risk.table.fontsize=4,
    risk.table.height = 0.3,
    ########## plot title ######,
    title=title
  )
}
