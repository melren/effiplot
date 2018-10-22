#' Round
#'
#' This is a custom round function that rounds a number to the nearest whole number down
#' if the decimal is below 0.5 and up if it is above. Default R round behavior rounds decimals of 0.5 down
#' when the whole number is even and this function avoids that behavior.
#'
#' @param x the number to round
#' @param n the number of decimal places to round
#'
#' @return returns a rounded version of the value provided
#' @export
#'
#' @examples round(4.5, 0)
round = function(x, n) {
  # check argument parameters
  checkmate::assertNumber(x)
  checkmate::assertCount(n)
  
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  return(z*posneg)
}

#' Last Observation Carried Forward
#'
#' This a function to handle missing values in a dataframe column. It fills empty/missing rows values
#' with the immediate available previous value.
#'
#' @param x data series/vector with missing values to fill
#'
#' @return returns the same data series with missing values filled in with the preceding value
#' @export
#'
#' @examples LOCF(c(1, 2, NA, 5, NA, 9, 10))
LOCF <- function(x){
  # check argument parameter
  if(!is.null(dim(x)) & !(1 %in% dim(x)))
    stop("Argument 'x' must be single dimension")
  
  y=matrix(NA,ncol=1,nrow=length(x))
  for (i in 1: length(x)){
    y[i,]=ifelse(is.na(x[i])==F,x[i],y[i-1])
  }
  return(y)
}

#' Worst Observation Carried Forward
#'
#' This a function to handle missing values in a dataframe column. It fills empty/missing rows values
#' with the worst(lowest) available alpha/numeric value.
#'
#' @inheritParams LOCF
#'
#' @return returns the same data series with missing values filled in with the lowest alpha/numeric value
#' @export
#'
#' @examples WOCF(c(1, 2, NA, 5, NA, 9, 10))
WOCF <- function(x) {
  # check argument parameter
  if(!is.null(dim(x)) & !(1 %in% dim(x)))
    stop("Argument 'x' must be single dimension")
  
  y=matrix(NA,ncol=1,nrow=length(x))
  for (i in 1: length(x)){
    y[i,]=ifelse(is.na(x[i])==F,x[i],min(x, na.rm = T))
  }
  return(y)
}

#' Average Observation Carried Forward
#'
#' This a function to handle missing values in a dataframe column. It fills empty/missing rows values
#' with the average numeric value.
#'
#' @inheritParams LOCF
#'
#' @return returns the same data series with missing values filled in with the average numeric value rounded to 2 decimal places
#' @export
#'
#' @examples AOCF(c(1, 2, NA, 5, NA, 9, 10))
AOCF <- function(x) {
  # check argument parameter
  checkmate::assertNumeric(x)
  if(!is.null(dim(x)) & !(1 %in% dim(x)))
    stop("Argument 'x' must be single dimension")
  
  y=matrix(NA,ncol=1,nrow=length(x))
  for (i in 1: length(x)){
    y[i,]=round(ifelse(is.na(x[i])==F,x[i],mean(x, na.rm = T)), 2)
  }
  return(y)
}
