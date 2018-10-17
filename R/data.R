#' Dummy ASL Dataset for Package Usage Examples
#'
#' Dummy Dataset of Analysis Dataset for Subject Level records
#'
#' @format A data frame based off a diabetic foot ulcer study with 60 rows and 14 variables:
#' \describe{
#'   \item{USUBJID}{unique subject identifier}
#'   \item{ARM}{treatment arm}
#'   \item{TRT}{active vs placebo label}
#'   \item{CHRT}{treatment arms of interest grouping}
#'   \item{CHRTGRP}{pooled treatment arms}
#'   \item{BAGE}{baseline age}
#'   \item{AGE65}{categorical age below 65 vs. not}
#'   \item{REGION}{study region}
#'   \item{BASEUA}{cutoff category for baseline ulcer area}
#'   \item{BASE}{baseline ulcer area in cm^2}
#'   \item{WEIGHT}{baseline weight in kg}
#'   \item{HBA1C}{baseline HBA1C count}
#'   \item{SAFFL}{safety evaluable flag}
#'   \item{ITTFL}{intent-to-treat flag}
#'   ...
#' }
"ASL"

#' Dummy ATE Dataset for Package Usage Examples
#'
#' Dummy Dataset of Analysis Dataset for Time to Event records
#'
#' @format A data frame based off a diabetic foot ulcer study with 120 rows and 19 variables:
#' \describe{
#'   \item{USUBJID}{unique subject identifier}
#'   \item{PARAMCD}{parameter code for time to event}
#'   \item{PARAM}{full description of time to event parameter}
#'   \item{CNSR}{censorship variable}
#'   \item{AVAL}{analysis value}
#'   \item{AVALU}{analysis value unit}
#'   \item{ARM}{treatment arm}
#'   \item{TRT}{active vs placebo label}
#'   \item{CHRT}{treatment arms of interest grouping}
#'   \item{CHRTGRP}{pooled treatment arms}
#'   \item{BAGE}{baseline age}
#'   \item{AGE65}{categorical age below 65 vs. not}
#'   \item{REGION}{study region}
#'   \item{BASEUA}{cutoff category for baseline ulcer area}
#'   \item{BASE}{baseline ulcer area in cm^2}
#'   \item{WEIGHT}{baseline weight in kg}
#'   \item{HBA1C}{baseline HBA1C count}
#'   \item{SAFFL}{safety evaluable flag}
#'   \item{ITTFL}{intent-to-treat flag}
#'   ...
#' }
"ATE"

#' Dummy AZA Dataset for Package Usage Examples
#'
#' Dummy Dataset of Analysis Dataset for longitudinal endpoint records
#'
#' @format A data frame based off a diabetic foot ulcer study with 1080 rows and 21 variables:
#' \describe{
#'   \item{USUBJID}{unique subject identifier}
#'   \item{ADY}{analysis (study) day}
#'   \item{AVAL}{analysis value (ulcer area size)}
#'   \item{ARM}{treatment arm}
#'   \item{TRT}{active vs placebo label}
#'   \item{CHRT}{treatment arms of interest grouping}
#'   \item{CHRTGRP}{pooled treatment arms}
#'   \item{BAGE}{baseline age}
#'   \item{AGE65}{categorical age below 65 vs. not}
#'   \item{REGION}{study region}
#'   \item{BASEUA}{cutoff category for baseline ulcer area}
#'   \item{BASE}{baseline ulcer area in cm^2}
#'   \item{WEIGHT}{baseline weight in kg}
#'   \item{HBA1C}{baseline HBA1C count}
#'   \item{SAFFL}{safety evaluable flag}
#'   \item{ITTFL}{intent-to-treat flag}
#'   \item{CHG}{change in ulcer area from baseline}
#'   \item{PCHG}{percent change in ulcer area from baseline}
#'   \item{PDCR}{percent decrease in ulcer area from baseline}
#'   \item{EPOCH}{study epoch}
#'   \item{RANK}{dummy ranking for heatmap plots}
#'   ...
#' }
"AZA"

#' Dummy BM Dataset for Package Usage Examples
#'
#' Dummy Dataset of Analysis Dataset for biomarker records
#'
#' @format A data frame based off a diabetic foot ulcer study with 720 rows and 10 variables:
#' \describe{
#'   \item{USUBJID}{unique subject identifier}
#'   \item{CHRT}{treatment arms of interest grouping}
#'   \item{CHRTGRP}{pooled treatment arms}
#'   \item{BASEUA}{cutoff category for baseline ulcer area}
#'   \item{ANALYTE}{name of biomarker analyte}
#'   \item{ADY}{analysis (study) day}
#'   \item{AVAL}{analysis value (biomarker lab value)}
#'   \item{BASE}{baseline ulcer area in cm^2}
#'   \item{CHG}{change in biomarker from baseline}
#'   \item{PCHG}{percent change in biomarker from baseline}
#'   ...
#' }
"BM"
