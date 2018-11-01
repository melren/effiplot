# effiplot
R package to efficiently and flexibly produce plots for efficacy analysis in clinical trials

**Documentation:** [https://melren.github.io/effiplot/](https://melren.github.io/effiplot/)

### About

This package was developed to simplify the process of creating efficacy analysis graphs. The motivation is to make code more reusable and sharable so that less time and effort needs to be spent on formatting the plot itself and more can be spent on the actual data analysis. The plots are mainly built upon the `ggplot2` package but with code formatted and hidden to make the function calls more clear and user-friendly.

### Status & Installation: Development Version

This package is still under development and more functions will be added as they are cleaned.
Currently there is no stable release version. To obtain the development version of the package, execute the following block of code in an R console:
```r
devtools::install_github(
  repo = "melren/effiplot",
  ref = "master",
  upgrade_dependencies = FALSE
)
```
### Package Contents

---

#### Primary Plot Functions

Function Name | Description
-------------- | ---------------------------------------------------------------
`tteplot(...)` | Generate a flexible KM plot with a risk table
`lineplot(...)`| Generate a flexible longitudinal or spaghetti plot for endpoint analysis
`boxplot(...)` | Generate a flexible box plot with optional facets for endpoint analysis
`grdplot(...)` | Generate a flexible gradient plot (heatmap or bubble plot) for efficacy analysis

#### Additional Utility Functions

Function Name | Description
------------------------ | ---------------------------------------------------------------
`round(x,n)` | Round to nearest, ties away from zero (different from R default to ties to even) 
`LOCF(x)` | Last Observeration Carried Forward method to fill missing values in data series 
`WOCF(x)` | Worst Observation Carried Forward method to fill missing values in data series
`AOCF(x)` | Average Observation Carried Forward method to fill missing numeric values in data series, rounded to 2 decimal places.
`strToFileName(s,sep)` | String to File Name, parses string to make it more filename friendly 

#### Bonus Dummy Clinical Datasets
A few datasets are bundled to provide a source input for the example function calls. The data structure is based upon a diabetic foot ulcer study but is randomly generated and contains no real clinical information.

Dataset Name | Description
-------------- | ---------------------------------------------------------------
ASL | Subject level dataset
ATE | Time to event dataset
AZA | Longitudinal dataset
BM | Biomarker dataset

### Issues
> **Currently:** No specific known issues

While argument checks have been added and the functions have all been tested, the amount of stress/unit testing is still far from enough. Additionally, more code testing needs to be conducted on data from other studies. Please feel free to [report any issues](https://github.com/melren/effiplot/issues) encountered while using the package.
