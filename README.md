# Interactive R/Shiny web app for detecting outliers in time series data

## General idea
This shiny-application was developed to facilitate working with time-series data, especially handling outliers. Although primarly made and used for handling microscopic video-data created in the Pertz lab, the application handles any kind of time-series data. There are currently four different modules that provide different tools that are suited for different kinds of time-series-data outliers.
Current modules are:
* Quantile Trimming (author: Maciej Dobrzynski)
* Isolation Tree
* Rolling Window
* Interactive PCA

## Running the app from the server
The app can be accessed here:
https://outlierapp.shinyapps.io/application/

## Running the app locally
Alternatively, after downloading the code, the app can be run within RStudio. Open `server.R` or `ui.R` file, then click "Run App" button with green triangle in the upper right corner of the window with code open.

Following packages need to be installed in order to run the app locally:

* shiny
* shinydashboard
* data.table
* ggplot2
* gplots
* dendextend
* RColorBrewer
* imputeTS
* gridExtra
* plotly
* tsfeaturex
* ggfortify
* reshape2
* shinyjs
* shinyBS

Install packages using `install.packages('name_of_the_package_from_the_list_above')` command in RStudio command line.
  
Install package 'tsfeaturex' using `devtools::install_github("nelsonroque/tsfeaturex")` command in RStudio command line.

```
install.packages(c("shiny", "shinydashboard",
		"data.table",
		"ggplot2", "gplots", "gridExtra",
		"dendextend", "RColorBrewer",
		"imputeTS", "plotly", "tsfeaturex", 
		"ggfortify", "reshape2", "shinyjs",
		"shinyBS")) 
```

## Input file
The app recognizes CSV (comma-separated values) files where data columns are separated by a comma, and floating point numbers use a dot (full-stop). Data should be arranged in a long format, where time-series (tracks) are arranged one after another. The wide format where individual tracks are arranged in neighboring columns is not supported.

The first row should include column headers. The input CSV file should contain at least these three columns:

* Unique identifier of time series, i.e. a track label
* Time points
* Time-varying variable
* Grouping column, e.g. treatment (optional)


| Group |  ID  | Time | Meas1 |
|-------|------|------|-------|
| gr1   | 1_1  |  1   | 3.3   |
| gr1   | 1_1  |  2   | 2.1   |
| gr1   | 1_1  |  4   | 4.3   |
|-------|------|------|-------|
| gr1   | 1_2  |  1   | 2.8   |
| gr1   | 1_2  |  2   | 1.9   |
| gr1   | 1_2  |  3   | 1.7   |
| gr1   | 1_2  |  4   | 2.2   |
|-------|------|------|-------|
| gr2   | 2_1  |  1   | 5.1   |
| gr2   | 2_1  |  2   | 5.4   |
| gr2   | 2_1  |  3   | 5.3   |

In case of multi-variate time series, additional columns with variables can be added in the input. Then, GUI allows for choosing a single or a combination of two variables to display.

## Thesis "Interactive Time-series Outlier Detection": 
The thesis centering around the development of this application can be accessed here:
https://www.researchgate.net/publication/333845151_Interactive_Time-series_Outlier_Detection
