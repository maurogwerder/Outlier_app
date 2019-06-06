# Interactive R/Shiny web app for detecting outliers in time series data

## Running the app from the server
The app can be accessed here:
https://pertzlab.unibe.ch:3838/Outlier_app/ (UniBe VPN only!)

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

Install packages using `install.packages('name_of_the_package_from_the_list_above')` command in RStudio command line.

```
install.packages(c("shiny", "shinydashboard",
		"data.table",
		"ggplot2", "gplots", "gridExtra",
		"dendextend", "RColorBrewer",
		"imputeTS")) 
```

## Input file
The app recognizes CSV (comma-separated values) files where data columns are separated by a comma, and floating point numbers use a dot (full-stop). Data should be arranged in a long format, where time-series (tracks) are arranged one after another. The wide format where individual tracks are arranged in neighboring columns is not supported.

The first row should include column headers. The input CSV file should contain at least these three columns:

* Unique identifier of time series, i.e. a track label
* Time points
* Time-varying variable
* Grouping column, e.g. treatment


| Group | ID | Time | Meas1 |
|-------|----|------|-------|
| gr1   | 1  |  1   | 3.3   |
| gr1   | 1  |  2   | 2.1   |
| gr1   | 1  |  4   | 4.3   |
|-------|----|------|-------|
| gr1   | 2  |  1   | 2.8   |
| gr1   | 2  |  2   | 1.9   |
| gr1   | 2  |  3   | 1.7   |
| gr1   | 2  |  4   | 2.2   |
|-------|----|------|-------|
| gr2   | 1  |  1   | 5.1   |
| gr2   | 1  |  2   | 5.4   |
| gr2   | 1  |  3   | 5.3   |


In case of multi-variate time series, additional columns with variables can be added in the input. Then, GUI allows for choosing a single or a combination of two variables to display.

