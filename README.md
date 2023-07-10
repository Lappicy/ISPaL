# ISPaL

This package was created for forecasting monthly values using three models (PAR, PARX and RIDGE) with up to three exogenous variables (with the PARX and RIDGE). Although it was designed to be used with streamflow values, there are no reasons to why it must be limited to such.

## Practical Example

To show how to run the main function within this package, named *forecast.PAR.PARX.RIDGE()*, a practical example was made available. This example uses four monthly streamflow time series from the Brazilian hydropower sector (four subsystems), shown in the image below, and three climatic indicators as exogenous variables (named U1, SST2 and NINO3). One may choose to download such data from the foldedr **Example application**, but this exact same data is available through the **R** package.

![alt text](https://github.com/Lappicy/ISPaL/blob/main/Example%20application/Images%20created/Map%20subsistem%20centroid.png)

### Download the package in R/Rstudio

To download the package through **R**, you must have downloaded the *devtools* package (https://cran.r-project.org/web/packages/devtools/index.html). If using windows as an OS (Operating System), it is needed to first download **Rtools**, from the website (https://cran.r-project.org/bin/windows/Rtools/).

To install and open the *devtools* package using the command line in R, run the following codes:
```r
  install.packages("devtools")
  library(devtools)
```
If the package (*devtools*) ir properly installed and opened, you must then install, and load, the *ISPaL* package through the github link with the code:
```r
  devtools::install_github("Lappicy/ISPaL")
  library(ISPaL)
```
### Acessing the data

The streamflow and climatic data used in this practical example can be acesses through the commands:
```r
  data(StreamflowEnergy)
  data(ClimaticInfo)
```
### Running the forecasts

To run the forecast, only one function is needed, that being *forecast.PAR.PARX.RIDGE()*. The arguments passed are the data.frame with the variables of interest (*Var.Y=*), a data.frame with the exogenous variables (*Var.X=*), the forecasting horizon (*forecast.lag*), months used (*forecast.months*) and the start and end dates for the calibration, testing and validation periods (*period.calib=*, *period.test=* and *period.valid=*). The following code runs the main function with the arguments being explicitely defined and saves the output into an object called **forecast.results**.
```r
  forecast.results <-
    forecast.PAR.PARX. RIDGE(var.Y = StreamflowEnergy,
                            var.X = ClimaticInfo,
                            forecast.lag = 1:6,
                            forecast.months = 1:12,
                            period.calibration = c(1949, 1990),
                            period.test = c(1991, 2010),
                            period.validation = c(2011, 2021))
```
### Acessing the forecasts

The output of the function is a list with four tables. The first table (*Forecast.table*) has all the observed and forecasted values for each model. The second and third table (*Error.table* and *All.error.table*) are simillar between each other, having the coefficients used for each exogenous variable as well as the **NSE** and **KGE** metrics (with its individuals components). The difference between both tables is that the level of detail in each one. Lastly, the final table (*Lambda.table*) has the lambda values used in the ridge regression for each time series, month and lag used.
```r
  Forecast.table <- forecast.results[[1]]
  Error.table <- forecast.results[[2]]
  All.error.table <- forecast.results[[3]]
  Lambda.table <- forecast.results[[4]]
```
 ### Tables 


 ### Possible analysis

 Different analysis may be done with the results. Below we show two possibilities - one being the density plots showing the difference between the metrics for each model proposed (PAR without any climatic indicators, PARX and RIDGE using climatic indicicators).

![alt text](https://github.com/Lappicy/ISPaL/blob/main/Example%20application/Images%20created/Climatic%20value%20density%20plot%20and%20boxplot.png)

 The other analysis may be spatial, shown in the map below.

 ![alt text](https://github.com/Lappicy/ISPaL/blob/main/Example%20application/Images%20created/Climatic%20gain%20SUBSYSTEM%20KGE.png)
