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
    forecast.PAR.PARX.RIDGE(var.Y = StreamflowEnergy,
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

The first and last lines of each of the four tables may be seen below.

*Forecast.table*:

| K  | Lag  | Month | Obs | SimPAR | SimPARX | SimRIDGE | SimPARX0 | SimPARX1 | SimPARX2 | SimPARX3 |
| :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: |
| 1 | 1 | 1 | 2720.3141 | 2309.7943 | 2298.3708 | 2287.6540 | 2311.3535 | 2480.7898 | 2298.3708 | 2252.7486 |
| 1 | 1 | 1 | 1350.0932 | 1785.8570 | 1784.4341 | 1704.5644 | 1780.2105 | 1537.3510 | 1784.4341 | 1786.6621 |
| 1 | 1 | 1 | 2056.2610 | 1937.3367 | 1937.2465 | 2016.5013 | 1933.7735 | 2182.3509 | 1937.2465 | 1963.6256 |
| ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... |
| 4 | 6 | 12 | 2585.1120 | 3283.3108 | 3231.6469 | 3337.2106 | 3270.5714 | 3202.9450 | 3193.2188 | 3359.0699 |
| 4 | 6 | 12 | 2126.4710 | 3026.4663 | 2875.8418 | 3074.8666 | 3004.2802 | 2950.1355 | 2950.1957 | 2989.7818 |
| 4 | 6 | 12 | 2997.5824 | 2974.3431 | 3083.3928 | 3188.0931 | 2950.2400 | 3036.4004 | 2902.9608 | 2992.6121 |

Both tables, *Error.table* / *All.error.table*, have the same columns. The difference is that the *All.error.table* has three more models (PARX1, PARX2 and PARX3):

| K  | Lag  | Month | Lambda | CoefB0 | CoefBVazao | CoefBX1 | CoefBX2 | CoefBX3 | NSE.orig | NSE.comp | Alpha.NSE | Beta.NSE | r.NSE | KGE.orig | Alpha.KGE | Beta.KGE | r.KGE |
| :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: |  :----------: | :----------: | :----------: |  :----------: | :----------: |
| 1 | 1 | 1 | PAR | 525.242237 | 1.28605022 | 0.00000000 | 0.000000 | 0.0000000 | 0.408715395 | 0.429344876 | 0.76733382 | 0.430889002 | 0.78441180 | 0.630199374 | 0.76733382 | 1.1901070 | 0.78441180 |
| 1 | 1 | 1 | PARX | 497.537335 | 1.30763782 | 0.00000000 | 29.343743 | 0.0000000 | 0.406752039 | 0.426690857 | 0.77169391 | 0.423614639 | 0.77858053 | 0.631108467 | 0.77169391 | 1.1868976 | 0.77858053 |
| 1 | 1 | 1 | RIDGE | 663.309483 | 1.19735852 | 47.37760182 | 25.644468 | 46.2968682 | 0.407968389 | 0.430721627 | 0.74652589 | 0.452525296 | 0.79890175 | 0.619801487 | 0.74652589 | 1.1996529 | 0.79890175 |
| ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... |
| 4 | 6 | 12 | PAR | 1978.175760 | 0.622835085 | 0.000000 | 0.000000 | 0.0000000 | -1.61319884 | -1.49194078 | 0.48541761 | 1.101172376 | -0.04504362 | -0.180908108 | 0.48541761 | 1.1939917 | -0.04504362 |
| 4 | 6 | 12 | PARX | 2183.900437 | 0.595745613 | 74.756779 | -80.498486 | 160.9157879 | -1.98999787 | -1.83914904 | 0.61230022 | 1.228205319 | 0.03613491 | -0.061208444 | 0.61230022 | 1.2163708 | 0.03613491 |
| 4 | 6 | 12 | RIDGE | 2643.172325 | 0.350114288 | 35.250906 | -12.987287 | 135.9972728 | -1.95075196 | -1.79037626 | 0.38036294 | 1.266395290 | -0.05513584 | -0.243798517 | 0.38036294 | 1.2230987 | -0.05513584 |

*Lambda.table*:

| K  | Lag  | Month | Lambda |
| :----------: | :----------: | :----------: | :----------: |
| 1 | 1 | 1 | 4.40 |
| 1 | 1 | 1 | 2.80 |
| 1 | 1 | 1 | 1.65 |
| ... | ... | ... | ... |
| 4 | 6 | 10 | 12.25 |
| 4 | 6 | 11 | 36.40 |
| 4 | 6 | 12 | 34.20 |

 ### Possible analysis

 Different analysis may be done with the results. Below we show two possibilities - one being the density plots showing the difference between the metrics for each model proposed (PAR without any climatic indicators, PARX and RIDGE using climatic indicicators).

![alt text](https://github.com/Lappicy/ISPaL/blob/main/Example%20application/Images%20created/Climatic%20value%20density%20plot%20and%20boxplot.png)

 The other analysis may be spatial, shown in the map below.

 ![alt text](https://github.com/Lappicy/ISPaL/blob/main/Example%20application/Images%20created/Climatic%20gain%20SUBSYSTEM%20KGE.png)
