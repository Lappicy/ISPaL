# Read the functions ####
source("R/Functions.R")


# Open data ####
StreamflowEnergy <-
  read.table(file = "Example application/Input data/Streamflow Equivalent Energy.txt",
             header = T, row.names = NULL, check.names = F, sep = "\t", dec = ".",
             fileEncoding = "UTF-8")

ClimaticInfo <-
  read.table(file = "Example application/Input data/Climatic indicators.txt",
             header = T, row.names = NULL, check.names = F, sep = "\t", dec = ".",
             fileEncoding = "UTF-8")


# Run function ####
forecast.results <-
  forecast.PAR.PARX.RIDGE(var.Y = StreamflowEnergy,
                          var.X = ClimaticInfo,
                          forecast.lag = 1:6,
                          forecast.months = 1:12,
                          period.calibration = c(1949, 1990),
                          period.test = c(1991, 2010),
                          period.validation = c(2011, 2021))


# Get the 4 created tables separated
Forecast.table <- forecast.results[[1]]
Error.table <- forecast.results[[2]]
All.error.table <- forecast.results[[3]]
Lambda.table <- forecast.results[[4]]