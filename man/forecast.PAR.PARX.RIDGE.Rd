\name{forecast.PAR.PARX.RIDGE}
\alias{forecast.PAR.PARX.RIDGE}
\title{Monthly forecast using PAR, PARX and RIDGE models with exonegous variables}
\usage{
forecast.PAR.PARX.RIDGE(models = c("PAR", "PARX", "RIDGE"),
                        var.Y, var.X,
                        forecast.lag = 1:6,
                        forecast.months = 1:12,
                        period.calibration,
                        period.validation,
                        period.test)
}
\arguments{

  \item{models}{Character vector specifying which models to run. Accepted values are "PAR", "PARX", and/or "RIDGE".}

  \item{var.Y}{A data.frame containing the first column as date and the others as streamflow time series}

  \item{var.X}{A data.frame containing the first column as date and the others as the desired exonegous variables}

  \item{forecast.lag}{The lead time used for the forecasts}

  \item{forecast.month}{The periodicity of the forecasts (still under construction)}

  \item{period.calibration}{start and end year for calibration process}

  \item{period.validation}{start and end year for validation process (only used with the PARX model)}

  \item{period.test}{start and end year for test process}
}
\value{
This function returns a list with 4 tables. One, \code{Forecast.table}, with the forecasted values for each model and each of the test dates. A table called \code{Error.table}, containing the values for NSE, KGE and their components for the three models (PAR, PARX and RIDGE). The third table, called \code{All.error.table} is similar to \code{Error.table}, but also contains the models PARX1, PARX2 and PARX3 which are defined as forcing to only use the first, second and third exogenous variable, respectively. This may be used to analyse the impact of the individual exonegous variables on the forecasted values. Lastly, there is \code{Lambda.table}, which contains the lambda value used in the ridge regression for each time series, each month and each lead time used.
}
\description{
Forecasts monthly values for \code{var.Y using} \code{var.X} as exogenous variables. It forecasts \code{forecast.lag} months ahead for all 12 months (\code{forecast.month}). The models coefficients are calculated using the \code{period.calibration} time period and tested within the \code{period.teste} years. For the PARX model there is an extra step, where different arrangements of which exogenous variables to use are tested within the \code{period.validation} time frame, with the "best" one being chosen via KGE and only this specific arrengement is tested.
}
\examples{
data(StreamflowEnergy)
data(ClimaticInfo)

forecast.PAR.PARX.RIDGE(var.Y = StreamflowEnergy, var.X = ClimaticInfo,
                        period.calibration = c(1949, 1990),
                        period.test = c(1991, 2010),
                        period.validation = c(2011, 2021))
}
