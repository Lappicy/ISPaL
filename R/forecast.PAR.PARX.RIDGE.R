# Forecast with the three models ####
forecast.PAR.PARX.RIDGE <-
  function(var.Y, var.X,
           forecast.lag = 1:6,
           forecast.months = 1:12,
           period.calibration, period.validation,
           period.test){

    # Pull the start and end data | calibration of PAR and RIDGE ####
    period.calibration.parx <- period.calibration
    period.calibration <- c(period.calibration[1], period.test[2])

    start.date.Y <- min(lubridate::year(var.Y$Date))
    end.date.Y <- max(lubridate::year(var.Y$Date))

    start.date.X <- min(lubridate::year(var.X$Date))
    end.date.X <- max(lubridate::year(var.X$Date))


    # Transform streamflow table to time series format ####
    var.Y <- as.matrix(var.Y[,2:ncol(var.Y)])
    var.Y.ts <- stats::ts(var.Y, deltat = 1/12,
                          start = c(start.date.Y, 1),
                          end = c(end.date.Y, 12))

    # Prepare the data for calibration as well
    var.Y.calib <- stats::window(var.Y.ts, deltat = 1/12,
                                 start = c(period.calibration[1], 1),
                                 end = c(period.calibration[2], 12))

    if(!is.null(period.test)){
      var.Y.calib.parx <- stats::window(var.Y.ts, deltat = 1/12,
                                        start = c(period.calibration.parx[1], 1),
                                        end = c(period.calibration.parx[2], 12))

      var.Y.test.parx <- stats::window(var.Y.ts, deltat = 1/12,
                                       start = c(period.test[1], 1),
                                       end = c(period.test[2], 12))
    }

    var.Y.valid <- stats::window(var.Y.ts, deltat = 1/12,
                                 start = c(period.validation[1], 1),
                                 end = c(period.validation[2], 12))


    # Transform climatic table to time series format ####
    # Transforme data into ts format
    var.X.ts <- stats::ts(var.X[,2:ncol(var.X)], deltat = 1/12,
                          start = c(start.date.X, 1),
                          end = c(end.date.Y, 12))

    # Calibration
    var.X.calib <- stats::window(var.X.ts, deltat = 1/12,
                                 start = c(period.calibration[1], 1),
                                 end = c(period.calibration[2], 12))

    # Test
    if(!is.null(period.test)){
      var.X.calib.parx <- stats::window(var.X.ts, deltat = 1/12,
                                        start = c(period.calibration.parx[1], 1),
                                        end = c(period.calibration.parx[2], 12))

      var.X.test.parx <- stats::window(var.X.ts, deltat = 1/12,
                                       start = c(period.test[1], 1),
                                       end = c(period.test[2], 12))
    }

    var.X.valid <-stats:: window(var.X.ts, deltat = 1/12,
                                 start = c(period.validation[1], 1),
                                 end = c(period.validation[2], 12))


    # For each column K of the streamflow data ####
    for(k in 1:ncol(var.Y.ts)){

      # Message about which K you are running ####
      k.name <- colnames(var.Y)[k]
      cat(paste0("k: ", k, " of ", ncol(var.Y.ts),
                 "  |  Name: ", k.name, "\n"))

      x.c.reg <- cbind(var.Y.calib[, k], var.X.calib)
      y.c.reg <- var.Y.calib[, k]
      x.v.reg <- cbind(var.Y.valid[, k], var.X.valid)
      y.v.reg <- var.Y.valid[, k]

      if(!is.null(period.test)){
        x.c.reg.parx <- cbind(var.Y.calib.parx[, k], var.X.calib.parx)
        y.c.reg.parx <- var.Y.calib.parx[, k]

        x.t.reg <- cbind(var.Y.test.parx[, k], var.X.test.parx)
        y.t.reg <- var.Y.test.parx[, k]
      }


      # For each forecast lag (or lead time) ####
      for (LAG in forecast.lag){
        # Create null ts with the correct validation times for each LAG ####
        yv.pred.par <- stats::ts(NA, deltat = 1/12,
                                 start = c(period.validation[1], 1),
                                 end = c(period.validation[2], 12))

        yv.pred.parx <- stats::ts(NA, deltat = 1/12,
                                  start = c(period.validation[1], 1),
                                  end = c(period.validation[2], 12))

        yv.pred.ridge <- stats::ts(NA, deltat = 1/12,
                                   start = c(period.validation[1], 1),
                                   end = c(period.validation[2], 12))

        # For each MONTH defined ####
        for (MONTH in forecast.months){

          # Select the preditors for calibration, test and validation ####
          # Calibration for PAR and RIDGE models
          aux.calib <- select.pred(lag = LAG, which.month = MONTH,
                                   dates = period.calibration,
                                   x.reg = x.c.reg, y.reg = y.c.reg)
          DATA.calib <- data.frame(y = aux.calib$yreg,
                                   x0 = aux.calib$xreg[,1],
                                   x1 = aux.calib$xreg[,2],
                                   x2 = aux.calib$xreg[,3],
                                   x3 = aux.calib$xreg[,4])

          # Calibration for PARX
          aux.calib.parx <- select.pred(lag = LAG, which.month = MONTH,
                                        dates = period.calibration.parx,
                                        x.reg = x.c.reg.parx,
                                        y.reg = y.c.reg.parx)
          DATA.calib.parx <- data.frame(y = aux.calib.parx$yreg,
                                        x0 = aux.calib.parx$xreg[,1],
                                        x1 = aux.calib.parx$xreg[,2],
                                        x2 = aux.calib.parx$xreg[,3],
                                        x3 = aux.calib.parx$xreg[,4])

          # Teste pros modelos PARX
          aux.test.parx <- select.pred(lag = LAG, which.month = MONTH,
                                       dates = period.test,
                                       x.reg = x.t.reg,
                                       y.reg = y.t.reg)
          DATA.test.parx <- data.frame(y = aux.test.parx$yreg,
                                       x0 = aux.test.parx$xreg[,1],
                                       x1 = aux.test.parx$xreg[,2],
                                       x2 = aux.test.parx$xreg[,3],
                                       x3 = aux.test.parx$xreg[,4])

          # Validation for ALL models
          aux.valid <- select.pred(lag = LAG, which.month = MONTH,
                                   dates = period.validation,
                                   x.reg = x.v.reg, y.reg = y.v.reg)
          DATA.valid <- data.frame(y = aux.valid$yreg,
                                   x0 = aux.valid$xreg[,1],
                                   x1 = aux.valid$xreg[,2],
                                   x2 = aux.valid$xreg[,3],
                                   x3 = aux.valid$xreg[,4])

          # PAR ####
          form <- y ~ x0
          aux.PAR <-  cv1(DATA.1 = DATA.calib, DATA.2 = DATA.valid,
                          formula = form, model.name = "PAR")


          # PARX ####
          # Use AIC and simple linear regression
          KGE.metric = TRUE
          if(KGE.metric){
            model.0 <- y ~ x0
            model.1 <- y ~ x0 + x1
            model.2 <- y ~ x0 + x2
            model.3 <- y ~ x0 + x3
            model.4 <- y ~ x0 + x1 + x2
            model.5 <- y ~ x0 + x1 + x3
            model.6 <- y ~ x0 + x2 + x3
            model.7 <- y ~ x0 + x1 + x2 + x3

            aux.PARX.X0 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.test.parx,
                               formula = model.0, model.name = "PARX0")
            aux.PARX.X1 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.test.parx,
                               formula = model.1, model.name = "PARX1")
            aux.PARX.X2 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.test.parx,
                               formula = model.2, model.name = "PARX2")
            aux.PARX.X3 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.test.parx,
                               formula = model.3, model.name = "PARX3")
            aux.PARX.4 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.test.parx,
                              formula = model.4, model.name = "PARX")
            aux.PARX.5 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.test.parx,
                              formula = model.5, model.name = "PARX")
            aux.PARX.6 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.test.parx,
                              formula = model.6, model.name = "PARX")
            aux.PARX.7 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.test.parx,
                              formula = model.7, model.name = "PARX")

            optimal.model <-
              which.max(c(aux.PARX.X0[[4]]["KGE.orig"], aux.PARX.X1[[4]]["KGE.orig"],
                          aux.PARX.X2[[4]]["KGE.orig"], aux.PARX.X3[[4]]["KGE.orig"],
                          aux.PARX.4[[4]]["KGE.orig"], aux.PARX.5[[4]]["KGE.orig"],
                          aux.PARX.6[[4]]["KGE.orig"], aux.PARX.7[[4]]["KGE.orig"]))

            optimal.form <- c(model.0, model.1, model.2, model.3,
                              model.4, model.5, model.6, model.7)[optimal.model]
            optimal.form <- as.formula(as.character(optimal.form))


            # Validate all PARX models, including the "BEST"
            aux.PARX <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.valid,
                            formula = optimal.form, model.name = "PARX")
            aux.PARX.X0 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.valid,
                               formula = model.0, model.name = "PARX0")
            aux.PARX.X1 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.valid,
                               formula = model.1, model.name = "PARX1")
            aux.PARX.X2 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.valid,
                               formula = model.2, model.name = "PARX2")
            aux.PARX.X3 <- cv1(DATA.1 = DATA.calib.parx, DATA.2 = DATA.valid,
                               formula = model.3, model.name = "PARX3")

            # Remove other PARX models
            remove(aux.PARX.4, aux.PARX.5, aux.PARX.6, aux.PARX.7)
          }


          # RIDGE ####
          form <- y ~ x0 + x1 + x2 + x3
          aux.RIDGE <- cv.ridge(DATA.1 = DATA.calib, DATA.2 = DATA.valid,
                                formula = form, model.name = "RIDGE")


          # Lambda table ####
          # Lambda values for each k and month
          if(LAG == forecast.lag[1] & MONTH == forecast.months[1] & k == 1){
            Lambda.table <-
              data.frame(K = k, Lag = LAG, Month = MONTH,
                         Lambda = aux.RIDGE[[3]])
          }else{
            Lambda.table <-
              rbind(Lambda.table,
                    data.frame(K = k, Lag = LAG, Month = MONTH,
                               Lambda = aux.RIDGE[[3]]))
          }


          # Error table (coeficientes NSE e KGE) ####
          if(LAG == forecast.lag[1] & MONTH == forecast.months[1] & k == 1){

            Error.table <-
              cbind(data.frame(K = k, Lag = LAG, Month = MONTH),
                    rbind(aux.PAR[[4]], aux.PARX[[4]], aux.RIDGE[[2]]))

            All.error.table <-
              cbind(data.frame(K = k, Lag = LAG, Month = MONTH),
                    rbind(aux.PAR[[4]], aux.PARX[[4]], aux.RIDGE[[2]],
                          aux.PARX.X1[[4]], aux.PARX.X2[[4]], aux.PARX.X3[[4]]))

          }else{
            Error.table <-
              rbind(Error.table,
                    cbind(data.frame(K = k, Lag = LAG, Month = MONTH),
                          rbind(aux.PAR[[4]], aux.PARX[[4]], aux.RIDGE[[2]])))

            All.error.table <-
              rbind(All.error.table,
                    cbind(data.frame(K = k, Lag = LAG, Month = MONTH),
                          rbind(aux.PAR[[4]], aux.PARX[[4]], aux.RIDGE[[2]],
                                aux.PARX.X1[[4]], aux.PARX.X2[[4]], aux.PARX.X3[[4]])))
          }


          # Forecast table ####
          if(LAG == forecast.lag[1] & MONTH == forecast.months[1] & k == 1){

            Forecast.table <-
              cbind(data.frame(K = k, Lag = LAG, Month = MONTH),
                    Obs = as.numeric(DATA.valid$y),
                    SimPAR = as.numeric(aux.PAR[[1]]),
                    SimPARX = as.numeric(aux.PARX[[1]]),
                    SimRIDGE = as.numeric(aux.RIDGE[[1]]),
                    SimPARX0 = as.numeric(aux.PARX.X0[[1]]),
                    SimPARX1 = as.numeric(aux.PARX.X1[[1]]),
                    SimPARX2 = as.numeric(aux.PARX.X2[[1]]),
                    SimPARX3 = as.numeric(aux.PARX.X3[[1]]))

          }else{
            Forecast.table <-
              rbind(Forecast.table,
                    cbind(data.frame(K = k, Lag = LAG, Month = MONTH),
                          Obs = as.numeric(DATA.valid$y),
                          SimPAR = as.numeric(aux.PAR[[1]]),
                          SimPARX = as.numeric(aux.PARX[[1]]),
                          SimRIDGE = as.numeric(aux.RIDGE[[1]]),
                          SimPARX0 = as.numeric(aux.PARX.X0[[1]]),
                          SimPARX1 = as.numeric(aux.PARX.X1[[1]]),
                          SimPARX2 = as.numeric(aux.PARX.X2[[1]]),
                          SimPARX3 = as.numeric(aux.PARX.X3[[1]])))
          }
          # Remove what has been done, to not erase it for next loop ####
          remove(aux.calib, DATA.calib, aux.calib.parx, DATA.calib.parx,
                 aux.test.parx, DATA.test.parx, aux.valid, DATA.valid,
                 form, aux.PAR, optimal.form, optimal.model, aux.PARX.X0,
                 aux.PARX.X1, aux.PARX.X2, aux.PARX.X3, aux.RIDGE)

        }
      }

    }


    # Return four tables ####
    return(list(Forecast.table = as.data.frame(Forecast.table),
                Error.table = as.data.frame(Error.table),
                All.error.table = as.data.frame(All.error.table),
                Lambda.table = as.data.frame(Lambda.table)))
  }
