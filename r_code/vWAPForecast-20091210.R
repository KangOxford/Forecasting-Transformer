################################################################################
##
## File: vWAPForecast-20091210.R
##
## Purpose:
##  R functions for forecasting in the daily-intradaily MEM.
##
## Created: 2008.11.08
##
## Version: 2009.12.07
##
## Author:
##  Fabrizio Cipollini <cipollini@ds.unifi.it>
##
################################################################################

################################################################################
## FUNCTION:                       TASK:
##
##  .forecast.diMEM.day()           Make forecasts from the di-MEM for a given
##                                   day.
##  .forecast.diMEM()               Make forecasts from the di-MEM.
##
################################################################################


.forecast.diMEM.day <-
function( parmVal, infoFilter, x, fltLag, flt0, day )
{
  ##############################################################################
  ## Description:
  ##  Make forecasts from the daily-intradaily MEM for a given day.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (numeric) data adjusted as in make.data().
  ##  fltLag: (list) current (at time1) lagged filter components. It has
  ##   components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##  flt0: (list) starting (at 0) filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##  day: (numeric) day at which to make forecasts.
  ##
  ## Value:
  ##  (list) with the following components
  ##  $forecast: (matrix) rolling forecasts of a day, from a given bin to the
  ##    last bin of the day.
  ##  $lagged: (numeric) updated lagged filter. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: General settings
  ##############################################################################

  #### From data
  ## Time indexes
  indx    <- index(x)
  ## First date in the data (taken as reference)
  dates  <- as.Date(indx[endpoints(x = x, on = "days")]) 
  ## Means
	meansX  <- .means.data(data1 = x)

  #### From diControl
  nBin  <- infoFilter$diControl$nBin

  #### Time point for forecasts
  ## Day of forecasts
  time1 <- dates[day]
  time1 <- as.numeric(format(time1, "%Y%m%d"))
  ## Last dateClock of forecasts
  timeL <- list(date = time1, bin = nBin)
  timeL <- .dateBin.2.dateClock(timeL, nBin)
  ## First dateClock of forecasts
  time1 <- list(date = time1, bin = 1)
  time1 <- .dateBin.2.dateClock(time1, nBin)  

  #### Observation points for forecasts
  ind1  <- which(indx == time1)
  indL  <- which(indx == timeL)
  print(ind1)
  print(indL)
  

  ##############################################################################
  ## Part 2: forecasts
  ##############################################################################

  #### Initialize output
  # forStore <- matrix(NA, nBin, nBin)
  forStore <- matrix(NA, nBin, nBin)
  colnames(forStore) <- as.character( (ind1 - 1) : (indL - 1) ) ## From in columns
  rownames(forStore) <- as.character( ind1 : indL )             ## To   in rows

  forStore1 <- matrix(NA, nBin, nBin)
  #### Forecasts (j is the from bin for the forecast)
  ind1 <- ind1 - 1
  for ( j in 0 : (nBin - 1) )
  {
    #### Set counter
    k  <- nBin - j       ## Number of steps ahead
    j  <- j + 1          ## First forecasted bin (bin at which we have info + 1)
    ind0 <- ind1         ## Current obs for estimates
    ind1 <- ind0 + 1     ## First obs for forecasts
    ind2 <- ind0 + k     ## Last obs for forecasts
    
		#### Forecast
		## Data
		data1  <- x[ind1 : ind2, , drop = FALSE]
		data1[, 1:2] <- NA
		data1 <- .make.data(data1 = data1, meanX = meansX$x, meanZM = meansX$zM)
		forecast <- .filter.diMEM.1(parmVal, infoFilter, data1, fltLag, flt0, ind0)$filter

        # save j-th position eta, seas and mu
        # forecast_df <- data.frame(
        #   eta = rep(forecast$eta, length(forecast$seas)),
        #   seas = forecast$seas,
        #   mu = forecast$mu
        # )
        # # print(j)
        # forecast_line <- forecast_df[j,]
        # Append to results_df
        # results_df <- rbind(results_df, forecast_line)

        # cat("j is :", j, "\n")


        forStore1[j,1] <- forecast$eta
        forStore1[j,2] <- forecast$seas[j]
        forStore1[j,3] <- forecast$mu[1]


		## Adjust forecasts
		times <- indx[ind1 : ind2]
		calendar <- .calendar(times, nBin)
		forecast <- .condMean(forecast, calendar)

    #### Update the filter
    ## Adjust data
    data1 <- x[ind1, , drop = FALSE]           ## Used for filtering
    data1 <- .make.data(data1 = data1, meanX = meansX$x, meanZM = meansX$zM)
    ## Filter
    fltLag <- .filter.diMEM.1(parmVal, infoFilter, data1, fltLag, flt0, ind0)$lagged

    #### Store
    forStore[j:nBin,j] <- forecast
    forStore1[j,4] <-forStore[j,j]

  }


  ##############################################################################
  ## Part 5: Answer
  ##############################################################################

  #### Answer
  # list(forecast = forStore, lagged = fltLag)
  list(forecast = forStore1, lagged = fltLag)
}
# ------------------------------------------------------------------------------


.forecast.diMEM <-
function( parmVal, infoFilter, x, diControl )
{
  ##############################################################################
  ## Description:
  ##  Make forecasts from the daily-intradaily MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  x: (xts) time series of data, an xts object produced by .data.2.xts().
  ##  diControl: (list) with (at least) the following components
  ##   $nBin: (numeric) number of equally spaced daily bins.
  ##   $nDFor: (numeric) days at which to make forecasts.
  ##   See .diControl() for details.
  ##
  ## Value:
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: General settings
  ##############################################################################

  #### Times
  nBin   <- diControl$nBin
  nDFor  <- diControl$nDFor
  indx   <- index(x)               ## Time index in the data
  dates  <- as.Date(indx[endpoints(x = x, on = "days")]) 
  nDx    <- NROW(dates)            ## Number of days in the data
  ## First/last day of forecasts
  nDFor1 <- min(nDFor)
  nDForL <- max(nDFor)
  ## First/last dates in forecasts
  # if ( nDForL > nDx)
  # {
  # 	stop("Forecast days go beyond the data")
  # }
  
  #### Time point for estimates
  ## Last date in the estimates = first date in the forecasts - 1
  time.e <- dates[nDFor1 - 1]
  time.e <- as.numeric(format(time.e, "%Y%m%d"))
  ## Append bin (the last of the day)
  time.e  <- list(date = time.e, bin = nBin)
  ## Transform to dateClock
  time.e  <- .dateBin.2.dateClock(time.e, nBin)
  

  ##############################################################################
  ## Part 3: Settings for the filter
  ##############################################################################

  #### Time series settings
  data1  <- .means.data(data1 = x)

  #### Info-filter
  maxLag <- infoFilter$maxLag

  #### Starting values of the filter
  eta0   <- data1$x
  mu0    <- 1
  flt0   <- .flt0.diMEM(eta0, mu0)


  ##############################################################################
  ## Part 3: filter
  ##############################################################################

  #### Adjust data for filtering
  data2 <- x[indx <= time.e, , drop = FALSE]
  data2 <- .make.data(data1 = data2, meanX = data1$meanX, meanZM = data1$meanZM)

  #### Filter
  ind0 <- 0
  fltLag <- .fltLag0.diMEM(flt0, maxLag)
  fltLag <- .filter.diMEM.1(parmVal, infoFilter, data2, fltLag, flt0, ind0)$lagged


  ##############################################################################
  ## Part 4: forecasts
  ##############################################################################

  #### Initialize
  out <- vector(mode = "list", length = nDForL - nDFor1 + 1)
  
  #### Forecast
  i <- 0
  for (day in nDFor[1]:nDFor[2])
  {
    #### Increase
    i <- i + 1
    #### Compute
    flt1 <- .forecast.diMEM.day( parmVal, infoFilter, x, fltLag, flt0, day)
    #### Update
    fltLag   <- flt1$lagged
    #### Store
    out[[i]] <- flt1$forecast
  }

  ##############################################################################
  ## Part 5: Answer
  ##############################################################################

  #### Answer
  out
}
# ------------------------------------------------------------------------------
