################################################################################
##
## File: vWAPTime-20091205.R
##
## Purpose:
##  Function for investigating the decomposition of the time series in different
##  components.
##
## Created: 2009.02.20
##
## Version: 2009.12.07
##
## Author:
##  Fabrizio Cipollini
##
################################################################################
##
## PART 1: FUNCTIONS for calendar management.
##
##  .dailyClock()                   Compute equally spaced clocks corresponding
##                                   to bins within a day.
##  .dailyBins()                    Compute equally spaced bins within a day.
##  .bin.2.clock()                  Convert daily bins to clocks (and viz).
##  .dateBin.2.dateClock()          Convert times from date-bin to date-clock
##                                   (and viz).
##  .is.dateBin()                   Check if an object is dateBin.
##  .is.dateClock()                 Check if an object is dateClock.
##  .date()                         Extract the date from a POSIX object
##                                   converting it to a numeric %Y%m%d.
##  .clock()                        Extract the date from a POSIX object
##                                   converting it to a character %H:%M.
##  .bin()                          Extract the bin from a POSIX object.
##  .calendar()                     Make the complete calendar (date, bin)
##                                   relative to time indexes into the input.
##  .day.2.date()                   Convert x (number of days since 'dateRef') 
##                                   to a Date and viz.
##
## PART 2: FUNCTIONS for data and statistics management.
##  .data.2.xts()                   Convert data to an xts object. Times are
##                                   built from columns "date" and "bin".
##  .make.data()                    Make data. In the input: the first column is
##                                   interpreted as the modeled variable; the
##                                   (possible) second column is interpreted as
##                                   prices.
##  .extract.lastBin()              Extract the last bin of the day.
##  .expand()                       Expand a vector to the whole time period.
##  .daily.x()                      Compute an estimate of the daily component
##                                   (eta) from the time series (x).
##  .intradaily.x()                 Compute an estimate of the intradaily
##                                   component (mu*seas) from the time series
##                                   (x) and an estimate of the daily component
##                                   (eta).
##  .intradaily.flt()               Compute the intradaily component (mu * seas)
##                                   from the filter (mu and seas components).
##  .condMean()                     Compute the conditional mean in the di-MEM.
##
################################################################################


################################################################################
## PART 1: FUNCTIONS for calendar management.
##
##  .dailyClock()                   Compute equally spaced clocks corresponding
##                                   to bins within a day.
##  .dailyBins()                    Compute equally spaced bins within a day.
##  .bin.2.clock()                  Convert daily bins to clocks (and viz).
##  .dateBin.2.dateClock()          Convert times from date-bin to date-clock
##                                   (and viz).
##  .is.dateBin()                   Check if an object is dateBin.
##  .is.dateClock()                 Check if an object is dateClock.
##  .date()                         Extract the date from a POSIX object
##                                   converting it to a numeric %Y%m%d.
##  .clock()                        Extract the date from a POSIX object
##                                   converting it to a character %H:%M.
##  .bin()                          Extract the bin from a POSIX object.
##  .calendar()                     Make the complete calendar (date, bin)
##                                   relative to time indexes into the input.
##  .day.2.date()                   Convert x (number of days since 'dateRef') 
##                                   to a Date and viz.
################################################################################

.dailyClock <-
function(nBin)
{
  ##############################################################################
  ## Description:
  ##  Compute equally spaced clocks corresponding to bins within a day, in the
  ##  %H:%M format.
  ##
  ## Arguments:
  ##  nBin: (numeric) number of bins.
  ##
  ## Value:
  ##  (character) bin clocks.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### NYSE Opening time
  times <- c("9:30", "16:00")

  #### Convert
  times <- strptime(x = times, format = "%H:%M")

  #### Time step (in seconds)
  stp <- (times[2] - times[1]) / nBin
  stp <- as.numeric(stp) * 3600

  #### Start from the end of the first bin
  times <- times[1] + stp
  times <- seq(from = times, by = stp, length.out = nBin)

  #### Answer as character
  format(x = times, format = "%H:%M", usetz = FALSE)
}
# ------------------------------------------------------------------------------


.dailyBins <-
function(nBin)
{
  ##############################################################################
  ## Description:
  ##  Compute equally spaced bins within a day.
  ##
  ## Arguments:
  ##  nBin: (numeric) number of bins.
  ##
  ## Value:
  ##  (numeric) bins.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:
  
  #### Answer
  1 : nBin
}
# ------------------------------------------------------------------------------


.bin.2.clock <-
function(x, nBin)
{
  ##############################################################################
  ## Description:
  ##  Convert daily bins to clocks (and viz).
  ##
  ## Arguments:
  ##  x: (numeric) bins or (character) clocks.
  ##  nBin: (numeric) number of bins.
  ##
  ## Value:
  ##  (character) clocks or (numeric) bins.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Set levels and labels
  if ( is.numeric(x[1]) )
  {
    .factor( x = x, levels = .dailyBins(nBin), labels = .dailyClock(nBin),
      typeNum = FALSE)
  }
  else
  {
    .factor( x = x, levels = .dailyClock(nBin), labels = .dailyBins(nBin),
      typeNum = TRUE)
  }
}
# ------------------------------------------------------------------------------


.dateBin.2.dateClock <-
function(x, nBin)
{
  ##############################################################################
  ## Description:
  ##  Convert times from date-bin to date-clock (and viz).
  ##
  ## Arguments:
  ##  x: (list) with components
  ##   $date: (numeric) date as %Y%m%d
  ##   $bin: (numeric) bins
  ##   or
  ##   (POSIX) date-clock as %Y%m%d %H:%M.
  ##  nBin: (numeric) number of bins.
  ##
  ## Value:
  ##  (POSIX) date-clock or (list) date-bin (see arguments).
  ##
  ## Author:	
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### if x is dateBin
  if ( .is.dateBin(x) )
  {
    #### Convert bin to dailyClock
    x$bin <- .bin.2.clock(x = x$bin, nBin = nBin)

    #### Append dates with daily times
    as.POSIXct(x = paste(x$date, x$bin), format = "%Y%m%d %H:%M", tz = "")
  }
  
  #### if x is dateClock
  else if ( .is.dateClock(x) )
  {
    #### Append dates with daily times
    list( date = .date(x), bin = .bin(x) )
  }
  
  #### Error
  else
  {
    stop("'x' argument of wrong type")
  }
}
# ------------------------------------------------------------------------------


.is.dateBin <-
function(x)
{
  ##############################################################################
  ## Description:
  ##  Check if an object is dateBin.
  ##
  ## Arguments:
  ##  x: (object) .
  ##
  ## Value:
  ##  (logical) .
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  ( NROW(x) == 2 && NROW(x[[1]]) == NROW(x[[2]]) )
}
# ------------------------------------------------------------------------------


.is.dateClock <-
function(x)
{
  ##############################################################################
  ## Description:
  ##  Check if an object is dateClock.
  ##
  ## Arguments:
  ##  x: (object) .
  ##
  ## Value:
  ##  (logical) .
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  (NROW(x) != 2 & NCOL(x) == 1)
}
# ------------------------------------------------------------------------------


.date <-
function(x)
{
  ##############################################################################
  ## Description:
  ##  Extract the date from a POSIX object converting it to a numeric %Y%m%d.
  ##
  ## Arguments:
  ##  x: (numeric) POSIX object.
  ##
  ## Value:
  ##  (numeric) dates as %Y%m%d.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  as.numeric( format(x = x, format = "%Y%m%d", tz = "", usetz = FALSE) )
}
# ------------------------------------------------------------------------------


.clock <-
function(x)
{
  ##############################################################################
  ## Description:
  ##  Extract the bin-clock from a POSIX object converting it to a character 
  ##  %H:%M.
  ##
  ## Arguments:
  ##  x: (numeric) POSIX object.
  ##
  ## Value:
  ##  (character) clock as %H:%M.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  format(x = x, format = "%H:%M", tz = "", usetz = FALSE)
}
# ------------------------------------------------------------------------------


.bin <-
function(x, nBin)
{
  ##############################################################################
  ## Description:
  ##  Extract the bin from a POSIX object.
  ##
  ## Arguments:
  ##  x: (numeric) POSIX object.
  ##
  ## Value:
  ##  (numeric) bin.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  .factor( x = .clock(x), levels = .dailyClock(nBin), labels = .dailyBins(nBin), 
    typeNum = TRUE )
}
# ------------------------------------------------------------------------------


.calendar <-
function(x, nBin)
{
  ##############################################################################
  ## Description:
  ##  Make the complete calendar (date, bin) relative to time indexes into x.
  ##
  ## Arguments:
  ##  x: (POSIX) time index.
  ##  nBin: (numeric) number of bins.
  ##
  ## Value:
  ##  (list) with components
  ##   $dateL: (numeric) whole vector of dates.
  ##   $date: (numeric) vector of dates.
  ##   $binL: (numeric) whole vector of bins.
  ##   $bin: (numeric) vector of bins.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### dateL
  dateL <- .date(x)

  #### Answer
  list(dateL = dateL, binL = .bin(x, nBin),
       date = sort(unique(dateL)), bin = 1 : nBin)
}
# ------------------------------------------------------------------------------


.time <-
function(time1, nBin)
{
  ##############################################################################
  ## Description:
  ##  Update the time stamp, making the complete list (time, day, bin).
  ##
  ## Arguments:
  ##  time1: (numeric) time, or (list) time stamp with components
  ##   $time: (numeric) time
  ##   $day: (numeric) day
  ##   $bin: (numeric) bin
  ##   At least $time or the couple ($day, $bin) must be set in input.
  ##  nBin: (numeric) nBin
  ##
  ## Value:
  ##  (list) as in the 'tim' argument with updated components.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Settings
  nBin <- nBin[1]
  if (is.list(time1))
  {
    tim <- time1$time[1]
    day <- time1$day[1]
    bin <- time1$bin[1]
  }
  else
  {
    tim <- time1[1]
  }

  #### Compose
  if ( NROW(tim) > 0 )
  {
    day <- ceiling(tim / nBin)
    bin <- tim - (day - 1) * nBin
  }
  else if ( NROW(day) > 0 && NROW(bin) > 0 )
  {
    tim <- (day - 1) * nBin + bin
  }
  else
  {
    stop("Argument 'tim' must specify $time or the couple $day, $bin")
  }

  #### Answer
  list(time = tim, day = day, bin = bin)
}
# ------------------------------------------------------------------------------


.day.2.date <- 
function(dateRef, x, nWD = 5, nD = 7)
{
  ##############################################################################
  ## Description:
  ##  Convert x (number of days since 'dateRef') to a Date and viz.
  ##
  ## Arguments:
  ##  dateRef: (Date) Date.
  ##  x: (numeric) day or (Date) Date to be converted.
  ##  nWD: (numeric) number of working days per week.
  ##  nD: (numeric) number of days per week.
  ##
  ## Value:
  ##  (Date) or (numeric) converted value.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

	#### If the input is numeric
	if (is.numeric(x))
	{
		nfw <- x %/% nWD                    ## Number of full weeks
		nrd <- x - nfw * nWD                ## Number of remaining days in the week
		x <- nfw * nD + nrd                 ## Number of days
		x <- dateRef + x
	}

	#### If the input is Date or POSIX
	else
	{
		x <- as.numeric(as.Date(x) - as.Date(dateRef))
		nfw <- x %/% nD                     ## Number of full weeks
		nrd <- x - nfw * nD                 ## Number of remaining days in the week
		x <- nfw * nWD + nrd                ## Number of days
	}
	
	#### Answer
	x
}
# ------------------------------------------------------------------------------


.seq.dateClock <- 
function(from, to, nBin, typeDB = TRUE)
{
  ##############################################################################
  ## Description:
  ##  Generate a sequence of dateClock from 'from' to 'to' spaced by 'nBin'.
  ##
  ## Arguments:
  ##  from: (POSIX) from.
  ##  to: (POSIX) to.
  ##  nBin: (numeric) nBin.
  ##  typeDB: (character) type dateBin or dateClock
  ##
  ## Value:
  ##  (Date) or (numeric) converted value.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

	#### Settings 
  x  <- range(from[1], to[1])
  from <- x[1]
  to <- x[2]
  nBin <- nBin[1]
  
  #### Separate date and bin
  fromD <- as.Date(from)
  toD <- as.Date(to)
  fromB <- .bin(x = from, nBin = nBin)
  toB <- .bin(x = to, nBin = nBin)

  #### Number of days
  nD <- .day.2.date(dateRef = fromD, x = toD)

  #### Convert dates to numeric
  fromD.N <- as.numeric(format(fromD, "%Y%m%d"))
  toD.N <- as.numeric(format(toD, "%Y%m%d"))
  
  #### 1) Case fromD == toD
  if (nD == 0)
  {
  	xB <- fromB : toB
  	xD <- rep.int(fromD.N, NROW(xB))
    x <- list(date = xD, bin = xB)
  }

  #### 2) Case fromD < toD
  else
  {
  	#### First part
  	xB <- fromB : nBin
  	xD <- rep.int(fromD.N, NROW(xB))
    x1 <- list(date = xD, bin = xB)
    
    #### Middle part
    if (nD > 1)
    { 
    	#### days
    	tmp <- nD - 1
    	xD <- .day.2.date(dateRef = fromD, x = 1 : tmp)
      xD <- as.numeric(format(xD, "%Y%m%d"))
    	xD <- rep.int(xD, rep.int(nBin, tmp))
    	#### bins
    	xB <- rep.int(1 : nBin, tmp)
    	####
    	x2 <- list(date = xD, bin = xB)
    }
    else
    {
    	x2 <- list(date = NULL, bin = NULL)
    }
  	
  	#### Last part
  	xB <- 1 : toB
  	xD <- rep.int(toD.N, NROW(xB))
    x3 <- list(date = xD, bin = xB)
    
    #### Append
    x <- list(date = c(x1$date, x2$date, x3$date), 
              bin  = c(x1$bin , x2$bin , x3$bin ))
  }
  
  ####
  if ( !typeDB[1] )
  {
  	x <- .dateBin.2.dateClock(x, nBin)
  }
	
	#### Answer
	x
}
# ------------------------------------------------------------------------------


################################################################################
## PART 2: FUNCTIONS for data and statistics management.
##  .data.2.xts()                   Convert data to an xts object. Times are
##                                   built from columns "date" and "bin".
##  .make.data()                    Make data. In the input: the first column is
##                                   interpreted as the modeled variable; the
##                                   (possible) second column is interpreted as
##                                   prices.
##  .extract.lastBin()              Extract the last bin of the day.
##  .expand()                       Expand a vector to the whole time period.
##  .daily.x()                      Compute an estimate of the daily component
##                                  (eta) from the time series (x).
##  .intradaily.x()                 Compute an estimate of the intradaily
##                                   component (mu* seas) from the time series
##                                   (x) and an estimate of the daily component
##                                   (eta).
##  .intradaily.flt()               Compute the intradaily component (mu * seas)
##                                   from the filter (mu and seas components).
##  .condMean()                     Compute the conditional mean in the di-MEM.
################################################################################


.data.2.xts <-
function(x)
{
  ##############################################################################
  ## Description:
  ##  Convert data to an xts object. Times are built from columns "date" and
  ##  "bin".
  ##
  ## Arguments:
  ##  x: (data.frame) Data. Columns "date" and "bin" are needed.
  ##
  ## Value:
  ##  (xts) Data.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Time columns
  timeCols <- c("date", "bin")
  dataCols <- !(colnames(x) %in% timeCols)
  
  #### Convert time
  timeL <- as.list( x[, timeCols, drop = FALSE] )
  timeL <- .dateBin.2.dateClock(x = timeL, nBin = nBin)
  
  #### xts
  xts(x = x[, dataCols, drop = FALSE], order.by = timeL)
}
# ------------------------------------------------------------------------------


.means.data <-
function(data1)
{
  ##############################################################################
  ## Description:
  ##  Compute some useful averages from the data.
  ##  The current version is quite raw, since it assumes that the
  ##  1) column 1 = modeled variable;
  ##  2) column two = prices
  ##  3) remaining columns (if any) = predetermined variables
  ##
  ## Arguments:
  ##  data1: (xts) data as produced by .data.2.xts().   
  ##
  ## Value:
  ##  (list) adjusted data. It has components:
  ##   $x: (numeric) average of x.
  ##   $zM: (numeric) average of predetermined variables.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Mean of the dependent variable
  x <- mean(data1[,1], na.rm = TRUE)

  #### predetermined into Mu
  if (NCOL(data1) >= 3)
  {
    zM <- colMeans(data1[, 3 : NCOL(data1), drop = FALSE], na.rm = TRUE)
  }
  else
  {
    zM <- 0
  }

  #### Answer
  list(x = x, zM = zM)
}
# ------------------------------------------------------------------------------


.make.data <-
function(data1, meanX = NULL, meanZM = NULL)
{
  ##############################################################################
  ## Description:
  ##  Make data. In the input: the first column is interpreted as the modeled
  ##  variable; the (possible) second column is interpreted as prices.
  ##
  ## Arguments:
  ##  data1: (xts) data as produced by .data.2.xts(). Currently, only the first 
  ##   one or two columns are used.
  ##  meanX: (numeric) A scalar giving the mean of the modeled variable.
  ##  meanZM: (numeric) A vector giving the mean of the predetrmined variables.
  ##
  ## Value:
  ##  (list) adjusted data. It has components:
  ##   $x: (numeric) modeled variable
  ##   $indRet: (numeric) with the same dimension as x, with elements:
  ##                     0  if return[i,j] > 0
  ##    indRet[i, j] = 0.5  if return[i,j] == 0 or NA
  ##                     1  if return[i,j] < 0
  ##    where return[i,j] = ln( price[i,j] / price[i,j-1] )
  ##   $indDRet: (numeric) with elements:
  ##                   0  if Dreturn[i] > 0
  ##    indDRet[i] = 0.5  if Dreturn[i] == 0 or NA
  ##                   1  if Dreturn[i] < 0
  ##    where Dreturn[i] = ln( price[i,J] / price[i-1,J] )
  ##   $meanX: (numeric) mean of x
  ##  If the input does not have a second column, indRet and indDRet have all
  ##  elements equal to 0.5.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: times, calendar
  ##############################################################################

  #### Settings
  nobs  <- NROW(data1)
  nDay  <- ndays(data1)
  nc1   <- NCOL(data1)


  ##############################################################################
  ## Part 2: Adjust data: dependent variable
  ##############################################################################

  #### x
  x <- data1[, 1]
  ind.x <- index(x)

  #### Mean of the dependent variable
  if ( NROW(meanX) == 0 )
  {
    meanX <- mean(x, na.rm = TRUE)
  }

  
  ##############################################################################
  ## Part 3: Adjust data: prices to returns
  ##############################################################################

  #### 'prices'
  if (nc1 >= 2)
  {
    #### Returns
    price  <- data1[, 2]
    tmp    <- as.numeric(price)
    ret    <- c( 0, log(tmp[-1] / tmp[-nobs]) )
    ret    <- replace(ret, is.na(ret), 0)
    indRet <- (ret < 0) + 0.5 * (ret == 0)

    #### Daily returns
    price   <- .extract.lastBin(price)
    tmp     <- as.numeric(price)
    ret     <- c( 0, log(tmp[-1] / tmp[-nDay]) )
    ret     <- replace(ret, is.na(ret), 0)
    indDRet <- (ret < 0) + 0.5 * (ret == 0)    
  }
  else
  {
    #### Returns
    indRet <- rep.int(0.5, nobs)

    #### Daily returns
    indDRet <- rep.int(0.5, nDay)
  }
	
  #### xts
  indRet <- xts(x = indRet, order.by = ind.x)


  ##############################################################################
  ## Part 4: Adjust data: predetermined
  ##############################################################################
  
  #### predetermined into Eta (not used for the moment)
	zE <- matrix(0, nDay, 1)
	meanZE <- 0

  #### predetermined into Mu
  if (nc1 >= 3)
  {
    #### Values
    zM <- data1[, 3 : nc1, drop = FALSE]
    
    #### Mean
    if (NROW(meanZM) == 0)
    {
      meanZM <- colMeans(zM, na.rm = TRUE)
    }
  }
  else
  {
    #### Predetermined
    zM <- matrix(0, nobs, 1)    
    meanZM <- 0
  }
  
  #### xts
  zM <- xts(x = zM, order.by = ind.x)

  
  ##############################################################################
  ## Part 5: Answer
  ##############################################################################

  #### Answer
  list(x = x, zM = zM, zE = zE, indRet = indRet, indDRet = indDRet, 
    meanX = meanX, meanZE = meanZE, meanZM = meanZM)
}
# ------------------------------------------------------------------------------


.extract.lastBin <-
function(x)
{
  ##############################################################################
  ## Description:
  ##  Extract the last bin of the day.
  ##
  ## Arguments:
  ##  x: (xts) time series.
  ##
  ## Value:
  ##  (xts) time series including only last bins of each day.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  x[ endpoints(x, on = "days"), ]
}
# ------------------------------------------------------------------------------


.expand <-
function(x, timL, tim)
{
  ##############################################################################
  ## Description:
  ##  Expand a vector to the whole time period. In practice, 'x' must have the 
  ##  same number of elements as 'tim': each element x[i] is replicated along 
  ##  'timL' for the same tim[i] value.
  ##
  ## Arguments:
  ##  x: (numeric) vector to be expanded.
  ##  timL: (numeric) time covering the whole period.
  ##  tim: (numeric) time covering only x.
  ##
  ## Value:
  ##  (numeric) expanded vector.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  .factor(x = timL, levels = tim, labels = x, typeNum = TRUE)
}
# ------------------------------------------------------------------------------


.daily.x <-
function(x, calendar)
{
  ##############################################################################
  ## Description:
  ##  Compute an estimate of the daily component (eta) from the time series (x)
  ##  by averaging x for each day. Retuns a NA is the day includes at leat a NA
  ##  value.
  ##
  ## Arguments:
  ##  x: (numeric) time series.
  ##  calendar: (list) including the following components:
  ##   $dateL: (numeric) whole vector of days.
  ##
  ## Value:
  ##  (numeric) daily estimates.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  aggregate(x = x, by = list(date = calendar$dateL), FUN = mean,
    na.rm = FALSE)$x
}
# ------------------------------------------------------------------------------


.intradaily.x <-
function(x, eta, calendar)
{
  ##############################################################################
  ## Description:
  ##  Compute an estimate of the intradaily component (mu * seas) from the
  ##  time series (x) and an estimate of the daily component (eta).
  ##
  ## Arguments:
  ##  x: (numeric) time series.
  ##  eta: (numeric) daily component.
  ##  calendar: (list) including the following components:
  ##   $dateL: (numeric) whole vector of dates.
  ##   $date: (numeric) vector of dates.
  ##
  ## Value:
  ##  (xts) intradaily estimates.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Expand eta conformably to x
  eta  <- .expand(eta, calendar$dateL, calendar$date)

  #### Answer
  x / eta
}
# ------------------------------------------------------------------------------


.intradaily.flt <-
function(flt, calendar)
{
  ##############################################################################
  ## Description:
  ##  Compute the intradaily component in the daily-intradaily MEM.
  ##
  ## Arguments:
  ##  flt: (list) including the following components:
  ##   $seas: (numeric) intradaily periodic component.
  ##   $mu: (numeric) intradaily non periodic component.
  ##  calendar: (list) including the following components:
  ##   $binL: (numeric) whole vector of bins.
  ##   $bin: (numeric) vector of bins.
  ##
  ## Value:
  ##  (numeric) intradaily component.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Expand seas conformably to mu
  seas <- .expand(flt$seas, calendar$binL, calendar$bin)

  #### Answer
  seas * flt$mu
}
# ------------------------------------------------------------------------------


.condMean <-
function(flt, calendar)
{
  ##############################################################################
  ## Description:
  ##  Compute the conditional mean in the daily-intradaily MEM.
  ##
  ## Arguments:
  ##  flt: (list) including the following components:
  ##   $eta: (numeric) daily component.
  ##   $seas: (numeric) intradaily periodic component.
  ##   $mu: (numeric) intradaily non periodic component.
  ##  calendar: (list) including the following components:
  ##   $dateL: (numeric) whole vector of dates.
  ##   $date: (numeric) vector of dates.
  ##   $binL: (numeric) whole vector of bins.
  ##   $bin: (numeric) vector of bins.
  ##
  ## Value:
  ##  (numeric) values of the conditional mean.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Expand eta and seas conformably to mu
  eta  <- .expand(flt$eta , calendar$dateL, calendar$date)
  seas <- .expand(flt$seas, calendar$binL, calendar$bin)

  #### Answer
  eta * seas * flt$mu
}
# ------------------------------------------------------------------------------

