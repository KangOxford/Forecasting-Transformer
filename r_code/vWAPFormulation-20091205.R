################################################################################
##
## File: vWAPFormulation-20091205.R
##
## Purpose:
##  R functions for model handling about daily-intradaily data.
##
## Created: 2008.10.10
##
## Version: 2009.12.05
##
## Author:
##  Fabrizio Cipollini
##
################################################################################

################################################################################
## FUNCTION:                       TASK:
##  .constants()                    Set some global constants.
##  .extract.model()                Extract model formulation from the
##                                   original 'model' input.
##  .check.model.internal()         Internal check of 'model' formulation
##                                   relative to the 'mu' components.
##  .ind.sort.model()               Compute row numbers for sorting the 'model'
##                                   formulation in a some way.
##  .sort.model()                   Sort the 'model' formulation in a particular
##                                   way.
##  .convert.parms()                Converts 'parm' from character to numeric
##                                   and viz.
##  .make.parNames()                Build parameter names from the 'model'
##                                   formulation.
##  .compose.parNames()             Compose parameter names.
##  .extractRows()                  Extract row numbers of the parameters,
##                                   by type, from the whole vector.
##  .diControl()                    Adjust control settings of the di-MEM.
##
################################################################################


.constants <-
function()
{
  ##############################################################################
  ## Description:
  ##  Set some global constants.
  ##
  ## Arguments:
  ##  NONE
  ##
  ## Value:
  ##  NONE
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Set 'model' constants
  ##          parms   parmsN   parmIter
  x <- c(  "omegaE",       1,         1,
           "deltaE",       2,         1,
           "alphaE",       3,         1,
           "gammaE",       4,         1,
            "betaE",       5,         1,
           "omegaM",      11,         0,
           "deltaM",      12,         1,
           "alphaM",      13,         1,
           "gammaM",      14,         1,
            "betaM",      15,         1,
            "seasD",      21,         1,
            "seasF",      31,         1,
            "sigma",      51,         0)
  x <- matrix(x, ncol = 3, byrow = TRUE)

  #### Vector of parameters
  ...parms.C  <<- as.character( x[,1] )
  ...parms.N  <<- as.numeric( x[,2] )

  #### Single parameters
  ind <- ...parms.C == "omegaM"
  ...omegaM.N  <<- ...parms.N[ind]
  ...omegaM.C  <<- ...parms.C[ind]
  ind <- ...parms.C == "omegaE"
  ...omegaE.N  <<- ...parms.N[ind]
  ...omegaE.C  <<- ...parms.C[ind]
  
  ...deltaE.N  <<- ...parms.N[...parms.C == "deltaE"]
  ...alphaE.N  <<- ...parms.N[...parms.C == "alphaE"]
  ...gammaE.N  <<- ...parms.N[...parms.C == "gammaE"]
  ...betaE.N   <<- ...parms.N[...parms.C == "betaE" ]
  ...parmsE.N  <<- c(...omegaE.N, ...deltaE.N, ...alphaE.N, ...gammaE.N, ...betaE.N)

  ...deltaM.N  <<- ...parms.N[...parms.C == "deltaM"]
  ...alphaM.N  <<- ...parms.N[...parms.C == "alphaM"]
  ...gammaM.N  <<- ...parms.N[...parms.C == "gammaM"]
  ...betaM.N   <<- ...parms.N[...parms.C == "betaM" ]
  ...parmsM.N  <<- c(...omegaM.N, ...deltaM.N, ...alphaM.N, ...gammaM.N, ...betaM.N)
  
  ...seasD.N   <<- ...parms.N[...parms.C == "seasD"]
  ...seasF.N   <<- ...parms.N[...parms.C == "seasF"]
  ...seas.N    <<- c(...seasD.N, ...seasF.N)

  ind <- ...parms.C == "sigma"
  ...sigma.N   <<- ...parms.N[ind]
  ...sigma.C   <<- ...parms.C[ind]

  #### Answer
  NULL
}
# ------------------------------------------------------------------------------


.extract.model <-
function(x)
{
  ##############################################################################
  ## Description:
  ##  Extract model formulation from the original 'model' input.
  ##
  ## Arguments:
  ##  x: (matrix) with 2 columns:
  ##   - x[,"parm"] includes 'model' formulation. Elements of x have structure
  ##     "parm[l]" where
  ##      - 'parm' is one of the proper parameter names;
  ##      - 'l' means the lag (or the order for other predetermined vars).
  ##     Symbols '[', ']', ',' can be replaced by any punctuation character.
  ##   - x[,"start"] includes starting values.
  ##
  ## Value:
  ##  (matrix) representing 'model' formulation with columns:
  ##   - 'parm': parameter code
  ##   - 'lag': lag
  ##   - 'start': starting values
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Add colnames if missing, check colnames
  ##############################################################################

  #### Checks if there is enough information
  if (NCOL(x) != 2)
  {
     stop("Not enough information for building a model formulation")
  }
  else
  {
    ## proper colnames
    proper  <- c("parm", "start")

    ## If colnames are missing store them
    if (is.null(colnames(x)))
    {
       colnames(x) <- proper
    }

    ## Remove not proper colnames
    ind  <- colnames(x) %in% proper
    if (!all(ind))
    {
       out <- paste("'", colnames(x)[!ind], "'", sep = "")
       warning("The following columns of 'model' are not proper: ", out,
               "they have been removed")
       x   <- x[, ind, drop = FALSE]
    }

    ## Remove duplicated colnames
    ind <- duplicated(colnames(x))
    if (any(ind))
    {
       x  <- x[,!ind]
    }
  }


  ##############################################################################
  ## Part 2: Select
  ##############################################################################

  if (NCOL(x) != 2)
  {
     stop("Not enough information for building a model formulation")
  }
  else
  {
     x1 <- x[,"parm"]
     startx <- x[,"start"]
  }


  ##############################################################################
  ## Part 3: Split information included into the string "parm[l]"
  ##############################################################################

  #### Split information in the parameter
  x1     <- gsub(pattern = "[[:blank:]]", replacement = "", x = x1)
  x.list <- strsplit(x = x1, split = c("[[:punct:]]"))

  #### Transforms 'parm' and 'lag' into vectors
  ## Initialize
  np    <- NROW(x1)
  parmx <- character(np)
  lagx  <- character(np)
  ## Cycle
  for (i in 1:np)
  {
    ## Extract i-th
    x.i      <- x.list[[i]]
    ## Dimension
    n.i      <- NROW(x.i)
    ## parm
    parmx[i] <- x.i[1]
    ## lag
    lagx[i]  <- ifelse(n.i > 1, x.i[2], 0)
    lagx[i]  <- ifelse(lagx[i] == "", 0, lagx[i])
  }

  #### Recode 'parm' to numeric values
  parmx <- .convert.parms(parmx)

  #### Compose
  x <- cbind(parm = parmx, lag = lagx, start = startx)
  x <- apply(x, 2, as.numeric)


  ##############################################################################
  ## Part 4: Answer
  ##############################################################################

  x
}
# ------------------------------------------------------------------------------


.check.model.internal <-
function(x)
{
  ##############################################################################
  ## Description:
  ##  Internal check of 'model' formulation.
  ##  The following points are checked:
  ##  1. Checks if only valid parameters are set (not valid are removed).
  ##  2. Checks lag values (only integer and positive values).
  ##  3. Checks for duplicated rows (are removed).
  ##
  ## Arguments:
  ##  x: (matrix) 'model' formulation with, at least, columns "parm", "lag".
  ##
  ## Value:
  ##  (matrix) checked (and perhaps reviewed) 'model' formulation.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Checks column 'parm'
  ##############################################################################

  #### Valid parameters only
  parmx <- x[,"parm"]
  ind <- is.na(parmx) | !(parmx %in% ...parms.N)
  if (any(ind))
  {
      x <- x[!ind, , drop = FALSE]
      warning("Some not valid parameters have been removed.")
      parmx <- x[,"parm"]
  }

  #### Iterative parameters only
  ind <- parmx %in% c(...sigma.N)
  if (any(ind))
  {
      x <- x[!ind, , drop = FALSE]
      warning("Parameters estimated in a non iterative way have been removed.")
      parmx <- x[,"parm"]
  }

  #### No both kind of seasonal parameters
  ind <- any( parmx == ...seasD.N) & any( parmx == ...seasF.N)
  if (ind)
  {
      stop("Different types of 'seas' parameters are not permitted")
  }


  ##############################################################################
  ## Part 2: Checks columns 'lag'
  ##############################################################################

  #### Reading
  lagx  <- x[,"lag"]

  #### 'lag'
  ## 1) 'lag' zero
  ind  <- parmx %in% c(...omegaE.N, ...omegaM.N)
  if (any(ind))
  {
     lagx[ind] <- 0
  }
  ## 2) 'lag' mandatory
  ind <- !ind
  ind1 <- lagx[ind] == 0
  if (any(ind1))
  {
     ind <- ind & lagx == 0
     parmx1 <- unique( .convert.parms(parmx[ind]) )
     stop("Some 'lag' values relative to parameters ", parmx1, " are 0.")
  }

  #### Restore
  x[,"lag"]  <- lagx


  ##############################################################################
  ## Part 3: Check if there are duplicated rows
  ##############################################################################

  #### Select columns
  ind    <- colnames(x) %in% c("parm", "lag")
  temp   <- x[, ind]

  ## Find and remove duplicated rows
  ind       <- duplicated(temp)
  if (any(ind))
  {
      x <- x[!ind,]
      warning("Some duplicated rows of model have been removed.")
  }


  ##############################################################################
  ## Part 4: Answer
  ##############################################################################

  x
}
# ------------------------------------------------------------------------------


.check.model.vs.diControl <-
function(x, diControl)
{
  ##############################################################################
  ## Description:
  ##  The following points are checked:
  ##  1. Checks if the number of seas parameters is consistent with nBin.
  ##
  ## Arguments:
  ##  x: (matrix) 'model' formulation with, at least, columns "parm", "lag".
  ##  diControl: (list) 'diControl' settings with, including the following component:
  ##   $nBin: (numeric) number of bins in the data
  ##
  ## Value:
  ##  (matrix) checked (and perhaps reviewed) 'model' formulation.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Checks column 'parm'
  ##############################################################################

  #### Seas parameters must have an index <= nBin
  ## Settings
  parmx <- x[,"parm"]
  lagx  <- x[,"lag"]
  nBin  <- diControl$nBin
  ## Find
  ind   <- parmx %in% ...seas.N & lagx >= nBin
  if (any(ind))
  {
      x <- x[!ind, , drop = FALSE]
      warning("'seas' parameters with index >= 'nBin' have been removed")
  }


  ##############################################################################
  ## Part 2: Answer
  ##############################################################################

  x
}
# ------------------------------------------------------------------------------


.ind.sort.model <-
function(x, vars)
{
  ##############################################################################
  ## Description:
  ##  Compute row numbers for sorting the model formulation in a some way.
  ##  The model can be sorted on the basis of one or more of the following
  ##  columns: "parm", "lag".
  ##
  ## Arguments:
  ##  x: (matrix) 'model' formulation
  ##  vars: (character) variables with respect to which 'x' is sorted. One or
  ##   more among "parm", "lag".
  ##
  ## Value:
  ##  (matrix) sorted 'model' formulation
  ##
  ## Remark:
  ##  This function is implemented so as functions exploiting the order of
  ##  the components of 'model' are consistent among them.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 0: First check: how many rows for x?
  ##############################################################################

  nr     <- NROW(x)
  if (nr <= 1)
  {
    return(x)
  }


  ##############################################################################
  ## Part 1: Variables
  ##############################################################################

  ## Remove bad cases from 'vars'
  vars   <- unique(vars)
  vars   <- vars[vars %in% c("parm", "lag")]


  ##############################################################################
  ## Part 2: Organize data
  ##############################################################################

  ## Initialize
  rx <- NROW(x)
  rv <- NROW(vars)
  x1 <- matrix(0, nrow = rx, ncol = rv)
  colnames(x1) <- vars

  ## 'code'
  if ( "parm" %in% vars )
  {
     x1[,"parm"] <- x[,"parm"]
  }

  ## 'lag'
  if ( "lag" %in% vars )
  {
     x1[,"lag"]  <- x[,"lag"]
  }


  ##############################################################################
  ## Part 3: Sorting
  ##############################################################################

  ind <- paste("order(", paste("x1[,", 1:rv, "]", sep = "", collapse = ", "), ")")
  ind <- eval( parse( text = ind ) )


  ##############################################################################
  ## Part 4: Answer
  ##############################################################################

  ind
}
# ------------------------------------------------------------------------------


.sort.model <-
function(x, vars)
{
  ##############################################################################
  ## Description:
  ##  Sort 'model' in a particular way. The model can be sorted on the basis
  ##  of one or more of the following variables: 'parm', 'lag'.
  ##
  ## Arguments:
  ##  x: (data.frame) 'model' formulation
  ##  vars: (character) variables with respect to which 'x' is sorted. One or
  ##   more among "parm", "lag".
  ##
  ## Value:
  ##  (matrix) sorted 'model' formulation
  ##
  ## Remark:
  ##  This function is implemented so as functions exploiting the order of
  ##  the components of 'model' are consistent among them.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  x[.ind.sort.model(x, vars),]
}
# ------------------------------------------------------------------------------


.convert.parms <-
function(x)
{
  ##############################################################################
  ## Description:
  ##  Converts 'parm' types from character to numeric and viz.
  ##
  ## Arguments:
  ##  x: (numeric or character) 'parm' as codes (if numeric) or names (if
  ##   character).
  ##
  ## Value:
  ##  (numeric or character) converted values.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Act, checking type of the argument
  if (is.numeric(x))
  {
    x <- .factor(x = x, levels = ...parms.N, labels = ...parms.C, typeNum = FALSE)
  }
  else if (is.character(x))
  {
    x <- .factor(x = x, levels = ...parms.C, labels = ...parms.N, typeNum = TRUE)
  }
  else
  {
    stop("Argument 'x' must be numeric or character.")
  }
}
# ------------------------------------------------------------------------------


.make.parNames <-
function(parm, lag)
{
  ##############################################################################
  ## Description:
  ##  Retrieve parameter names from the 'model' formulation. They are built as:
  ##  - 'name'      for parameters with no indices
  ##  - 'name[lag]' for parameters with 1 index
  ##
  ## Arguments:
  ##  parm: (numeric) parameter types as numeric codes
  ##  lag: (numeric) lag for each parameter
  ##
  ## Value:
  ##  out: (character) parameter names
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 0: Initialize
  ##############################################################################

  #### Initialize
  out  <- character( NROW(parm) )

  #### Group parameters on the basis of the information used in building names:
  ## 0, 1 indices
  parms0 <- c(...omegaE.N, ...omegaM.N, ...sigma.N)
  parms1 <- c(...deltaE.N, ...alphaE.N, ...gammaE.N, ...betaE.N,
              ...deltaM.N, ...alphaM.N, ...gammaM.N, ...betaM.N,
              ...seas.N)

  #### 'parm' as character
  parmC <- .convert.parms(parm)

  #### other info
  x  <- lag


  ##############################################################################
  ## Part 1: Parameters without indices
  ##############################################################################

  ind  <- parm %in% parms0
  if (any(ind))
  {
     out[ind] <- parmC[ind]
  }


  ##############################################################################
  ## Part 2: Parameters with 1 index only
  ##############################################################################

  ind  <- parm %in% parms1
  if (any(ind))
  {
     out[ind] <- .compose.parNames(names = parmC[ind], info = x[ind])
  }


  ##############################################################################
  ## Part 5: Answer
  ##############################################################################

  out
}
# ------------------------------------------------------------------------------


.compose.parNames <-
function(names, info)
{
  ##############################################################################
  ## Description:
  ##  Compose parameter names. It is an auxiliary function of .make.parNames
  ##
  ## Arguments:
  ##  names: (character) vector of parameter names;
  ##  info: (character)  matrix of further pieces of information.
  ##
  ## Value:
  ##  out (character): vector of parameter names
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION

  ## Settings
  startChar <- "["
  endChar   <- "]"
  info      <- as.matrix( info )
  nc        <- NCOL(info)

  ## Initialize
  out  <- paste( names, startChar, info[,1], sep = "")

  ## Add other info
  i <- 2
  while (i <= nc)
  {
    out  <- paste( out, info[,i], sep = ",")
    i    <- i+1
  }

  ## End
  paste( out, endChar, sep = "")
}
# ------------------------------------------------------------------------------


.diControl <-
function(control)
{
  ##############################################################################
  ## Description:
  ##  Adjust 'diControl' settings as far as concerns the model part.
  ##  Estimation settings are adjusted by functions .nrControl() and
  ##  .nmControl().
  ##
  ## Arguments:
  ##  x: (list) with components:
  ##   $method: (character) set the task accomplished by the main function. One
  ##    among:
  ##    - "settings": only control settings are returned;
  ##    - "simulation": simulated data are returned;
  ##    - "estimation": (default) inferences from the daily-intradaily MEM are
  ##      returned.
  ##    - "forecast": forecasts from the daily-intradaily MEM are returned.
  ##   $nBin: (numeric) number of equally spaced daily bins.
  ##   $intraAdj: (logical) type of initialization of the intradaily component.
  ##    In general muLag(i,0) = muLag(i-1,J), but this scheme can be changed by
  ##    setting intraAdj = TRUE (default), meaning that components 1-min(J, L) of
  ##    xLag(i,0) are replaced by the starting value mu(0) (usually 1);
  ##   $penCoeff: (numeric) penalty coefficient imposed on the optimized
  ##    function.
  ##   $nDBurn: (numeric) number of burning days (default = 1000). Used only
  ##    in simulations.
  ##   $eta0: (numeric) Initialization of the 'daily' filter. Used only in
  ##    simulations.
  ##   $nDFor: (numeric) days at which to make forecasts. Used only in
  ##    forecasts. Can be set to a scalar (single day of forecasts) or to a
  ##    2-dimensional vector (denoting a range of days at which to make
  ##    forecasts).
  ##
  ## Value:
  ##  (list): control settings.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION

  ##############################################################################
  ## Part 1: General options.
  ##############################################################################

  #### 'method' is the task.
  ## default value
  def <- c("estimation", "simulation", "settings", "forecast")
  ## Set value
  tmp <- control$method[1]
  ## Adjust
  control$method <- ifelse (NROW(tmp) > 0 && tmp %in% def, tmp, def[1])

  #### 'nBin' is the number of intradaily bins.
  ## default value
  def <- NA
  ## Set value
  tmp <- control$nBin
  ## Adjust
  if (NROW(tmp) > 0 && tmp > 0)
  {
    control$nBin <- as.integer(tmp)
  }
  else
  {
    stop("The '$nBin' component of 'control' must be set to a positive integer")
  }

  #### 'intraAdj' indicates how muLag are initialized at each day.
  ## default value
  def <- TRUE
  ## Set value
  tmp <- control$intraAdj[1]
  ## Adjust
  control$intraAdj <- ifelse(NROW(tmp) > 0, as.logical(tmp), def)

  #### 'penCoeff' indicates the coefficient of the penalty imposed on the
  ## optimized function.
  ## default value
  def <- 0
  ## Set value
  tmp <- control$penCoeff[1]
  ## Adjust
  control$penCoeff <- ifelse( NROW(tmp) > 0 && tmp >= def, tmp, 0 )
  

  ##############################################################################
  ## Part 3: Simulation options.
  ##############################################################################

  #### 'nDBurn' is the number of burning days in simulations
  ## default value
  def <- 1000
  ## Set value
  tmp <- as.numeric(control$nDBurn[1])
  ## Adjust
  control$nDBurn <- ifelse (NROW(tmp) > 0 && tmp >= 0, as.integer(tmp), def)

  #### 'eta0' means the starting value of daily component
  ## default value
  def <- NA
  ## Set value
  tmp <- as.numeric(control$eta0[1])
  ## Adjust
  if (control$method == "simulation")
  {
    if(NROW(tmp) > 0 && tmp > 0)
    {
      control$eta0 <- tmp
    }
    else
    {
      stop("The '$eta0' component of 'control' must be set to a positive value",
           "when $method == \"simulation\"")
    }
  }
  else
  {
    control$eta0 <- NA
  }


  ##############################################################################
  ## Part 4: Forecast options.
  ##############################################################################

  #### 'nDFor' means days at which to make forecasts
  ## default value
  def <- NA
  ## Set value
  tmp <- control$nDFor
  ## Adjust
  if (control$method == "forecast")
  {
    ## Not more than 2 elements
    if ( NROW(tmp) > 2 )
    {
      tmp <- tmp[1:2]
    }
    ## Only integer entries
    tmp <- as.integer(tmp)
    ## Only positive entries
    tmp <- tmp[tmp > 0]
    ## Set
    if( NROW(tmp) > 0 )
    {
      tmp <- range(tmp)
      tmp <- rep(tmp, length.out = 2)
      control$nDFor <- rep(tmp, length.out = 2)
    }
    else
    {
      stop("The '$nDFor' component of 'control' must be set to a positive",
           "values when $method == \"forecast\"")
    }
  }
  else
  {
    control$nDFor <- NA
  }


  ##############################################################################
  ## Part 5: Answer.
  ##############################################################################

  control
}
# ------------------------------------------------------------------------------
