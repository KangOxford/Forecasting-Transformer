################################################################################
##
## File: vWAPFiltering-20091205.R
##
## Purpose:
##  R functions for filtering in the daily-intradaily MEM.
##
## Created: 2008.10.10
##
## Version: 2009.12.05
##
## Author:
##  Fabrizio Cipollini <cipollini@ds.unifi.it>
##
################################################################################

################################################################################
## FUNCTION:                       TASK:
##
## PART 0: Basic FUNCTIONS for filter updating
##  .update.MEM()                   One-step update of the filter of one
##                                   component of the di-MEM model.
##  .updateD.MEM()                  One-step update of filter and derivative of
##                                   one component of the di-MEM model.
##
## PART 1: General FUNCTIONS for filtering within the daily-intradaily MEM.
##  .filter.diMEM.1()               Filter within the daily-intraDaily model.
##  .fgh.diMEM.1()                  Compute values of the (pseudo-scaled)
##                                   criterion function, gradient, Hessian in
##                                   the di-MEM. As a subproduct computes also
##                                   lagged values of quantities involved in the
##                                   filter and their first derivatives.
##  .flt0.diMEM()                   Build components of the filter at the
##                                   starting time.
##  .fltLag0.diMEM()                Build lagged components of the filter at the
##                                   starting time.
##  .flt.diMEM()                    Retrieve the current value of the filtered
##                                   quantities from the lagged values.
##  .fltD0.diMEM()                  Build components of the filter their
##                                   derivatives at the starting time.
##  .fltDLag0.diMEM()               Build lagged components of the filter and
##                                   their derivatives at the starting time.
##  .fltD.diMEM()                   Retrieve the current value of the filtered
##                                   quantities and their derivatives from the
##                                   lagged values.
##  .adjIntraLag()                  Adjust the intradaily lagged component
##                                   xLag(i,j).
##  .adjDerIntraLag()               Adjust the intradaily lagged component
##                                   xLag(i,j) and its derivative xDLag(i,j).
##
## PART 2: FUNCTIONS for filtering within the daily-intradaily MEM.
##  .filter.diMEM()                 Filter daily ('eta') periodic ('seas') and
##                                   intradaily ('mu') components in the
##                                   di-MEM
##  .fgh.diMEM()                    Compute values of the (pseudo-scaled)
##                                   criterion function, gradient, Hessian in
##                                   the di-MEM.
##  .f.diMEM()                      Computes the values of the (pseudo-scaled)
##                                   criterion function in the di-MEM.
##
## PART 3: General FUNCTIONS for filtering within the MEM.
##  .filter.MEM.1()                 Filter within the MEM.
##  .fgh.MEM.1()                    Compute values of the (pseudo-scaled)
##                                   criterion function, gradient, Hessian in
##                                   the MEM. As a subproduct computes also
##                                   lagged values of quantities involved in the
##                                   filter and their first derivatives.
##  .flt0.MEM()                     Build components of the filter at the
##                                   starting time.
##  .fltLag0.MEM()                  Build lagged components of the filter at the
##                                   starting time.
##  .flt.MEM()                      Retrieve the current value of the filtered
##                                   quantities from the lagged values.
##  .fltD0.MEM()                    Build components of the filter their
##                                   derivatives at the starting time.
##  .fltDLag0.MEM()                 Build lagged components of the filter and
##                                   their derivatives at the starting time.
##  .fltD.MEM()                     Retrieve the current value of the filtered
##                                   quantities and their derivatives from the
##                                   lagged values.
##
## PART 4: FUNCTIONS for filtering within the MEM.
##  .filter.MEM()                   Filter in the MEM
##  .fgh.MEM()                      Compute values of the (pseudo-scaled)
##                                   criterion function, gradient, Hessian in
##                                   the MEM.
##  .f.MEM()                        Computes the values of the (pseudo-scaled)
##                                   criterion function in the MEM.
##
## PART 6: FUNCTIONS for simulation
##  .r.diMEM.1()                    General function for simulating from the
##                                   di-MEM.
##  .r.diMEM()                      Simulates values form the di-MEM.
##
## PART 7: FUNCTIONS for intradaily periodic component
##
##  .indVarsIP()                    Make independent variables for estimating
##                                   the intradaily periodic component.
##  .indVarsDummies()               Make independent variables for the
##                                   intradaily periodic component expressed via
##                                   dummy variables.
##  .indVarsFourier()               Make independent variables for the
##                                   intradaily periodic component expressed via
##                                   Fourier (sin/cos) variables.
##  .fit.IP()                        Fit intradaily periodic component by using
##                                   OLS on logs.
##  .computeIP()                    Compute the intradaily component for all
##                                   bins of a day.
##  .computedlogIP()                Compute the derivative of the log-seasonal
##                                   component for all bins of a day.
##
## PART 8: FUNCTIONS for utilities
##  .extract.rows()                 Extract positions of the parameters, by
##                                   type, from the whole vector.
##  .infoFilter()                   Extract information for filtering.
##  .extract.infoFilter()           Select information of the given 'type' from
##                                   the whole 'infoFilter'.
##  .parmVal.diMEM()                Extract values of parameters in di-MEM.
##  .parmVal.MEM()                  Extract values of parameters in MEM.
##  .modelType()                    Returns the type of model.
##  .IPType()                       Returns the type of intradaily periodic
##                                   component.
##
################################################################################


################################################################################
## PART 0: Basic FUNCTIONS for filter updating
##  .update.MEM()                   One-step update of the filter of one
##                                   component of the di-MEM.
##  .updateD.MEM()                  One-step update of filter and derivative of
##                                   one component of the di-MEM.
################################################################################


.update.MEM <-
function(z, xL, x1L, fL, lagX, lagX1, lagF, parmO, parmZ, parmX, parmX1, parmF)
{
  ##############################################################################
  ## Description:
  ##  One-step update of the filter of one component of the di-MEM.
  ##
  ## Arguments:
  ##  z: (numeric) predetermined values (dimension maxLagZ).
  ##  xL: (numeric) lagged x values (dimension maxLagX).
  ##  x1L: (numeric) lagged x1 values (dimension maxLagX1).
  ##  fL: (numeric) lagged filtered values (dimension maxLagF).
  ##  lagX: (numeric) lags of xL into parameters.
  ##  lagX1: (numeric) lags of x1L into parameters.
  ##  lagF: (numeric) lags of fL into parameters.
  ##  parmO: (numeric) omega (dimension 1).
  ##  parmZ: (numeric) parameters corresponding to z[lagZ]
  ##  parmX: (numeric) parameters corresponding to xL[lagX].
  ##  parmX1: (numeric) parameters corresponding to x1L[lagX1].
  ##  parmF: (numeric) parameters corresponding to fL[lagF].
  ##
  ## Value:
  ##  (numeric) filtered value.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:
  
  #### Answer
  parmO + sum( parmZ * z ) +
          sum( parmX * xL[lagX] ) +
          sum( parmX1 * x1L[lagX1] ) +
          sum( parmF * fL[lagF] )
}
# ------------------------------------------------------------------------------


.updateD.MEM <-
function(z, xL, x1L, fL,  xDL, x1DL, fDL,  lagX, lagX1, lagF,
meanZ, meanX, meanX1, meanF, indVT,
rowsO, rowsZ, rowsX, rowsX1, rowsF, parmO, parmZ, parmX, parmX1, parmF)
{
  ##############################################################################
  ## Description:
  ##  One-step update of filter and derivative of one component of the di-MEM.
  ##
  ## Arguments:
  ##  z: (numeric) predetermined values (dimension maxLagZ).
  ##  xL: (numeric) lagged x values (dimension maxLagX).
  ##  x1L: (numeric) lagged x1 values (dimension maxLagX1).
  ##  fL: (numeric) lagged filtered values (dimension maxLagF).
  ##  xDL: (numeric) derivative of xL (dimension (nPar, maxLagX)).
  ##  x1DL: (numeric) derivative of x1L (dimension (nPar, maxLagX1)).
  ##  fDL: (numeric) derivative of lagged filtered values (dimension
  ##   (nPar, maxLagF)).
  ##  lagX: (numeric) lags of xL into parameters.
  ##  lagX1: (numeric) lags of x1L into parameters.
  ##  lagF: (numeric) lags of fL into parameters.
  ##  meanZ: (numeric) mean(z) (dimension maxLagZ).
  ##  meanX: (numeric) mean(x) (dimension 1).
  ##  meanX1: (numeric) mean(x1) (dimension 1).
  ##  meanF: (numeric) mean(f) (dimension 1).
  ##  indVT: (logical) Variance targeting indicator (dimension 1).
  ##  rowsO: (numeric) row of omega (dimension 1 - indVT)
  ##  rowsZ: (numeric) rows of parameters corresponding to z.
  ##  rowsX: (numeric) rows of parameters corresponding to xL[lagX].
  ##  rowsX1: (numeric) rows of parameters corresponding to x1L[lagX1].
  ##  rowsF: (numeric) rows of parameters corresponding to fL[lagF].
  ##  parmO: (numeric) omega (dimension 1).
  ##  parmZ: (numeric) parameters corresponding to z[lagZ]
  ##  parmX: (numeric) parameters corresponding to xL[lagX].
  ##  parmX1: (numeric) parameters corresponding to x1L[lagX1].
  ##  parmF: (numeric) parameters corresponding to fL[lagF].
  ##
  ## Value:
  ##  (list) with components
  ##  $f: (numeric) filtered value.
  ##  $fD: (numeric) derivative of the filtered value.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Select
  ##############################################################################

  #### Select
  xLs  <- xL[lagX]
  x1Ls <- x1L[lagX1]
  fLs  <- fL[lagF]


  ##############################################################################
  ## Part 2: f
  ##############################################################################

  ####
  f  <- parmO + sum( parmZ * z ) +
								sum( parmX * xL[lagX] ) +
								sum( parmX1 * x1L[lagX1] ) +
								sum( parmF * fL[lagF] )

  ##############################################################################
  ## Part 3: fD
  ##############################################################################

  #### Initialize
  fD <- xDL[, lagX, drop = FALSE] %*% parmX +
        x1DL[, lagX1, drop = FALSE] %*% parmX1 +
        fDL[, lagF, drop = FALSE] %*% parmF

  #### Adjust
  if ( indVT )
  {
  	z <- z - meanZ
    xLs  <- xLs - meanX
    x1Ls <- x1Ls - meanX1
    fLs  <- fLs - meanF
  }
  else
  {
    fD[rowsO] <- fD[rowsO] + 1
  }
  
  fD[rowsZ]  <- fD[rowsZ] + z
  fD[rowsX]  <- fD[rowsX] + xLs
  fD[rowsX1] <- fD[rowsX1] + x1Ls
  fD[rowsF]  <- fD[rowsF] + fLs


  ##############################################################################
  ## Part 4: Answer
  ##############################################################################

  list(f = f, fD = fD)
}
# ------------------------------------------------------------------------------


################################################################################
## PART 1: General FUNCTIONS for filtering within the daily-intradaily MEM.
##  .filter.diMEM.1()               Filter within the di-MEM.
##  .fgh.diMEM.1()                  Compute values of the (pseudo-scaled)
##                                   criterion function, gradient, Hessian in
##                                   the di-MEM. As a subproduct computes also
##                                   lagged values of quantities involved in the
##                                   filter and their first derivatives.
##  .flt0.diMEM()                   Build components of the filter at the
##                                   starting time.
##  .fltLag0.diMEM()                Build lagged components of the filter at the
##                                   starting time.
##  .flt.diMEM()                    Retrieve the current value of the filtered
##                                   quantities from the lagged values.
##  .fltD0.diMEM()                  Build components of the filter their
##                                   derivatives at the starting time.
##  .fltDLag0.diMEM()               Build lagged components of the filter and
##                                   their derivatives at the starting time.
##  .fltD.diMEM()                   Retrieve the current value of the filtered
##                                   quantities and their derivatives from the
##                                   lagged values.
##  .adjIntraLag()                  Adjust the intradaily lagged component
##                                   xLag(i,j).
##  .adjDerIntraLag()               Adjust the intradaily lagged component
##                                   xLag(i,j) and its derivative xDLag(i,j).
################################################################################


.filter.diMEM.1 <-
function(parmVal, infoFilter, data, fltLag, flt0, time1)
{
  ##############################################################################
  ## Description:
  ##  Filter within the daily-intraDaily model.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the function .make.data() for details).
  ##  fltLag: (list) current (at time1) lagged filter components. It has
  ##   components:
  ##   $v: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $mu: (numeric)
  ##  flt0: (list) starting (at 0) filter components. It has components:
  ##   $v: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $mu: (numeric)
  ##  time1: (numeric) current time of the filter.
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $filter: (list) filter components, since first = time1 + 1 to
  ##    last = time1 + nobs. It has components:
  ##    $eta: (numeric)
  ##    $seas: (numeric)
  ##    $mu: (numeric)
  ##   $lagged: (list) last (at time1 + nobs) lagged filtered components.
  ##    It has components:
  ##    $v: (numeric)
  ##    $eta: (numeric)
  ##    $w: (numeric)
  ##    $mu: (numeric)
  ##
  ## Details:
  ##  The first and last time of the filter are computed as
  ##   first = time1 + 1, last = time1 + data$nobs.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Parameter and model settings. These are essentially ruled by the
  #### structure of eta(i) and mu(i,j) (see .update.MEM() and .updateD.MEM()).
  ## lag
  lagV      <- infoFilter$lag$v
  lagV1     <- infoFilter$lag$v1
  lagEta    <- infoFilter$lag$eta
  lagW      <- infoFilter$lag$w
  lagW1     <- infoFilter$lag$w1
  lagMu     <- infoFilter$lag$mu
  ## maxLag
  maxLagV   <- infoFilter$maxLag$v
  maxLagV1  <- infoFilter$maxLag$v1
  maxLagEta <- infoFilter$maxLag$eta
  maxLagW   <- infoFilter$maxLag$w
  maxLagW1  <- infoFilter$maxLag$w1
  maxLagMu  <- infoFilter$maxLag$mu
  ## L
  Lw        <- infoFilter$L$w
  Lw1       <- infoFilter$L$w1
  Lmu       <- infoFilter$L$mu
  ## parameters
  parmValL  <- .parmVal.diMEM(parmVal, infoFilter, data)
  omegaE    <- parmValL$omegaE
  deltaE    <- parmValL$deltaE
  alphaE    <- parmValL$alphaE
  gammaE    <- parmValL$gammaE
  betaE     <- parmValL$betaE
  omegaM    <- parmValL$omegaM
  deltaM    <- parmValL$deltaM
  alphaM    <- parmValL$alphaM
  gammaM    <- parmValL$gammaM
  betaM     <- parmValL$betaM
  
  #### diControl settings
  nBin      <- infoFilter$diControl$nBin
  intraAdj  <- infoFilter$diControl$intraAdj
  
  #### Data settings
  x         <- as.numeric(data$x)
  nX        <- NROW(x)
  indRet    <- as.numeric(data$indRet)
  indDRet   <- data$indDRet
  zE        <- data$zE
  zM        <- matrix(data = data$zM, nrow = nX, ncol = NCOL(data$zM)) 

  #### Time settings
  tim1      <- .time( list(time = time1 +  1), nBin )
  tim2      <- .time( list(time = time1 + nX), nBin )
  nDay      <- (tim2$day - tim1$day) + 1


  ##############################################################################
  ## Part 2: Seasonal component
  ##############################################################################

  #### Compute
  seas <- .computeIP(parmVal, infoFilter)


  ##############################################################################
  ## Part 3: Initialize filter
  ##############################################################################

  #### Starting values 'v0', 'v10', 'eta0', 'w0', 'w10', 'mu0'
  v0      <- flt0$v
  v10     <- flt0$v1
  eta0    <- flt0$eta
  w0      <- flt0$w
  w10     <- flt0$w1
  mu0     <- flt0$mu

  #### Lagged 'vL', 'v1L', 'etaL', 'wL', 'w1L', 'muL'
  vL      <- fltLag$v
  v1L     <- fltLag$v1
  etaL    <- fltLag$eta
  wL      <- fltLag$w
  w1L     <- fltLag$w1
  muL     <- fltLag$mu

  #### Current 'v', 'v1', 'eta', 'w', 'w1', 'mu'
  flt     <- .flt.diMEM(fltLag)
  v       <- flt$v
  v1      <- flt$v1
  eta     <- flt$eta
  w       <- flt$w
  w1      <- flt$w1
  mu      <- flt$mu

  #### Initialize output
  etaStore <- numeric(nDay)
  muStore  <- numeric(nX)


  ##############################################################################
  ## Part 4: Filter
  ##############################################################################

  #### Current time settings
  j   <- tim1$bin
  i   <- 0

  #### Save 'eta' if j > 1 (i.e. not at first bin of the day)
  if (j > 1)
  {
    #### Update day
    i <- i + 1

    #### Store
    etaStore[i] <- eta
  }

  #### Cycle
  for (t1 in 1:nX)
  {
    #### Update 'eta' if j = 1 (i.e. at first bin of the day)
    if (j == 1)
    {
      #### Update day
      i <- i + 1

      #### Current eta
      eta <- .update.MEM(zE[i,], vL, v1L, etaL, lagV, lagV1, lagEta,
        omegaE, deltaE, alphaE, gammaE, betaE)

      #### Store
      etaStore[i] <- eta

      #### Update etaL
      etaL <- c(eta, etaL[-maxLagEta])

      #### Initialize v
      v   <- 0
    }
    
    #### Current mu
    ## Adjust lagged values
    wLs  <- .adjIntraLag(j, Lw , wL , w0 , intraAdj)
    w1Ls <- .adjIntraLag(j, Lw1, w1L, w10, intraAdj)
    muLs <- .adjIntraLag(j, Lmu, muL, mu0, intraAdj)
    ## Current mu
    mu <- .update.MEM(zM[t1,], wLs, w1Ls, muLs, lagW, lagW1, lagMu,
      omegaM, deltaM, alphaM, gammaM, betaM)

    #### Store
    muStore[t1] <- mu

    ## Current seas
    sj <- seas[j]

    #### Current observations
    xj <- x[t1]
    if (is.na(xj))
    {
      xj <- sj * eta * mu
    }

    #### Update for v; Current w
    v  <- v + xj / (sj * mu)
    w  <- xj / (sj * eta)
    indRetj <- indRet[t1]
    w1 <- w * indRetj

    #### Update lagged w and mu (wL, muL)    
    wL  <- c(w, wL[-maxLagW])
    w1L <- c(w1, w1L[-maxLagW1])
    muL <- c(mu, muL[-maxLagMu])

    #### Update 'etaL' and 'vL' if j = nBin (i.e. at last bin of the day)
    if (j < nBin)
    {
      #### Update j
      j <- j + 1

      #### Update first component of vL
      vL[1] <- v
    }
    else
    {
      #### Current v, v1
      v  <- v / nBin
      v1 <- v * indDRet[i]

      #### Update vL
      vL  <- c(v, vL[-maxLagV])
      v1L <- c(v1, v1L[-maxLagV1])

      #### Update j
      j <- 1
    }
  }


  ##############################################################################
  ## Part 5: Answer
  ##############################################################################

  list( filter = list(eta = etaStore, seas = seas, mu = muStore),
        lagged = list(v = vL, v1 = v1L, eta = etaL,
                      w = wL, w1 = w1L, mu = muL) )
}
# ------------------------------------------------------------------------------


.fgh.diMEM.1 <-
function(parmVal, infoFilter, data, fltDLag, fltD0, time1)
{
  ##############################################################################
  ## Description:
  ##  Computes the values of the following quantities in the
  ##  daily-intradaily MEM:
  ##  - scaled-pseudo-objective function: f = g'g / 2
  ##  - scaled-pseudo-gradient:           g = sum_{t = 1}^T a(t) u(t)
  ##  - scaled-pseudo-Hessian:            H = sum_{t = 1}^T a(t) a(t)'
  ##  The not scaled pseudo-objective function, pseudo-gradient and
  ##  pseudo-Hessian are, respectively,
  ##  - fn = sigma^{-4} f
  ##  - gn = sigma^{-2} g
  ##  - Hn = sigma^{-2} H.
  ##  As a subproduct computes also lagged values of quantities involved in the
  ##  filter and their first derivatives.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the function .data() for details).
  ##  fltDLag: (list) current (at time1) lagged filter components and their
  ##   first derivatives. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##   $wD: (numeric)
  ##   $w1D: (numeric)
  ##   $muD: (numeric)
  ##  fltD0: (list) starting (at 0) filter components ad their first
  ##   derivatives. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##   $wD: (numeric)
  ##   $w1D: (numeric)
  ##   $muD: (numeric)
  ##  time1: (numeric) current time of the filter.
  ##
  ## Value:
  ##  (list) with components:
  ##   $fgh: (list)
  ##    $f: (numeric) scaled pseudo-objective function.
  ##    $g: (numeric) scaled pseudo-gradient.
  ##    $h: (matrix) scaled pseudo-Hessian.
  ##   $lagged: (list)
  ##    $v: (numeric)
  ##    $v1: (numeric)
  ##    $eta: (numeric)
  ##    $w: (numeric)
  ##    $w1: (numeric)
  ##    $mu: (numeric)
  ##    $vD: (numeric)
  ##    $v1D: (numeric)
  ##    $etaD: (numeric)
  ##    $wD: (numeric)
  ##    $w1D: (numeric)
  ##    $muD: (numeric)
  ##
  ## Details:
  ##  The first and last time of the filter are computed as
  ##   first = time1 + 1, last = time1 + data$nobs.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Parameter and model settings. These are essentially ruled by the
  #### structure of eta(i) and mu(i,j) (see .update.MEM() and .updateD.MEM()).
  ## lag
  lagV      <- infoFilter$lag$v
  lagV1     <- infoFilter$lag$v1
  lagEta    <- infoFilter$lag$eta
  lagW      <- infoFilter$lag$w
  lagW1     <- infoFilter$lag$w1
  lagMu     <- infoFilter$lag$mu
  ## maxLag
  maxLagV   <- infoFilter$maxLag$v
  maxLagV1  <- infoFilter$maxLag$v1
  maxLagEta <- infoFilter$maxLag$eta
  maxLagW   <- infoFilter$maxLag$w
  maxLagW1  <- infoFilter$maxLag$w1
  maxLagMu  <- infoFilter$maxLag$mu
  ## L
  Lw        <- infoFilter$L$w
  Lw1       <- infoFilter$L$w1
  Lmu       <- infoFilter$L$mu
  ## parameters
  parmValL  <- .parmVal.diMEM(parmVal, infoFilter, data)
  omegaE    <- parmValL$omegaE
  deltaE    <- parmValL$deltaE
  alphaE    <- parmValL$alphaE
  gammaE    <- parmValL$gammaE
  betaE     <- parmValL$betaE
  omegaM    <- parmValL$omegaM
  deltaM    <- parmValL$deltaM
  alphaM    <- parmValL$alphaM
  gammaM    <- parmValL$gammaM
  betaM     <- parmValL$betaM
  ## Variance targeting indicators
  indVT.E   <- infoFilter$VT$eta
  indVT.M   <- infoFilter$VT$mu
  ## The following settings are not into '.filter.diMEM.1()'
  rowsOmegaE <- infoFilter$rows$omegaE
  rowsDeltaE <- infoFilter$rows$deltaE
  rowsAlphaE <- infoFilter$rows$alphaE
  rowsGammaE <- infoFilter$rows$gammaE
  rowsBetaE  <- infoFilter$rows$betaE
  rowsOmegaM <- infoFilter$rows$omegaM
  rowsDeltaM <- infoFilter$rows$deltaM
  rowsAlphaM <- infoFilter$rows$alphaM
  rowsGammaM <- infoFilter$rows$gammaM
  rowsBetaM  <- infoFilter$rows$betaM
  nPar       <- NROW(parmVal)

  #### diControl settings
  nBin      <- infoFilter$diControl$nBin
  intraAdj  <- infoFilter$diControl$intraAdj

  #### Data settings
  x         <- as.numeric( data$x )
  nX        <- NROW(x)
  indRet    <- as.numeric( data$indRet )
  indDRet   <- data$indDRet
  zE        <- data$zE
  zM        <- matrix(data = data$zM, nrow = nX, ncol = NCOL(data$zM))
  meanZE    <- data$meanZE
  meanV     <- data$meanX
  meanV1    <- meanV / 2
  meanEta   <- meanV
  meanZM    <- data$meanZM
  meanW     <- 1
  meanW1    <- meanW / 2
  meanMu    <- meanW

  #### Time settings
  tim1      <- .time( list(time = time1 +  1), nBin )
  tim2      <- .time( list(time = time1 + nX), nBin )
  nDay      <- (tim2$day - tim1$day) + 1


  ##############################################################################
  ## Part 2: Seasonal component
  ##############################################################################

  #### Compute
  seas  <- .computeIP(parmVal, infoFilter)
  logSD <- .computedlogIP(parmVal, infoFilter)


  ##############################################################################
  ## Part 3: Initialize filter and derivatives
  ##############################################################################

  #### Starting values 'v0', 'v10', 'eta0', 'w0', 'w10', 'mu0' and their
  #### derivatives
  v0      <- fltD0$v
  v10     <- fltD0$v1
  eta0    <- fltD0$eta
  w0      <- fltD0$w
  w10     <- fltD0$w1
  mu0     <- fltD0$mu
  vD0     <- fltD0$vD
  v1D0    <- fltD0$v1D
  etaD0   <- fltD0$etaD
  wD0     <- fltD0$wD
  w1D0    <- fltD0$w1D
  muD0    <- fltD0$muD

  #### Lagged 'vL', 'v1L', 'etaL', 'wL', 'w1L', 'muL' and their derivatives
  vL      <- fltDLag$v
  v1L     <- fltDLag$v1
  etaL    <- fltDLag$eta
  wL      <- fltDLag$w
  w1L     <- fltDLag$w1
  muL     <- fltDLag$mu
  vDL     <- fltDLag$vD
  v1DL    <- fltDLag$v1D
  etaDL   <- fltDLag$etaD
  wDL     <- fltDLag$wD
  w1DL    <- fltDLag$w1D
  muDL    <- fltDLag$muD

  #### Current 'v', 'eta', 'w', 'mu' and their derivatives
  fltD    <- .fltD.diMEM(fltDLag)
  v       <- fltD$v
  v1      <- fltD$v1
  eta     <- fltD$eta
  w       <- fltD$w
  w1      <- fltD$w1
  mu      <- fltD$mu
  vD      <- fltD$vD
  v1D     <- fltD$v1D
  etaD    <- fltD$etaD
  wD      <- fltD$wD
  w1D     <- fltD$w1D
  muD     <- fltD$muD

  #### Initialize u and a for computing moment function and information matrix
  u       <- numeric(nX)
  a       <- matrix(0, nX, nPar)


  ##############################################################################
  ## Part 4: Filter
  ##############################################################################

  #### Current time settings
  j   <- tim1$bin
  i   <- 0

  #### Save 'eta' if j > 1 (i.e. not first bin of the day)
  if (j > 1)
  {
    #### Update day
    i <- i + 1
  }

  #### Cycle
  for (t1 in 1:nX)
  {
    #### Update 'eta' if j = 1 (i.e. at first bin of the day)
    if (j == 1)
    {
      #### Update day
      i <- i + 1

      #### Current eta and etaD
      tmp <- .updateD.MEM(zE[i,], vL, v1L, etaL, vDL, v1DL, etaDL, 
        lagV, lagV1, lagEta,
        meanZE, meanV, meanV1, meanEta, indVT.E,
        rowsOmegaE, rowsDeltaE, rowsAlphaE, rowsGammaE, rowsBetaE,
        omegaE, deltaE, alphaE, gammaE, betaE)
      eta  <- tmp$f
      etaD <- tmp$fD

      #### Update eta, etaD
      etaL  <- c(eta, etaL[-maxLagEta])
      etaDL <- cbind(etaD, etaDL[, -maxLagEta])

      #### Initialize v, vDer
      v  <- 0
      vD <- 0
    }

    #### Current mu and muD
    #### Adjust wL, muL and wDL, muDL
    tmp   <- .adjDerIntraLag(j, Lw, wL, w0, wDL, wD0, intraAdj)
    wLs   <- tmp$x
    wDLs  <- tmp$xD
    tmp   <- .adjDerIntraLag(j, Lw1, w1L, w10, w1DL, w1D0, intraAdj)
    w1Ls  <- tmp$x
    w1DLs <- tmp$xD
    tmp   <- .adjDerIntraLag(j, Lmu, muL, mu0, muDL, muD0, intraAdj)
    muLs  <- tmp$x
    muDLs <- tmp$xD

    #### Current mu and muD
    tmp <- .updateD.MEM(zM[j,], wLs, w1Ls, muLs, wDLs, w1DLs, muDLs, 
      lagW, lagW1, lagMu,
      meanZM, meanW, meanW1, meanMu, indVT.M,
      rowsOmegaM, rowsDeltaM, rowsAlphaM, rowsGammaM, rowsBetaM,
      omegaM, deltaM, alphaM, gammaM, betaM)
    mu   <- tmp$f
    muD  <- tmp$fD

    #### Current seas and logSeasD
    sj     <- seas[j]
    logSDj <- logSD[j,]

    #### Current observation
    xj  <- x[t1]
    ind <- is.na(xj)

    #### Current contribution to vD, wD
    if (!ind)    ## If x[j] is valued...
    {
      vj <- xj / (sj * mu)
      v  <- v + vj
      w  <- xj / (sj * eta)
      vD <- vD - vj * ( logSDj + muD / mu )
      wD <- -w * ( logSDj + etaD / eta )
    }
    else         ## If x[j] is NA...
    {
      #### Correct
      xj <- sj * eta * mu

      vj <- eta
      v  <- v + vj
      w  <- mu
      vD <- vD + etaD
      wD <- muD
    }
    ####
    indRetj <- indRet[t1]
    w1  <- w * indRetj
    w1D <- wD * indRetj

    #### Update lagged w and mu (wL, muL)
    wL   <- c(w, wL[-maxLagW])
    w1L  <- c(w1, w1L[-maxLagW1])
    muL  <- c(mu, muL[-maxLagMu])
    wDL  <- cbind(wD, wDL[, -maxLagW])
    w1DL <- cbind(w1D, w1DL[, -maxLagW1])
    muDL <- cbind(muD, muDL[, -maxLagMu])

    #### Store u, a
    u[t1]  <- xj / (sj * eta * mu) - 1
    a[t1,] <- etaD / eta + logSDj + muD / mu

    #### Update 'etaL' and 'vL' if j = nBin (i.e. at last bin of the day)
    if (j < nBin)
    {
      #### Update j
      j <- j + 1

      #### Update first element of vL, vDL
      vL[1]  <- v
      v1L[1] <- v1
      vDL[,1]  <- vD
      v1DL[,1] <- v1D
    }
    else
    {
      #### Current v, vD
      indDReti <- indDRet[i]
      v   <- v / nBin
      v1  <- v * indDReti                          ## Use the last bin of the day
      vD  <- vD / nBin
      v1D <- vD * indDReti                         ## Use the last bin of the day

      #### Update vL, vDL
      vL  <- c(v, vL[-maxLagV])
      v1L <- c(v1, v1L[-maxLagV1])
      vDL  <- cbind(vD, vDL[, -maxLagV])
      v1DL <- cbind(v1D, v1DL[, -maxLagV1])

      #### Update j
      j <- 1
    }
  }


  ##############################################################################
  ## Part 5: Computes useful quantities for optimization.
  ##############################################################################

  #### Scaled-pseudo-gradient (constant sigma^(-2) removed)
  g <- colSums(a * u)

  #### Scaled-pseudo-Hessian (constant sigma^(-2) removed)
  H <- -crossprod(a)

  #### Pseudo-objective function
  f <- 0.5 * sum(g*g)


  ##############################################################################
  ## Part 6: Answer
  ##############################################################################

  list(fgh = list(f = f, g = g, h = H),
       lagged = list(v = vL, v1 = v1L, eta = etaL,
                     w = wL, w1 = w1L, mu = muL,
                     vD = vDL, v1D = v1DL, etaD = etaDL,
                     wD = wDL, w1D = w1DL, muD = muDL) )
}
# ------------------------------------------------------------------------------


.flt0.diMEM <-
function(eta0, mu0)
{
  ##############################################################################
  ## Description:
  ##  Build components of the filter at the starting time.
  ##
  ## Arguments:
  ##  eta0: (numeric) starting daily component.
  ##  mu0: (numeric) starting intradaily component.
  ##
  ## Value:
  ##  (list) Starting filter components. It has components:
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

  #### Answer
  list(v = eta0, v1 = eta0/2, eta = eta0, w = mu0, w1 = mu0/2, mu = mu0)
}
# ------------------------------------------------------------------------------


.fltLag0.diMEM <-
function(x0, maxLag)
{
  ##############################################################################
  ## Description:
  ##  Build lagged components of the filter at the starting time.
  ##
  ## Arguments:
  ##  x0: (list) starting filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##  maxLag: (list) maximum lag of the filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##
  ## Value:
  ##  (list) Starting lagged filter components. It has components:
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
  
  #### Answer
  list(
       v = rep.int(x0$v, maxLag$v), v1 = rep.int(x0$v1, maxLag$v1),
       eta = rep.int(x0$eta, maxLag$eta),
       w = rep.int(x0$w, maxLag$w), w1 = rep.int(x0$w1, maxLag$w1),
       mu = rep.int(x0$mu,  maxLag$mu)
       )
}
# ------------------------------------------------------------------------------


.flt.diMEM <-
function(xLag)
{
  ##############################################################################
  ## Description:
  ##  Retrieve the current value of the filtered quantities from the lagged
  ##  values.
  ##
  ## Arguments:
  ##  xLag: (list) starting filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##
  ## Value:
  ##  (list) Current filter components. It has components:
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

  #### Answer
  list(v = xLag$v[1], v1 = xLag$v1[1], eta = xLag$eta[1],
       w = xLag$w[1], w1 = xLag$w1[1], mu = xLag$mu[1])
}
# ------------------------------------------------------------------------------


.fltD0.diMEM <-
function(eta0, mu0, etaD0, muD0)
{
  ##############################################################################
  ## Description:
  ##  Build components of the filter their first derivatives at the starting
  ##  time.
  ##
  ## Arguments:
  ##  eta0: (numeric) starting daily component.
  ##  mu0: (numeric) starting intradaily component.
  ##  etaD0: (numeric) first derivative of the starting daily component.
  ##  muD0: (numeric) first derivative of the starting intradaily component.
  ##
  ## Value:
  ##  (list) Starting filter components and their first derivatives. It has
  ##   components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##   $wD: (numeric)
  ##   $w1D: (numeric)
  ##   $muD: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  list(v = eta0, v1 = eta0/2, eta = eta0,
       w = mu0, w1 = mu0/2, mu = mu0,
       vD = etaD0, v1D = etaD0/2, etaD = etaD0,
       wD = muD0, w1D = muD0/2, muD = muD0)
}
# ------------------------------------------------------------------------------


.fltDLag0.diMEM <-
function(xD0, maxLag)
{
  ##############################################################################
  ## Description:
  ##  Build lagged components of the filter and their first derivatives at the
  ##  starting time.
  ##
  ## Arguments:
  ##  xD0: (list) starting filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##   $wD: (numeric)
  ##   $w1D: (numeric)
  ##   $muD: (numeric)
  ##  maxLag: (list) maximum lag of the filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##
  ## Value:
  ##  (list) Starting lagged filter components and their first derivatives. It
  ##   has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##   $wD: (numeric)
  ##   $w1D: (numeric)
  ##   $muD: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  list(v   = rep.int(xD0$v, maxLag$v),
       v1  = rep.int(xD0$v1, maxLag$v1),
       eta = rep.int(xD0$eta, maxLag$eta),
       w   = rep.int(xD0$w, maxLag$w),
       w1  = rep.int(xD0$w1, maxLag$w1),
       mu  = rep.int(xD0$mu,  maxLag$mu),
       vD   = outer(xD0$vD, rep.int(1,maxLag$v)),
       v1D  = outer(xD0$v1D, rep.int(1,maxLag$v1)),
       etaD = outer(xD0$etaD, rep.int(1,maxLag$eta)),
       wD   = outer(xD0$wD, rep.int(1,maxLag$w)),
       w1D  = outer(xD0$w1D, rep.int(1,maxLag$w1)),
       muD  = outer(xD0$muD, rep.int(1,maxLag$mu)))
}
# ------------------------------------------------------------------------------


.fltD.diMEM <-
function(xDLag)
{
  ##############################################################################
  ## Description:
  ##  Retrieve the current value of the filtered quantities and their
  ##  derivatives from the lagged values.
  ##
  ## Arguments:
  ##  xLag: (list) lagged filter components and their first derivatives. It has
  ##   components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##   $wD: (numeric)
  ##   $w1D: (numeric)
  ##   $muD: (numeric)
  ##
  ## Value:
  ##  (list) current filter components and their first derivatives. It has
  ##   components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $w: (numeric)
  ##   $w1: (numeric)
  ##   $mu: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##   $wD: (numeric)
  ##   $w1D: (numeric)
  ##   $muD: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  list(v = xDLag$v[1], v = xDLag$v1[1], eta = xDLag$eta[1],
       w = xDLag$w[1], w1 = xDLag$w1[1], mu = xDLag$mu[1],
       vD = xDLag$vD[1,], v1D = xDLag$v1D[1,], etaD = xDLag$etaD[1,],
       wD = xDLag$wD[1,], w1D = xDLag$w1D[1,], muD = xDLag$muD[1,])
}
# ------------------------------------------------------------------------------


.adjIntraLag <-
function(j, Lx, xL, x0, indAdj)
{
  ##############################################################################
  ## Description:
  ##  Adjust the intradaily lagged component 'xL', returning the usual lagged
  ##  vector, say xLag(i,j), or an adjusted lagged vector, say xLagA(i,j).
  ##  The possible adjustment is ruled by the logical 'indAdj'.
  ##
  ## Arguments:
  ##  j: (numeric) current bin.
  ##  Lx: (numeric) maximum ajusted lag.
  ##  xL: (numeric) lagged values to be updated, i.e. xLag(i,j-1) for
  ##                j = 2, ..., J-1; xLag(i-1,J) for j = 1.
  ##  x0: (numeric) initializing value, x(0).
  ##  indAdj: (logical) adjust lagged values? TRUE means that components
  ##   j:Lx are replaced by x(0).
  ##
  ## Value:
  ##  (numeric): adjusted lagged component
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Possible adjustment
  if (indAdj && j <= Lx)
  {
    xL[j:Lx] <- x0
  }

  #### Answer
  xL
}
# ------------------------------------------------------------------------------


.adjDerIntraLag <-
function(j, Lx, xL, x0, xDL, xD0, indAdj)
{
  ##############################################################################
  ## Description:
  ##  Adjust the intradaily lagged component 'xL' and its the derivative 'xDL',
  ##  returning the usual components, say xLag(i,j) and xDLag(i,j), or an
  ##  adjusted lagged components, say xDA(i,j) and xDLagA(i,j).
  ##  The possible adjustment is ruled by the logical 'indAdj'.
  ##
  ## Arguments:
  ##  j: (numeric) current bin.
  ##  Lx: (numeric) maximum ajusted lag.
  ##  xL: (numeric) lagged values to be updated, i.e. xLag(i,j-1) for
  ##                j = 2, ..., J-1; xLag(i-1,J) for j = 1.
  ##  x0: (numeric) initializing value, x(0).
  ##  xDL: (numeric) lagged derivative values to be updated, i.e. xDLag(i,j-1)
  ##                for j = 2, ..., J-1; xDLag(i-1,J) for j = 1.
  ##  xD0: (numeric) initializing value, xD(0).
  ##  indAdj: (logical) adjust lagged values? TRUE means that columns
  ##   j:Lx are replaced by xD(0).
  ##
  ## Value:
  ##  (matrix): adjusted lagged component
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Possible adjustment
  if (indAdj && j <= Lx)
  {
    xDL[,j:Lx] <- xD0
    xL[j:Lx] <- x0
  }

  #### Answer
  list(x = xL, xD = xDL)
}
# ------------------------------------------------------------------------------


################################################################################
## PART 2: FUNCTIONS for filtering within the daily-intradaily MEM.
##  .filter.diMEM()                 Filter daily ('eta') periodic ('seas') and
##                                   intradaily ('mu') components in the
##                                   di-MEM
##  .fgh.diMEM()                    Compute values of the (pseudo-scaled)
##                                   criterion function, gradient, Hessian in
##                                   the di-MEM.
##  .f.diMEM()                      Computes the values of the (pseudo-scaled)
##                                   criterion function in the di-MEM.
################################################################################

.filter.diMEM <-
function(parmVal, infoFilter, data)
{
  ##############################################################################
  ## Description:
  ##  Filter daily ('eta') periodic ('seas') and intradaily ('mu') components
  ##  in the daily-intradaily MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the function .data() for details).
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $eta: (numeric) values of the daily filter component;
  ##   $seas: (numeric) values of the periodic component;
  ##   $mu: (numeric) values of the intradaily filter component.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Set starting point
  time1 <- 0

  #### Starting values for the filter
  ## Settings
  maxLag <- infoFilter$maxLag
  ## Starting
  eta0   <- data$meanX
  mu0    <- 1
  ## Make
  flt0   <- .flt0.diMEM(eta0, mu0)
  fltLag <- .fltLag0.diMEM(flt0, maxLag)


  ##############################################################################
  ## Part 2: Answer
  ##############################################################################

  .filter.diMEM.1(parmVal, infoFilter, data, fltLag, flt0, time1)$filter
}
# ------------------------------------------------------------------------------


.fgh.diMEM <-
function(parmVal, infoFilter, data)
{
  ##############################################################################
  ## Description:
  ##  Computes the values of the following quantities in the
  ##  daily-intradaily MEM:
  ##  - scaled-pseudo-objective function: f = g'g / 2
  ##  - scaled-pseudo-gradient:           g = sum_{t = 1}^T a(t) u(t)
  ##  - scaled-pseudo-Hessian:            H = sum_{t = 1}^T a(t) a(t)'
  ##  The not scaled pseudo-objective function, pseudo-gradient and
  ##  pseudo-Hessian are, respectively,
  ##  - fn = sigma^{-4} f
  ##  - gn = sigma^{-2} g
  ##  - Hn = sigma^{-2} H.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the function .data() for details).
  ##
  ## Value:
  ##  (list) with components:
  ##   $f: (numeric) scaled pseudo-objective function.
  ##   $g: (numeric) scaled pseudo-gradient.
  ##   $h: (matrix) scaled pseudo-Hessian.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Set point
  time1 <- 0

  #### Starting values for the filter and their first derivatives
  ## Settings
  maxLag <- infoFilter$maxLag
  nPar   <- NROW(parmVal)
  ## Starting
  eta0   <- data$meanX
  mu0    <- 1
  etaD0  <- numeric(nPar)
  muD0   <- numeric(nPar)
  ## Make
  fltD0   <- .fltD0.diMEM(eta0, mu0, etaD0, muD0)
  fltDLag <- .fltDLag0.diMEM(fltD0, maxLag)


  ##############################################################################
  ## Part 4: Answer
  ##############################################################################

  .fgh.diMEM.1(parmVal, infoFilter, data, fltDLag, fltD0, time1)$fgh
}
# ------------------------------------------------------------------------------


.f.diMEM <-
function(parmVal, infoFilter, data)
{
  ##############################################################################
  ## Description:
  ##  Computes the values of the scaled-pseudo-objective function in the
  ##  daily-intradaily MEM:
  ##  - f = g'g / 2
  ##  The not scaled pseudo-objective function is
  ##  - fn = sigma^{-4} f
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the function .data() for details).
  ##
  ## Value:
  ##  (numeric) value of the scaled-pseudo-objective function.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  .fgh.diMEM(parmVal, infoFilter, data)$f
}
# ------------------------------------------------------------------------------


################################################################################
## PART 3: General FUNCTIONS for filtering within the MEM.
##  .filter.MEM.1()                 Filter within the MEM.
##  .fgh.MEM.1()                    Compute values of the (pseudo-scaled)
##                                   criterion function, gradient, Hessian in
##                                   the MEM. As a subproduct computes also
##                                   lagged values of quantities involved in the
##                                   filter and their first derivatives.
##  .flt0.MEM()                     Build components of the filter at the
##                                   starting time.
##  .fltLag0.MEM()                  Build lagged components of the filter at the
##                                   starting time.
##  .flt.MEM()                      Retrieve the current value of the filtered
##                                   quantities from the lagged values.
##  .fltD0.MEM()                    Build components of the filter their
##                                   derivatives at the starting time.
##  .fltDLag0.MEM()                 Build lagged components of the filter and
##                                   their derivatives at the starting time.
##  .fltD.MEM()                     Retrieve the current value of the filtered
##                                   quantities and their derivatives from the
##                                   lagged values.
################################################################################

.filter.MEM.1 <-
function(parmVal, infoFilter, data, fltLag, flt0, time1)
{
  ##############################################################################
  ## Description:
  ##  Filter in MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the function .data() for details).
  ##
  ## Value:
  ##  (numeric) values of the MEM filter.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Parameter and model settings
  lagV      <- infoFilter$lag$v
  lagV1     <- infoFilter$lag$v1
  lagEta    <- infoFilter$lag$eta
  maxLagV   <- infoFilter$maxLag$v
  maxLagV1  <- infoFilter$maxLag$v1
  maxLagEta <- infoFilter$maxLag$eta
  parmValL  <- .parmVal.MEM(parmVal, infoFilter, data)
  omegaE    <- parmValL$omegaE
  deltaE    <- parmValL$deltaE
  alphaE    <- parmValL$alphaE
  gammaE    <- parmValL$gammaE
  betaE     <- parmValL$betaE

  #### Data settings
  x         <- as.numeric( data$x )
  nX        <- NROW(x)
  zE        <- matrix(data = data$zE, nrow = nX, ncol = NCOL(data$zE)) 
  indRet    <- as.numeric( data$indRet )

  #### Time settings
  tim1      <- time1 + 1
  tim2      <- time1 + nX


  ##############################################################################
  ## Part 2: Initialize filter
  ##############################################################################

  #### Starting values 'v0', 'v10', 'eta0'
  v0      <- flt0$v
  v10     <- flt0$v1
  eta0    <- flt0$eta

  #### Lagged 'v', v1, 'eta'
  vL      <- fltLag$v
  v1L     <- fltLag$v1
  etaL    <- fltLag$eta

  #### Current 'v', 'v1', 'eta'
  flt     <- .flt.MEM(fltLag)
  v       <- flt$v
  v1      <- flt$v1
  eta     <- flt$eta

  #### Initialize output
  etaStore <- numeric(nX)


  ##############################################################################
  ## Part 3: Filter
  ##############################################################################

  #### External cycle
  for (i in 1:nX)
  {
    #### Current eta
    eta <- .update.MEM(zE[i,], vL, v1L, etaL, lagV, lagV1, lagEta,
      omegaE, deltaE, alphaE, gammaE, betaE)

    #### Store
    etaStore[i] <- eta

    #### Current v, v1
    v    <- x[i]
    if (is.na(v))
    {
      v <- eta
    }
    indReti <- indRet[i]
    v1 <- v * indReti

    #### Update lagged 'v', 'v1', 'eta'
    vL  <- c(v, vL[-maxLagV])
    v1L <- c(v1, v1L[-maxLagV1])
    etaL <- c(eta, etaL[-maxLagEta])
  }


  ##############################################################################
  ## Part 4: Answer
  ##############################################################################

  list( filter = etaStore,
        lagged = list(v = vL, v1 = v1L, eta = etaL) )
}
# ------------------------------------------------------------------------------


.fgh.MEM.1 <-
function(parmVal, infoFilter, data, fltDLag, fltD0, time1)
{
  ##############################################################################
  ## Description:
  ##  Computes the values of the scaled-pseudo-objective function,
  ##  scaled-pseudo-gradient (moment function) and of
  ##  the scaled-pseudo-Hessian (-scaled information matrix) in MEM:
  ##  - scaled-pseudo-objective function: f = g'g / 2
  ##  - scaled-pseudo-gradient:           g = sum_{t = 1}^T a(t) u(t)
  ##  - scaled-pseudo-Hessian:            h = sum_{t = 1}^T a(t) a(t)'
  ##  The not scaled pseudo-gradient and pseudo-Hessian are, respectively,
  ##  - fn = sigma^{-4} f
  ##  - gn = sigma^{-2} g
  ##  - hn = sigma^{-2} H.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the function .data() for details).
  ##
  ## Value:
  ##  (list) with components:
  ##   $f: (numeric) scaled pseudo-objective function.
  ##   $g: (numeric) scaled pseudo-gradient.
  ##   $h: (matrix) scaled pseudo-Hessian.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Parameter and model settings
  lagV      <- infoFilter$lag$v
  lagV1     <- infoFilter$lag$v1
  lagEta    <- infoFilter$lag$eta
  maxLagV   <- infoFilter$maxLag$v
  maxLagV1  <- infoFilter$maxLag$v1
  maxLagEta <- infoFilter$maxLag$eta
  parmValL  <- .parmVal.MEM(parmVal, infoFilter, data)
  omegaE    <- parmValL$omegaE
  deltaE    <- parmValL$deltaE
  alphaE    <- parmValL$alphaE
  gammaE    <- parmValL$gammaE
  betaE     <- parmValL$betaE
  ## The following settings are not into '.filter.MEM.1()'
  indVT.E   <- infoFilter$VT$eta
  tmp        <- infoFilter$rows
  rowsOmegaE <- c( tmp$omegaE, tmp$omegaM )
  rowsDeltaE <- c( tmp$deltaE, tmp$deltaM )
  rowsAlphaE <- c( tmp$alphaE, tmp$alphaM )
  rowsGammaE <- c( tmp$gammaE, tmp$gammaM )
  rowsBetaE  <- c( tmp$betaE, tmp$betaM )
  nPar       <- NROW(parmVal)

  #### Data settings
  x       <- as.numeric( data$x )
  meanX   <- data$meanX
  nX      <- NROW(x)
  indRet  <- as.numeric( data$indRet )
  zE      <- matrix(data = data$zE, nrow = nX, ncol = NCOL(data$zE))
  meanZE  <- data$meanZE
  meanV   <- data$meanX
  meanV1  <- meanV / 2
  meanEta <- meanV

  #### Time settings
  tim1    <- time1 + 1
  tim2    <- time1 + nX


  ##############################################################################
  ## Part 3: Initialize filter and derivatives
  ##############################################################################

  #### Starting values 'v0', 'v10', 'eta0' and first derivatives
  v0      <- fltD0$v
  v10     <- fltD0$v1
  eta0    <- fltD0$eta
  vD0     <- fltD0$vD
  v1D0    <- fltD0$v1D
  etaD0   <- fltD0$etaD

  #### Lagged 'v', 'eta', 'w', 'mu' and first derivatives
  vL      <- fltDLag$v
  v1L     <- fltDLag$v1
  etaL    <- fltDLag$eta
  vDL     <- fltDLag$vD
  v1DL    <- fltDLag$v1D
  etaDL   <- fltDLag$etaD

  #### Current 'v', 'eta', 'w', 'mu' and first derivatives
  fltD    <- .fltD.MEM(fltDLag)
  v       <- fltD$v
  v1      <- fltD$v1
  eta     <- fltD$eta
  vD      <- fltD$vD
  v1D     <- fltD$v1D
  etaD    <- fltD$etaD

  #### Initialize u and a for computing moment function and information matrix
  u       <- numeric(nX)
  a       <- matrix(0, nX, nPar)


  ##############################################################################
  ## Part 3: Filter
  ##############################################################################

  #### Cycle
  for (i in 1:nX)
  {
    #### Current eta and etaD
    tmp <- .updateD.MEM(zE[i,], vL, v1L, etaL, vDL, v1DL, etaDL, lagV, lagV1, lagEta,
      meanZE, meanV, meanV1, meanEta, indVT.E,
      rowsOmegaE, rowsDeltaE, rowsAlphaE, rowsGammaE, rowsBetaE,
      omegaE, deltaE, alphaE, gammaE, betaE)
    eta  <- tmp$f
    etaD <- tmp$fD

    #### Current contribution to v, vD
    ind  <- !is.na(x[i])
    if (ind)
    {
      v  <- x[i]
      vD <- vD0
    }
    else
    {
      v  <- eta
      vD <- etaD
    }
    
    indReti <- indRet[i]
    v1 <- v * indReti
    v1D <- vD * indReti

    #### Store u, a
    u[i]  <- v / eta - 1
    a[i,] <- etaD / eta

    #### Update lagged v, eta
    vL   <- c(v, vL[-maxLagV])
    v1L  <- c(v1, v1L[-maxLagV1])
    etaL <- c(eta, etaL[-maxLagEta])

    #### Update lagged etaD, vD
    vDL   <- cbind(vD, vDL[, -maxLagV])
    v1DL  <- cbind(v1D, v1DL[, -maxLagV1])
    etaDL <- cbind(etaD, etaDL[, -maxLagEta])
  }


  ##############################################################################
  ## Part 4: Computes useful quantities for optimization.
  ##############################################################################

  #### Scaled-pseudo-gradient (constant sigma^(-2) removed)
  g <- colSums(a * u)

  #### Scaled-pseudo-Hessian (constant sigma^(-2) removed)
  H <- -crossprod(a)

  #### Pseudo-objective function
  f <- 0.5 * sum(g*g)


  ##############################################################################
  ## Part 5: Answer
  ##############################################################################

  list( fgh = list(f = f, g = g, h = H),
        lagged = list(v = vL, v1 = v1L, eta = etaL,
                      v = vDL, v1 = v1DL, etaD = etaDL) )
}
# ------------------------------------------------------------------------------


.flt0.MEM <-
function(eta0)
{
  ##############################################################################
  ## Description:
  ##  Build components of the filter at the starting time.
  ##
  ## Arguments:
  ##  eta0: (numeric) starting component.
  ##
  ## Value:
  ##  (list) Starting filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  list(v = eta0, v1 = eta0/2, eta = eta0)
}
# ------------------------------------------------------------------------------


.fltLag0.MEM <-
function(x0, maxLag)
{
  ##############################################################################
  ## Description:
  ##  Build lagged components of the filter at the starting time.
  ##
  ## Arguments:
  ##  flt0: (list) starting filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##  maxLag: (list) maximum lag of the filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##
  ## Value:
  ##  (list) Starting lagged filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  list( v = rep.int(x0$v, maxLag$v),
        v1 = rep.int(x0$v1, maxLag$v1),
        eta = rep.int(x0$eta, maxLag$eta) )
}
# ------------------------------------------------------------------------------


.flt.MEM <-
function(xLag)
{
  ##############################################################################
  ## Description:
  ##  Retrieve the current value of the filtered quantities from the lagged
  ##  values.
  ##
  ## Arguments:
  ##  xLag: (list) starting filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##
  ## Value:
  ##  (list) Current filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  list(v = xLag$v[1], v1 = xLag$v1[1], eta = xLag$eta[1])
}
# ------------------------------------------------------------------------------


.fltD0.MEM <-
function(eta0, etaD0)
{
  ##############################################################################
  ## Description:
  ##  Build components of the filter their first derivatives at the starting
  ##  time.
  ##
  ## Arguments:
  ##  eta0: (numeric) starting daily component.
  ##  etaD0: (numeric) first derivative of the starting daily component.
  ##
  ## Value:
  ##  (list) Starting filter components and their first derivatives. It has
  ##   components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  list(v = eta0, v1 = eta0/2, eta = eta0,
       vD = etaD0, v1D = etaD0/2, etaD = etaD0)
}
# ------------------------------------------------------------------------------


.fltDLag0.MEM <-
function(xD0, maxLag)
{
  ##############################################################################
  ## Description:
  ##  Build lagged components of the filter and their first derivatives at the
  ##  starting time.
  ##
  ## Arguments:
  ##  xD0: (list) starting filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##  maxLag: (list) maximum lag of the filter components. It has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##
  ## Value:
  ##  (list) Starting lagged filter components and their first derivatives. It
  ##   has components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  list( v   = rep.int(xD0$v, maxLag$v),
        v1  = rep.int(xD0$v1, maxLag$v1),
        eta = rep.int(xD0$eta, maxLag$eta),
        vD   = outer(xD0$vD, rep.int(1,maxLag$v)),
        v1D  = outer(xD0$v1D, rep.int(1,maxLag$v1)),
        etaD = outer(xD0$etaD, rep.int(1,maxLag$eta)) )
}
# ------------------------------------------------------------------------------


.fltD.MEM <-
function(xDLag)
{
  ##############################################################################
  ## Description:
  ##  Retrieve the current value of the filtered quantities and their
  ##  derivatives from the lagged values.
  ##
  ## Arguments:
  ##  xLag: (list) lagged filter components and their first derivatives. It has
  ##   components:
  ##   $v: (numeric)
  ##   $v1: (numeric)
  ##   $eta: (numeric)
  ##   $vD: (numeric)
  ##   $v1D: (numeric)
  ##   $etaD: (numeric)
  ##
  ## Value:
  ##  (list) current filter components and their first derivatives. It has
  ##   components:
  ##   $v: (numeric)
  ##   $eta: (numeric)
  ##   $vD: (numeric)
  ##   $etaD: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  list( v = xDLag$v[1], v1 = xDLag$v1[1], eta = xDLag$eta[1],
        vD = xDLag$vD[1,], v1D = xDLag$v1D[1,], etaD = xDLag$etaD[1,] )
}
# ------------------------------------------------------------------------------


################################################################################
## PART 4: FUNCTIONS for filtering within the MEM.
##  .filter.MEM()                   Filter within MEM
##  .fgh.MEM()                      Compute values of the (pseudo-scaled)
##                                   criterion function, gradient, Hessian in
##                                   the MEM.
##  .f.MEM()                        Computes the values of the (pseudo-scaled)
##                                   criterion function in the MEM.
################################################################################

.filter.MEM <-
function(parmVal, infoFilter, data)
{
  ##############################################################################
  ## Description:
  ##  Filter in the MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the function .data() for details).
  ##
  ## Value:
  ##  (numeric) values of the filter.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Set starting point
  time1 <- 0

  #### Starting values for the filter
  ## Settings
  maxLag <- infoFilter$maxLag
  ## Starting
  eta0   <- data$meanX
  ## Make
  flt0   <- .flt0.MEM(eta0)
  fltLag <- .fltLag0.MEM(flt0, maxLag)


  ##############################################################################
  ## Part 2: Answer
  ##############################################################################

  .filter.MEM.1(parmVal, infoFilter, data, fltLag, flt0, time1)$filter
}
# ------------------------------------------------------------------------------


.fgh.MEM <-
function(parmVal, infoFilter, data)
{
  ##############################################################################
  ## Description:
  ##  Computes the values of the following quantities in the MEM:
  ##  - scaled-pseudo-objective function: f = g'g / 2
  ##  - scaled-pseudo-gradient:           g = sum_{t = 1}^T a(t) u(t)
  ##  - scaled-pseudo-Hessian:            H = sum_{t = 1}^T a(t) a(t)'
  ##  The not scaled pseudo-objective function, pseudo-gradient and
  ##  pseudo-Hessian are, respectively,
  ##  - fn = sigma^{-4} f
  ##  - gn = sigma^{-2} g
  ##  - Hn = sigma^{-2} H.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the function .data() for details).
  ##
  ## Value:
  ##  (list) with components:
  ##   $f: (numeric) scaled pseudo-objective function.
  ##   $g: (numeric) scaled pseudo-gradient.
  ##   $h: (matrix) scaled pseudo-Hessian.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Set point
  time1 <- 0

  #### Starting values for the filter and their first derivatives
  ## Settings
  maxLag <- infoFilter$maxLag
  nPar   <- NROW(parmVal)
  ## Starting
  eta0   <- data$meanX
  etaD0  <- numeric(nPar)
  ## Make
  fltD0   <- .fltD0.MEM(eta0, etaD0)
  fltDLag <- .fltDLag0.MEM(fltD0, maxLag)


  ##############################################################################
  ## Part 4: Answer
  ##############################################################################

  .fgh.MEM.1(parmVal, infoFilter, data, fltDLag, fltD0, time1)$fgh
}
# ------------------------------------------------------------------------------


.f.MEM <-
function(parmVal, infoFilter, data)
{
  ##############################################################################
  ## Description:
  ##  Computes the values of the scaled-pseudo-objective function in the MEM:
  ##  - f = g'g / 2
  ##  The not scaled pseudo-objective function is
  ##  - fn = sigma^{-4} f
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the function .data() for details).
  ##
  ## Value:
  ##  (numeric) value of the scaled-pseudo-objective function.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  .fgh.MEM(parmVal, infoFilter, data)$f
}
# ------------------------------------------------------------------------------



################################################################################
## PART 6: FUNCTIONS for simulation
##  .r.diMEM.1()                    General function for simulating from the
##                                   di-MEM.
##  .r.diMEM()                      Simulates values form the di-MEM.
##
################################################################################

.r.diMEM.1 <-
function(parmVal, infoFilter, data, fltLag, flt0, time1)
{
  ##############################################################################
  ## Description:
  ##  Simulates values form the daily-intradaily MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  data: (list) adjusted data (see the function .data() for details).
  ##   $x: (numeric) includes simulated residuals instead of (as usual) the time
  ##    series.
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
  ##  time1: (numeric) current time of the filter.
  ##
  ## Value:
  ##  (list) with components:
  ##   $x: (numeric) simulated time series
  ##   $filter: (list) filter components, since first = time1 + 1 to
  ##    last = time1 + nobs. It has components:
  ##    $eta: (numeric)
  ##    $seas: (numeric)
  ##    $mu: (numeric)
  ##   $lagged: (list) last (at time1 + nobs) lagged filtered components.
  ##    It has components:
  ##    $v: (numeric)
  ##    $v1: (numeric)
  ##    $eta: (numeric)
  ##    $w: (numeric)
  ##    $w1: (numeric)
  ##    $mu: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Parameter and model settings. These are essentially ruled by the
  #### structure of eta(i) and mu(i,j) (see .update.MEM() and .updateD.MEM()).
  ## lag
  lagV      <- infoFilter$lag$v
  lagV1     <- infoFilter$lag$v1
  lagEta    <- infoFilter$lag$eta
  lagW      <- infoFilter$lag$w
  lagW1     <- infoFilter$lag$w1
  lagMu     <- infoFilter$lag$mu
  ## maxLag
  maxLagV   <- infoFilter$maxLag$v
  maxLagV1  <- infoFilter$maxLag$v1
  maxLagEta <- infoFilter$maxLag$eta
  maxLagW   <- infoFilter$maxLag$w
  maxLagW1  <- infoFilter$maxLag$w1
  maxLagMu  <- infoFilter$maxLag$mu
  ## L
  Lw        <- infoFilter$L$w
  Lw1       <- infoFilter$L$w1
  Lmu       <- infoFilter$L$mu
  ## parameters
  parmValL  <- .parmVal.diMEM(parmVal, infoFilter, data)
  omegaE    <- parmValL$omegaE
  deltaE    <- parmValL$deltaE
  alphaE    <- parmValL$alphaE
  gammaE    <- parmValL$gammaE
  betaE     <- parmValL$betaE
  omegaM    <- parmValL$omegaM
  deltaM    <- parmValL$deltaM
  alphaM    <- parmValL$alphaM
  gammaM    <- parmValL$gammaM
  betaM     <- parmValL$betaM

  #### diControl settings
  nBin      <- infoFilter$diControl$nBin
  intraAdj  <- infoFilter$diControl$intraAdj

  #### Data settings
  eps       <- data$x
  nX        <- NROW(eps)
  indRet    <- data$indRet
  indDRet   <- data$indDRet

  #### Time settings
  tim1      <- .time( list(time = time1 +  1), nBin )
  tim2      <- .time( list(time = time1 + nX), nBin )
  nDay      <- (tim2$day - tim1$day) + 1


  ##############################################################################
  ## Part 2: Seasonal component
  ##############################################################################

  #### Compute
  seas <- .computeIP(parmVal, infoFilter)


  ##############################################################################
  ## Part 3: Initialize filter
  ##############################################################################

  #### Starting values 'v0', 'v10', 'eta0', 'w0', 'w10', 'mu0'
  v0      <- flt0$v
  v10     <- flt0$v1
  eta0    <- flt0$eta
  w0      <- flt0$w
  w10     <- flt0$w1
  mu0     <- flt0$mu

  #### Lagged 'vL', 'v1L', 'etaL', 'wL', 'w1L', 'muL'
  vL      <- fltLag$v
  v1L     <- fltLag$v1
  etaL    <- fltLag$eta
  wL      <- fltLag$w
  w1L     <- fltLag$w1
  muL     <- fltLag$mu

  #### Current 'v', 'v1', 'eta', 'w', 'w1', 'mu'
  flt     <- .flt.diMEM(fltLag)
  v       <- flt$v
  v1      <- flt$v1
  eta     <- flt$eta
  w       <- flt$w
  w1      <- flt$w1
  mu      <- flt$mu

  #### Initialize output
  etaStore <- numeric(nDay)
  muStore  <- numeric(nX)
  xStore   <- numeric(nX)


  ##############################################################################
  ## Part 4: Filter
  ##############################################################################

  #### Current time settings
  j   <- tim1$bin
  i   <- 0

  #### Save 'eta' if j > 1 (i.e. not at first bin of the day)
  if (j > 1)
  {
    #### Update day
    i <- i + 1

    #### Store
    etaStore[i] <- eta
  }

  #### Cycle
  for (t1 in 1:nX)
  {
    #### Update 'eta' if j = 1 (i.e. at first bin of the day)
    if (j == 1)
    {
      #### Update day
      i <- i + 1

      #### Current eta
      eta <- .update.MEM(zE[i,], vL, v1L, etaL, lagV, lagV1, lagEta,
        omegaE, deltaE, alphaE, gammaE, betaE)

      #### Store
      etaStore[i] <- eta

      #### Update etaL
      etaL <- c(eta, etaL[-maxLagEta])

      #### Initialize v
      v   <- 0
    }

    #### Current mu
    ## Adjust lagged values
    wLs  <- .adjIntraLag(j, Lw , wL , w0 , intraAdj)
    w1Ls <- .adjIntraLag(j, Lw1, w1L, w10, intraAdj)
    muLs <- .adjIntraLag(j, Lmu, muL, mu0, intraAdj)
    ## Current mu
    mu <- .update.MEM(zM[t1,], wLs, w1Ls, muLs, lagW, lagW1, lagMu,
      omegaM, deltaM, alphaM, gammaM, betaM)

    ## Current seas
    sj <- seas[j]

    #### Simulated x
    xj <- sj * eta * mu * eps[t1]

    #### Store
    muStore[t1] <- mu
    xStore[t1]  <- xj

    #### Update
    if (is.na(xj))
    {
      xj <- sj * eta * mu
    }

    #### Update for v; Current w, 'w1'
    v  <- v + xj / (sj * mu)
    w  <- xj / (sj * eta)
    indRetj <- indRet[t1]
    w1 <- w * indRetj

    #### Update lagged w and mu (wL, muL)
    wL  <- c(w, wL[-maxLagW])
    w1L <- c(w1, w1L[-maxLagW1])
    muL <- c(mu, muL[-maxLagMu])

    #### Update 'etaL' and 'vL' if j = nBin (i.e. at last bin of the day)
    if (j < nBin)
    {
      #### Update j
      j <- j + 1

      #### Update first components of vL, v1L
      vL[1]  <- v
      v1L[1] <- v1
    }
    else
    {
      #### Current 'v', 'v1'
      v    <- v / nBin
      v1   <- v * indDRet[i]

      #### Update lagged 'v', 'v1'
      vL <- c(v, vL[-maxLagV])
      v1L <- c(v1, v1L[-maxLagV1])

      #### Update j
      j <- 1
    }
  }


  ##############################################################################
  ## Part 5: Answer
  ##############################################################################

  #### Answer
  list(x = xStore,
       filter = list(eta = etaStore, seas = seas, mu = muStore),
       lagged = list(v = vL, v1 = v1L, eta = etaL, w = wL, w1 = w1L, mu = muL) )
}
# ------------------------------------------------------------------------------


.r.diMEM <-
function(parmVal, infoFilter, eps, diControl)
{
  ##############################################################################
  ## Description:
  ##  Simulates values form the daily-intradaily MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  eps: (numeric) innovations. It can be:
  ##   - A vector. In such a case includes innovations;
  ##   - A matrix[nobs,2]. In such a case eps[,1] = innovations;
  ##     eps[,2] = returns.
  ##  diControl: (list) model and estimation control settings. See functions
  ##   .diControl() for details.
  ##
  ## Value:
  ##  (list) with components:
  ##   $x: (numeric) simulated time series
  ##   $filter: (list) filter components, since first = time1 + 1 to
  ##    last = time1 + nobs. It has components:
  ##    $eta: (numeric)
  ##    $seas: (numeric)
  ##    $mu: (numeric)
  ##   $lagged: (list) last (at time1 + nobs) lagged filtered components.
  ##    It has components:
  ##    $v: (numeric)
  ##    $v1: (numeric)
  ##    $eta: (numeric)
  ##    $w: (numeric)
  ##    $w1: (numeric)
  ##    $mu: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Data
  data1 <- .make.data( x = eps, nBin = diControl$nBin, meanX = NA,
    times = c(1, NROW(eps)) )

  #### Settings
  flt0   <- .flt0.diMEM(eta0 = diControl$eta0, mu0 = 1)
  fltLag <- .fltLag0.diMEM(flt0, infoFilter$maxLag)


  ##############################################################################
  ## Part 2: Burn
  ##############################################################################

  #### Simulation
  t1    <- 1
  t2    <- diControl$nDBurn * diControl$nBin
  data2 <- .extract.data( x = data1, times = c(t1, t2) )
  time1 <- t1 - 1
  fltLag <- .r.diMEM.1(parmVal, infoFilter, data2, fltLag, flt0, time1)$lagged


  ##############################################################################
  ## Part 2: Simulation
  ##############################################################################

  #### Simulation
  t1    <- t2 + 1
  t2    <- NROW(eps)
  data2 <- .extract.data( x = data1, times = c(t1, t2) )
  time1 <- t1 - 1
  x     <- .r.diMEM.1(parmVal, infoFilter, data2, fltLag, flt0, time1)$x
  

  ##############################################################################
  ## Part 3: Prices
  ##############################################################################

  #### prices
  if (NCOL(eps) > 1)
  {
    x <- cbind(x, eps[t1:t2, 2])
  }
  
  
  ##############################################################################
  ## Part 4: Answer
  ##############################################################################

  ####
  x
}
# ------------------------------------------------------------------------------


################################################################################
## PART 7: FUNCTIONS for intradaily periodic component
##
##  .indVarsIP()                    Make independent variables for estimating
##                                   the intradaily periodic component.
##  .indVarsDummies()               Make independent variables for the
##                                   intradaily periodic component expressed via
##                                   dummy variables.
##  .indVarsFourier()               Make independent variables for the
##                                   intradaily periodic component expressed via
##                                   Fourier (sin/cos) variables.
##  .fit.IP()                       Fit intradaily periodic component by using
##                                   OLS on logs.
##  .computeIP()                    Compute the intradaily component for all
##                                   bins of a day.
##  .computedlogIP()                Compute the derivative of the log-seasonal
##                                   component for all bins of a day.
################################################################################

.indVarsIP <-
function(J, j, type)
{
  ##############################################################################
  ## Description:
  ##  Make independent variables for estimating the intradaily periodic
  ##  component.
  ##
  ## Arguments:
  ##  J: (numeric) period.
  ##  j: (numeric) periods.
  ##  type: (character) "dummy" or "fourier".
  ##
  ## Value:
  ##  (matrix) independent variables.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Check j against J
  if (any(j > J))
  {
    stop("Argument 'j' must be <= 'J'")
  }

  #### Answer
  if (type == "dummy")
  {
    .indVarsDummies(J, j)
  }
  else if (type == "fourier")
  {
    .indVarsFourier(J, j)
  }
  else
  {
    stop("Argument 'type' must be 'dummy' or 'fourier'")
  }
}
# ------------------------------------------------------------------------------


.indVarsDummies <-
function(J, j)
{
  ##############################################################################
  ## Description:
  ##  Make independent variables for the intradaily periodic component expressed
  ##  via dummy variables.
  ##
  ## Arguments:
  ##  J: (numeric) period.
  ##  j: (numeric) periods.
  ##
  ## Value:
  ##  (matrix) independent variables.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Dimensions
  n <- NROW(j)
  K <- J - 1

  #### Build
  ## Initialize
  x <- matrix(0, n, K)
  ## Replace
  x[j == J, ] <- -1
  x[ cbind(1:n, ifelse(j < J, j, 0)) ] <- 1
  ## Colnames
  colnames(x) <- paste("d", 1:K, sep = "")

  #### Answer
  x
}
# ------------------------------------------------------------------------------


.indVarsFourier <-
function(J, j)
{
  ##############################################################################
  ## Description:
  ##  Make independent variables for the intradaily periodic component expressed
  ##  via Fourier (sin/cos) variables.
  ##
  ## Arguments:
  ##  J: (numeric) period.
  ##  j: (numeric) periods.
  ##
  ## Value:
  ##  (matrix) independent variables.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Dimensions
  K <- trunc(0.5 * J)
  K1 <- K
  K2 <- ifelse(J%%2 == 0, K - 1, K)

  #### Build
  ## f
  f  <- 2 * pi / J
  ## j
  j <- j - 1
  ## Make
  x <- cbind( mapply(FUN = cos1, k = 1:K1, MoreArgs = list(f = f, j = j)),
              mapply(FUN = sin1, k = 1:K2, MoreArgs = list(f = f, j = j)))
  ## Colnames
  colnames(x) <- c( paste("cos", 1:K1, sep = ""),
                    paste("sin", 1:K2, sep = "") )

  #### Answer
  x
}

cos1 <-
function(f, j, k)
{
  cos(f * j * k)
}

sin1 <-
function(f, j, k)
{
  sin(f * j * k)
}
# ------------------------------------------------------------------------------


.fit.IP <-
function(x, infoFilter, calendar)
{
  ##############################################################################
  ## Description:
  ##  Fit intradaily periodic by using OLS on log(seas).
  ##
  ## Arguments:
  ##  x: (numeric) dependent variable
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  calendar: (list)
  ##
  ## Value:
  ##  (numeric) estimated coefficients
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### bin
  nBin <- infoFilter$diControl$nBin

  #### Build Regressors
  type <- .IPType(infoFilter$parms)
  x1 <- .indVarsIP(J = nBin, j = calendar$binL, type = type)

  #### Remove undesired components
  ind <- infoFilter$lags[infoFilter$rows$seas]
  x1 <- x1[, ind, drop = FALSE]

  #### Make formula
  formula1 <- paste("y ~ ", paste(colnames(x1), collapse = " + "), sep = "")

  #### Build it as data.frame
  data1 <- data.frame( y = log( as.numeric(x) ), x1 )

  #### Model
  mod1 <- lm(formula = as.formula(formula1), data = data1)
  print( summary(mod1) )

  #### Answer: Coefficients (all but the Intercept!)
  ind <- names(mod1$coefficients) == "(Intercept)"
  mod1$coefficients[!ind]
}
# ------------------------------------------------------------------------------


.computeIP <-
function(parmVal, infoFilter)
{
  ##############################################################################
  ## Description:
  ##  Compute the intradaily periodic component.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values
  ##  infoFilter: (list) information summarizing model settings. The following
  ##   components are used (see function .infoFilter() for details):
  ##   $parms: (numeric)
  ##   $lags: (numeric)
  ##   $rows: (list)
  ##    $seas: (numeric)
  ##   $diControl: (list)
  ##    $nBin: (numeric)
  ##
  ## Value:
  ##  (numeric) values of the intradaily periodic component.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Settings
  type <- .IPType(infoFilter$parms)
  nBin <- infoFilter$diControl$nBin

  ####
  if (type == "none")
  {
    rep.int(1, nBin)
  }
  else
  {
    #### Settings
    rows    <- infoFilter$rows$seas
    lags    <- infoFilter$lags[rows]
    parmVal <- parmVal[rows]

    #### Regressors
    ## Make
    x1 <- .indVarsIP(J = nBin, j = 1:nBin, type = type)
    ## Select
    x1 <- x1[, lags, drop = FALSE]
    #### Answer
    exp( x1 %*% parmVal )
  }
}
# ------------------------------------------------------------------------------


.computedlogIP <-
function(parmVal, infoFilter)
{
  ##############################################################################
  ## Description:
  ##  Compute the intradaily periodic component.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values
  ##  infoFilter: (list) information summarizing model settings. The following
  ##   components are used (see function .infoFilter() for details):
  ##   $parms: (numeric)
  ##   $lags: (numeric)
  ##   $rows: (list)
  ##    $seas: (numeric)
  ##   $diControl: (list)
  ##    $nBin: (numeric)
  ## Value:
  ##  (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Settings
  type <- .IPType(infoFilter$parms)
  nBin <- infoFilter$diControl$nBin
  np   <- NROW(parmVal)

  #### Initialize
  out <- matrix(0, nBin, np)

  #### If none
  if (type != "none")
  {
    #### Settings
    rows <- infoFilter$rows$seas
    lags <- infoFilter$lags[rows]

    #### Make
    x1 <- .indVarsIP(J = nBin, j = 1:nBin, type = type)[, lags, drop = FALSE]

    #### Insert
    out[, rows] <- x1
  }

  #### Answer
  out
}
# ------------------------------------------------------------------------------


################################################################################
## PART 8: FUNCTIONS for utilities
##  .extract.rows()                 Extract positions of the parameters, by
##                                   type, from the whole vector.
##  .infoFilter()                   Extract information for filtering.
##  .extract.infoFilter()           Select information of the given 'type' from
##                                   the whole 'infoFilter'.
##  .data()                         Make data.
##  .parmVal.diMEM()                Extract values of parameters in di-MEM.
##  .parmVal.MEM()                  Extract values of parameters in MEM.
##  .modelType()                    Returns the type of model.
##  .IPType()                       Returns the type of intradaily periodic
##                                   component.
################################################################################


.extract.rows <-
function(parm)
{
  ##############################################################################
  ## Description:
  ##  Extract positions of the parameters, by type, from the whole vector.
  ##
  ## Arguments:
  ##  parm: (numeric) parameter types.
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $omegaE:
  ##   $deltaE:
  ##   $alphaE:
  ##   $gammaE:
  ##   $betaE:
  ##   $omegaM:
  ##   $deltaM:
  ##   $alphaM:
  ##   $gammaM:
  ##   $betaM:
  ##   $sigma:
  ##  Each component denotes the position, in the whole vector, of the
  ##  corresponding parameter.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  list(omegaE = which(parm == ...omegaE.N),
       deltaE = which(parm == ...deltaE.N),
       alphaE = which(parm == ...alphaE.N),
       gammaE = which(parm == ...gammaE.N),
       betaE  = which(parm == ...betaE.N),
       omegaM = which(parm == ...omegaM.N),
       deltaM = which(parm == ...deltaM.N),
       alphaM = which(parm == ...alphaM.N),
       gammaM = which(parm == ...gammaM.N),
       betaM  = which(parm == ...betaM.N),
       seas   = which(parm %in% ...seas.N),
       sigma  = which(parm == ...sigma.N))
}
# ------------------------------------------------------------------------------


.infoFilter <-
function(parms, lags, diControl)
{
  ##############################################################################
  ## Description:
  ##  Extract information for filtering.
  ##
  ## Arguments:
  ##  parms: (numeric) parameter types
  ##  lags: (numeric) lags
  ##  diControl: (list) with (at least) the following components
  ##   $nBin: (numeric) number of equally spaced daily bins.
  ##   $intraAdj: (logical) type initialization of the intradaily component.
  ##    See .diControl() for details.
  ##
  ## Value:
  ##  (list) with the following components:
  ##  $parms: (numeric)
  ##  $lags: (numeric)
  ##  $rows: (numeric)
  ##  $lag
  ##   $v
  ##   $v1
  ##   $eta
  ##   $zm
  ##   $w
  ##   $w1
  ##   $mu
  ##  $maxLag
  ##   $v
  ##   $v1
  ##   $eta
  ##   $zm
  ##   $w
  ##   $w1
  ##   $mu
  ##  $VT
  ##   $eta
  ##   $mu
  ##  $L
  ##   $w
  ##   $w1
  ##   $mu
  ##  $diControl
  ##   $nBin
  ##   $intraAdj
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Parameters
  ##############################################################################

  #### Extract parameter's indices
  rows    <- .extract.rows(parms)

  #### Component settings
  indVT.E <- NROW(rows$omegaE) == 0
  indVT.M <- NROW(rows$omegaM) == 0


  ##############################################################################
  ## Part 2: lags
  ##############################################################################

  #### Extracts lags
  lagV    <- lags[rows$alphaE]
  lagV1   <- lags[rows$gammaE]
  lagEta  <- lags[rows$betaE]
  lagW    <- lags[rows$alphaM]
  lagW1   <- lags[rows$gammaM]
  lagMu   <- lags[rows$betaM]

  #### maxLag for different components
  maxLagV   <- max( 0, lagV )
  maxLagV1  <- max( 0, lagV1 )
  maxLagEta <- max( 0, lagEta )
  maxLagW   <- max( 0, lagW )
  maxLagW1  <- max( 0, lagW1 )
  maxLagMu  <- max( 0, lagMu )


  ##############################################################################
  ## Part 3: lags
  ##############################################################################

  #### Extracts diControl settings
  nBin <- diControl$nBin
  intraAdj <- diControl$intraAdj

  #### Max lag for replacing mu starting values if maxLagW > nBin - 1
  #### and intraAdj == TRUE
  Lw  <- min(nBin - 1, maxLagW)
  Lw1 <- min(nBin - 1, maxLagW1)
  Lmu <- min(nBin - 1, maxLagMu)


  ##############################################################################
  ## Part 4: Answer
  ##############################################################################

  #### Pack as list
  list( parms = parms,
        lags = lags,
        rows = rows,
        lag = list(v = lagV, v1 = lagV1, eta = lagEta,
                   w = lagW, w1 = lagW1, mu = lagMu),
        maxLag = list(v = maxLagV, v1 = maxLagV1, eta = maxLagEta,
                   w = maxLagW, w1 = maxLagW1, mu = maxLagMu),
        VT = list(eta = indVT.E, mu = indVT.M),
        L = list(w = Lw, w1 = Lw1, mu = Lmu),
        diControl = list(nBin = nBin, intraAdj = intraAdj)
        )
}
# ------------------------------------------------------------------------------


.extract.infoFilter <-
function(infoFilter, type)
{
  ##############################################################################
  ## Description:
  ##  Select information of the given 'type' from the whole 'infoFilter'.
  ##
  ## Arguments:
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  type: (character) 'E' for eta, 'M' for mu.
  ##
  ## Value:
  ##  (list) with the following components:
  ##  $parms: (numeric)
  ##  $lags: (numeric)
  ##  $rows: (numeric)
  ##  $lag: (list)
  ##   $v: (numeric)
  ##   $eta: (numeric)
  ##  $maxLag: (list)
  ##   $v: (numeric)
  ##   $eta: (numeric)
  ##  $VT: (list)
  ##   $eta: (logical)
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Adjust
  type   <- tolower(type[1])


  ##############################################################################
  ## Part 2: daily component
  ##############################################################################

  #### Select
  parms  <- infoFilter$parms
  lags   <- infoFilter$lags
  lag    <- infoFilter$lag
  maxLag <- infoFilter$maxLag
  VT     <- infoFilter$VT

  #### Adjust
  if (type == "e")
  {
     parmsX <- ...parmsE.N
  }
  else if (type == "m")
  {
     parmsX  <- ...parmsM.N
     lag$v   <- lag$w
     lag$v1  <- lag$w1
     lag$eta <- lag$mu
     maxLag$ze  <- maxLag$zm
     maxLag$v   <- maxLag$w
     maxLag$v1  <- maxLag$w1
     maxLag$eta <- maxLag$mu
     VT$eta  <- VT$mu
  }
  else
  {
     stop("Argument 'type' must be 'E' or 'M'")
  }
  lag$w  <- NULL
  lag$w1 <- NULL
  lag$mu <- NULL
  maxLag$zm <- NULL
  maxLag$w  <- NULL
  maxLag$w1 <- NULL
  maxLag$mu <- NULL
  VT$mu  <- NULL

  #### Select
  rows  <- which(parms %in% parmsX)
  parms <- parms[rows]
  lags  <- lags[rows]
  rows  <- .extract.rows(parms)


  ##############################################################################
  ## Part 3: Answer
  ##############################################################################

  #### Pack as list
  list( parms = parms,
        lags = lags,
        rows = rows,
        lag = lag,
        maxLag = maxLag,
        VT = VT )
}
# ------------------------------------------------------------------------------


.parmVal.diMEM <-
function(parmVal, infoFilter, data)
{
  ##############################################################################
  ## Description:
  ##  Extract values of parameters in the daily-intradaily MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values
  ##  infoFilter: (list) information summarizing model settings. The following
  ##   components are used (see function .infoFilter() for details):
  ##   $rows: (list) rows where to find parameters.
  ##   $VT: (list) VT logicals.
  ##  data: (list) data. The following components are used (see function
  ##   .infoFilter() for details):
  ##   $mean: (numeric) mean(x) (useful only if omegaE is estimated via VT)
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $omegaE: (numeric)
  ##   $deltaE: (numeric)
  ##   $alphaE: (numeric)
  ##   $gammaE: (numeric)
  ##   $betaE: (numeric)
  ##   $omegaM: (numeric)
  ##   $deltaM: (numeric)
  ##   $gammaM: (numeric)
  ##   $alphaM: (numeric)
  ##   $betaM: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Extract parameter's indices
  rows  <- infoFilter$rows

  #### Component settings
  VT    <- infoFilter$VT

  #### Extracts parameters (all but omega's)
  alphaE  <- parmVal[rows$alphaE]
  deltaE  <- parmVal[rows$deltaE]
  gammaE  <- parmVal[rows$gammaE]
  betaE   <- parmVal[rows$betaE]
  alphaM  <- parmVal[rows$alphaM]
  deltaM  <- parmVal[rows$deltaM]
  gammaM  <- parmVal[rows$gammaM]
  betaM   <- parmVal[rows$betaM]

  #### omega's
  ## omegaE
  if ( NROW(VT$eta) == 0 )
  {
    omegaE <- numeric(0)
  }
  else if ( VT$eta )
  {
    omegaE <- .estimateOmegaE(parmVal, infoFilter, data)
  }
  else
  {
    omegaE <- parmVal[rows$omegaE]
  }

  ## omegaM
  if ( NROW(VT$mu) == 0 )
  {
    omegaM <- numeric(0)
  }
  else if ( VT$mu )
  {
    omegaM <- .estimateOmegaM(parmVal, infoFilter, data)
  }
  else
  {
    omegaM <- parmVal[rows$omegaM]
  }

  #### Answer
  list( omegaE = omegaE, deltaE = deltaE, alphaE = alphaE, gammaE = gammaE, 
        betaE = betaE,
        omegaM = omegaM, deltaM = deltaM, alphaM = alphaM, gammaM = gammaM, 
        betaM = betaM)
}
# ------------------------------------------------------------------------------


.parmVal.MEM <-
function(parmVal, infoFilter, data)
{
  ##############################################################################
  ## Description:
  ##  Extract values of parameters in MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values
  ##  infoFilter: (list) information summarizing model settings. The following
  ##   components are used (see function .infoFilter() for details):
  ##   $rows: (list) rows where to find parameters.
  ##   $VT: (list) VT logicals.
  ##  data: (list) data. The following components are used (see function
  ##   .infoFilter() for details):
  ##   $mean: (numeric) mean(x) (useful only if omegaE is estimated via VT)
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $omegaE: (numeric)
  ##   $alphaE: (numeric)
  ##   $gammaE: (numeric)
  ##   $betaE: (numeric)
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### parameter's indices
  rows  <- infoFilter$rows

  #### Component settings
  VT    <- infoFilter$VT$eta

  #### Extracts parameters
  alphaE <- parmVal[ c(rows$alphaE, rows$alphaM) ]
  deltaE <- parmVal[ c(rows$deltaE, rows$deltaM) ]
  gammaE <- parmVal[ c(rows$gammaE, rows$gammaM) ]
  betaE  <- parmVal[ c(rows$betaE, rows$betaM) ]

  if ( VT )
  {
    omegaE <- .estimateOmega(deltaE, alphaE, gammaE, betaE, 
      data$meanX, data$meanZE)
  }
  else
  {
    omegaE <- parmVal[ c(rows$omegaE, rows$omegaM) ]
  }

  #### Answer
  list( omegaE = omegaE, deltaE = deltaE, alphaE = alphaE, gammaE = gammaE, 
    betaE = betaE )
}
# ------------------------------------------------------------------------------


.modelType <-
function(parms)
{
  ##############################################################################
  ## Description:
  ##  Returns the type of model.
  ##
  ## Arguments:
  ##  parms: (numeric) parameter types.
  ##
  ## Value:
  ##  (character):
  ##  "E" for daily;
  ##  "M" for intradaily;
  ##  "EM" for daily-intradaily.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  #### Select type
  if ( all(parms %in% ...parmsE.N) )
  {
    "E"
  }
  else if ( all(parms %in% ...parmsM.N) )
  {
    "M"
  }
  else
  {
    "EM"
  }
}
# ------------------------------------------------------------------------------


.IPType <-
function(parms)
{
  ##############################################################################
  ## Description:
  ##  Returns the type of intradaily periodic component.
  ##
  ## Arguments:
  ##  parms: (numeric) parameter types.
  ##
  ## Value:
  ##  (character):
  ##  "fourier" periodic component in fourier (sin/cos) form;
  ##  "dummy"   periodic component in dummy form;
  ##  "none"    no periodic component.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  #### Answer
  if ( any(parms %in% ...seasF.N) )
  {
    "fourier"
  }
  else if ( any(parms %in% ...seasD.N) )
  {
    "dummy"
  }
  else
  {
    "none"
  }
}
# ------------------------------------------------------------------------------