################################################################################
##
## File: vWAPEstimation-20091205.R
##
## Purpose:
##  R functions for estimating the daily-intradaily MEM.
##
## Created: 2008.10.10
##
## Version: 2009.12.06
##
## Author:
##  Fabrizio Cipollini <cipollini@ds.unifi.it>
##
################################################################################

################################################################################
## FUNCTION:                       TASK:
##
## PART 1: FUNCTIONS for Newton-Raphson algorithm. ** Note: Newton's method **
##  .nrControl()                    Newton-Raphson control settings.
##  .nr.di()                        Newton-Raphson algorithm, specific for the
##                                   daily-intradaily MEM or a simple MEM.
##  .nr()                           Newton-Raphson algorithm. It exploits a
##                                   function that computes, simultaneously,
##                                   the gradient and the Hessian.
##
## PART 2: FUNCTIONS for Nelder-Mead algorithm. ** Note: Simplex Method **
##  .nmControl()                    Nelder-Mead control settings.
##  .nm.di()                        Nelder-Mead algorithm, specific for the
##                                   daily-intradaily MEM or a simple MEM.
##  .nm()                           Nelder-Mead algorithm. It exploits the R
##                                   function 'optim'.
##
## PART 3: FUNCTIONS for non iterative estimators and other inferences.
##  .estimateSigma()                Estimate the standard deviation of the
##                                   innovation component in the di-MEM.
##  .estimateOmega()                Evaluate 'omega' in a MEM from parameters
##                                   and the unconditional mean.
##  .estimateOmegaE()               Estimate 'omegaE' the constant of the daily
##                                   component in the daily-intradaily MEM.
##  .estimateOmegaM()               Estimate 'omegaM' the constant of the
##                                   intradaily component in the
##                                   daily-intradaily MEM.
##  .gammaLogLik()                  LogLikelihood assuming Gamma distribution
##                                   of the innovations.
##  .compute.IC()                   Computes the values of some information
##                                   criteria.
##
## PART 4: FUNCTIONS for inference.
##  .adjustStart.di()               Adjust starting values of parameters.
##  .fit.diMEM()                    Make inferences from the daily-intradaily
##                                   MEM.
##  .print.diMEM()                  Print inferences from the daily-intradaily
##                                   MEM.
##
## PART 5: FUNCTIONS for diagnostics.
##  .compute.portmanteau()          Computes Portmanteau (Box-Ljang) test from
##                                   estimated innovations.
##
################################################################################


################################################################################
## PART 1: FUNCTIONS for nr() (Newton-Raphson)
##  .nrControl()                    Newton-Raphson control settings.
##  .nr.di()                        Newton-Raphson algorithm, specific for the
##                                   daily-intradaily MEM or a simple MEM.
##  .nr()                           Newton-Raphson algorithm. It exploits a
##                                   function that computes, simultaneously,
##                                   the gradient and the Hessian.
################################################################################

.nrControl <-
function(parmVal = NULL, control = NULL)
{
  ##############################################################################
  ## Description:
  ##  Newton-Raphson control settings.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  control: (list) with (at least) these components:
  ##   $nrTrace: (numeric) any nrTrace iterations are printed during estimation
  ##    (default 10)
  ##   $nrGradTol: (numeric) gradient tolerance (default 1e-5)
  ##   $nrMaxIter: (numeric) maximum number of iterations (default 200)
  ##   $nrWgt: (numeric) weigth for gradient (default 1)
  ##   $parNames: (character) parameter names (default 1:NROW(parmVal))
  ##
  ## Value:
  ##  (list) with nr control settings adjusted.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Eta.
  ##############################################################################

  #### 'nrTrace': level of printing during the minimization process. Print
  #### any nrTrace iterations.
  ## default value
  def <- 1
  ## Adjust
  tmp <- as.integer(control$E$nrTrace[1])
  control$E$nrTrace <- ifelse(NROW(tmp) > 0 && 0 <= tmp && tmp <= 2, tmp, def)

  #### 'nrGradTol': tolerance at which the scaled gradient is considered close
  #### enough to zero.
  ## default value
  def <- 1e-5
  ## Adjust
  tmp <- control$E$nrGradTol[1]
  control$E$nrGradTol <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)

  #### 'nrMaxIter': maximum number of iterations to be performed before the
  #### termination.
  ## default value
  def <- 200
  ## Adjust
  tmp <- as.integer(control$E$nrMaxIter[1])
  control$E$nrMaxIter <- ifelse(NROW(tmp) > 0 && tmp >= 0, tmp, def)

  #### 'nrWgt': gradient weigth.
  ## default value
  def <- 1
  ## Adjust
  tmp <- as.integer(control$E$nrWgt[1])
  control$E$nrWgt <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)


  ##############################################################################
  ## Part 2: Mu.
  ##############################################################################

  #### 'nrTrace': level of printing during the minimization process. Print
  #### any nrTrace iterations.
  ## default value
  def <- 1
  ## Adjust
  tmp <- as.integer(control$M$nrTrace[1])
  control$M$nrTrace <- ifelse(NROW(tmp) > 0 && 0 <= tmp && tmp <= 2, tmp, def)

  #### 'nrGradTol': tolerance at which the scaled gradient is considered close
  #### enough to zero.
  ## default value
  def <- 1e-5
  ## Adjust
  tmp <- control$M$nrGradTol[1]
  control$M$nrGradTol <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)

  #### 'nrMaxIter': maximum number of iterations to be performed before the
  #### termination.
  ## default value
  def <- 200
  ## Adjust
  tmp <- as.integer(control$M$nrMaxIter[1])
  control$M$nrMaxIter <- ifelse(NROW(tmp) > 0 && tmp >= 0, tmp, def)

  #### 'nrWgt': gradient weigth.
  ## default value
  def <- 1
  ## Adjust
  tmp <- as.integer(control$M$nrWgt[1])
  control$M$nrWgt <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)


  ##############################################################################
  ## Part 3: Eta-Mu.
  ##############################################################################

  #### 'nrTrace': level of printing during the minimization process. Print
  #### any nrTrace iterations.
  ## default value
  def <- 1
  ## Adjust
  tmp <- as.integer(control$EM$nrTrace[1])
  control$EM$nrTrace <- ifelse(NROW(tmp) > 0 && 0 <= tmp && tmp <= 2, tmp, def)

  #### 'nrGradTol': tolerance at which the scaled gradient is considered close
  #### enough to zero.
  ## default value
  def <- 1e-5
  ## Adjust
  tmp <- control$EM$nrGradTol[1]
  control$EM$nrGradTol <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)

  #### 'nrMaxIter': maximum number of iterations to be performed before the
  #### termination.
  ## default value
  def <- 200
  ## Adjust
  tmp <- as.integer(control$EM$nrMaxIter[1])
  control$EM$nrMaxIter <- ifelse(NROW(tmp) > 0 && tmp >= 0, tmp, def)

  #### 'nrWgt': gradient weigth.
  ## default value
  def <- 1
  ## Adjust
  tmp <- as.integer(control$EM$nrWgt[1])
  control$EM$nrWgt <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)


  ##############################################################################
  ## Part 4: parNames.
  ##############################################################################

  #### 'parNames': parameter names.
  ## default value
  def <- 1:NROW(parmVal)
  ## Adjust
  tmp <- as.character(control$parNames)
  if ( NROW(tmp) > 0 )
  {
    control$parNames <- tmp
  }
  else
  {
    control$parNames <- def
  }


  ##############################################################################
  ## Part 5: Answer.
  ##############################################################################

  control
}
# ------------------------------------------------------------------------------


.nr.di <-
function(parmVal, infoFilter, data, diControl)
{
  ##############################################################################
  ## Description:
  ##  Newton-Raphson algorithm specific for the daily-intradaily MEM or for a
  ##  simple MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values.
  ##  infoFilter: (list) settings on the model
  ##   (see .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see .data() for details).
  ##  diControl: (list) (see .nrControl() for details)
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $parmVal: (numeric) parameter estimates
  ##   $g: (numeric) gradient
  ##   $h: (numeric) hessian
  ##   $stop: (numeric) stopping code
  ##   $nIter: (numeric) number of iterations
  ##   $time: (numeric) estimation time
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  #### Settings
  parms <- infoFilter$parms
  lags  <- infoFilter$lags

  #### Adjust diControl
  diControl$parNames <- .make.parNames(parms, lags)
  diControl <- .nrControl(parmVal, diControl)

  #### Select model type
  typ <- .modelType(parms)

  #### Trace time
  tim <- Sys.time()

  #### Estimate
  if ( typ == "E" )
  {
    control <- c( diControl$E, list(parNames = diControl$parNames) )
    est <- .nr(f = .fgh.MEM, parmVal = parmVal, control = control,
        infoFilter = infoFilter, data = data)
  }
  else if ( typ == "M" )
  {
    control <- c( diControl$M, list(parNames = diControl$parNames) )
    est <- .nr(f = .fgh.MEM, parmVal = parmVal, control = control,
        infoFilter = infoFilter, data = data)
  }
  else if ( typ == "EM" )
  {
    control <- c( diControl$EM, list(parNames = diControl$parNames) )
    est <- .nr(f = .fgh.diMEM, parmVal = parmVal, control = control,
        infoFilter = infoFilter, data = data)
  }

  #### Trace time
  est$time <- Sys.time() - tim

  #### Rename components
  lev1 <- c("estimate", "gradient", "hessian", "code", "iterations", "time")
  lab1 <- c("parmVal" , "g"       , "h"      , "stop", "nIter"     , "time")
  tmp  <- .factor(x = names(est), levels = lev1, labels = lab1, typeNum = FALSE)
  names(est) <- tmp

  #### Answer
  est
}
# ------------------------------------------------------------------------------


.nr <-
function(fun, parmVal, control, ...)
{
  ##############################################################################
  ## Description:
  ##  Newton-Raphson algorithm. It exploits a function that computes,
  ##  simultaneously, the gradient and the Hessian.
  ##
  ## Arguments:
  ##  fun: (function) returning a list with two components:
  ##   $g: (numeric) the gradient vector
  ##   $h: (numeric) the Hessian matrix
  ##  parmVal: (numeric) starting values of parameters.
  ##  control: (list) see function .nrControl() for details.
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $estimate: (numeric) parameter estimates
  ##   $gradient: (numeric) gradient
  ##   $hessian: (numeric) hessian
  ##   $code: (numeric) stopping code
  ##   $iterations: (numeric) number of iterations
  ##   $time: (numeric) estimation time
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Set control
  maxIter  <- control$nrMaxIter
  gradTol  <- control$nrGradTol
  wgt      <- control$nrWgt
  parNames <- control$parNames
  trace    <- control$nrTrace

  #### Set tracing
  gradName <- ifelse( abs( wgt - 1) < 1e-8, "gradient", "scaled gradient" )
  cnames   <- c("parameter", "estimate", gradName)

  #### Trace time
  timeEst <- Sys.time()

  #### Case estimation
  if (maxIter > 0)
  {
    #### Trace
    cat("--------------------------------------------------------------------------------", "\n")
    cat("iteration ", 0, "\n")
    out <- cbind(parNames, parmVal, NA)
    colnames(out) <- cnames
    print(out, quote = FALSE, digits = 6, na.print = "..")
    
    #### Cycle
    for (i in 1:maxIter)
    {
      #### Computes mFct, iFct
      tmp <- fun(parmVal, ...)

      #### Try solve
      parmDelta <- try(solve(tmp$h, tmp$g))
			## parmDelta <- try( qr.coef(qr(tmp$h), tmp$g) )

      #### Check
      indSolve <- is.numeric(parmDelta)
			
			#### Update
      if ( indSolve )
      {
				#### Copy past value
				parmVal0 <- parmVal

      	#### Change parmVal
				parmVal  <- parmVal0 - parmDelta
	
				#### Check convergence
				gMean <- tmp$g * wgt
				indGrad <- all( abs(gMean) < gradTol )

				#### Trace
				if (i %% trace == 0)
				{
					cat("--------------------------------------------------------------------------------", "\n")
					cat("iteration ", i, "\n")
					out <- cbind(parNames, parmVal, gMean)
					colnames(out) <- cnames
					print(out, quote = FALSE, digits = 6, na.print = "..")
				}
			}

      #### Checks convergence
      if ( !indSolve || indGrad )
      {
        break
      }
    }
  }
  else
  {
     indSolve <- FALSE
     indGrad <- FALSE
     i       <- 0
     tmp     <- list( g = rep.int(NA, NROW(parmVal)),
                      h = matrix(NA, NROW(parmVal), NROW(parmVal)) )
  }

  #### Trace time
  timeEst <- Sys.time() - timeEst

  #### Trace convergence
  if (!indSolve)
  { 
  	nrStop <- 2
  }
  else
  {
		if (indGrad)
		{
			nrStop <- 0
		}
		else
		{
			nrStop <- 1
		}
  }

  #### Answer
  list(estimate = parmVal, gradient = tmp$g, hessian = tmp$h,
       code = nrStop, iterations = i, time = timeEst)
}
# ------------------------------------------------------------------------------


################################################################################
## PART2: FUNCTIONS for nm() (Nelder-Mead)
##  .nmControl()                    Nelder-Mead control settings.
##  .nm.di()                        Nelder-Mead algorithm, specific for the
##                                   daily-intradaily MEM or a simple MEM.
##  .nm()                           Nelder-Mead algorithm. It exploits the R
##                                   function 'optim'.
################################################################################


.nmControl <-
function(control = NULL)
{
  ##############################################################################
  ## Description:
  ##  Nelder-Mead control settings.
  ##
  ## Arguments:
  ##  control: (list) can have these components:
  ##   $nmTrace: (numeric) iteration tracing option (see the trace control in
  ##    R function optim() for details). default 1.
  ##   $nmAbsTol: (numeric) Absolute convergence tolerance. Only useful for
  ##    non-negative functions, as a tolerance for reaching zero. default 1e-5.
  ##   $nmRelTol: (numeric) Relative convergence tolerance. The algorithm stops
  ##    if it is unable to reduce the value by a factor of
  ##    reltol * (abs(val) + reltol) at a step. default 1e-8.
  ##   $nmMaxIter: (numeric) maximum number of function evaluations (default
  ##    200)
  ##
  ## Value:
  ##  (list) control with nr settings adjusted.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Eta.
  ##############################################################################

  #### 'nmTrace': Option for tracing information during optimization. Higher
  #### values may produce more tracing information.
  ## default value
  def <- 10
  ## Adjust
  tmp <- as.integer(control$E$nmTrace[1])
  control$E$nmTrace <- ifelse(NROW(tmp) && 0 <= tmp && tmp <= 2, tmp, def)

  #### 'nmAbsTol': Absolute convergence tolerance. Only useful for non-negative
  #### functions, as a tolerance for reaching zero.
  ## default value
  def <- 1e-5
  ## Adjust
  tmp <- control$E$nmAbsTol[1]
  control$E$nmAbsTol <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)

  #### 'nmRelTol': Relative convergence tolerance. The algorithm stops if it is
  #### unable to reduce the value by a factor of reltol * (abs(val) + reltol)
  #### at a step. Typically about 1e-8.
  ## default value
  def <- 1e-8
  ## Adjust
  tmp <- control$E$nmRelTol[1]
  control$E$nmRelTol <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)

  #### 'nmMaxIter': maximum number of iterations to be performed before the
  #### termination.
  ## default value
  def <- 0
  ## Adjust
  tmp <- as.integer(control$E$nmMaxIter[1])
  control$E$nmMaxIter <- ifelse(NROW(tmp) > 0 && tmp >= 0, tmp, def)


  ##############################################################################
  ## Part 2: Mu.
  ##############################################################################

  #### 'nmTrace': Option for tracing information during optimization. Higher
  #### values may produce more tracing information.
  ## default value
  def <- 10
  ## Adjust
  tmp <- as.integer(control$M$nmTrace[1])
  control$M$nmTrace <- ifelse(NROW(tmp) && 0 <= tmp && tmp <= 2, tmp, def)

  #### 'nmAbsTol': Absolute convergence tolerance. Only useful for non-negative
  #### functions, as a tolerance for reaching zero.
  ## default value
  def <- 1e-5
  ## Adjust
  tmp <- control$M$nmAbsTol[1]
  control$M$nmAbsTol <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)

  #### 'nmRelTol': Relative convergence tolerance. The algorithm stops if it is
  #### unable to reduce the value by a factor of reltol * (abs(val) + reltol)
  #### at a step. Typically about 1e-8.
  ## default value
  def <- 1e-8
  ## Adjust
  tmp <- control$M$nmRelTol[1]
  control$M$nmRelTol <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)

  #### 'nmMaxIter': maximum number of iterations to be performed before the
  #### termination.
  ## default value
  def <- 0
  ## Adjust
  tmp <- as.integer(control$M$nmMaxIter[1])
  control$M$nmMaxIter <- ifelse(NROW(tmp) > 0 && tmp >= 0, tmp, def)


  ##############################################################################
  ## Part 3: Eta-Mu.
  ##############################################################################

  #### 'nmTrace': Option for tracing information during optimization. Higher
  #### values may produce more tracing information.
  ## default value
  def <- 10
  ## Adjust
  tmp <- as.integer(control$EM$nmTrace[1])
  control$EM$nmTrace <- ifelse(NROW(tmp) && 0 <= tmp && tmp <= 2, tmp, def)

  #### 'nmAbsTol': Absolute convergence tolerance. Only useful for non-negative
  #### functions, as a tolerance for reaching zero.
  ## default value
  def <- 1e-5
  ## Adjust
  tmp <- control$EM$nmAbsTol[1]
  control$EM$nmAbsTol <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)

  #### 'nmRelTol': Relative convergence tolerance. The algorithm stops if it is
  #### unable to reduce the value by a factor of reltol * (abs(val) + reltol)
  #### at a step. Typically about 1e-8.
  ## default value
  def <- 1e-8
  ## Adjust
  tmp <- control$EM$nmRelTol[1]
  control$EM$nmRelTol <- ifelse(NROW(tmp) > 0 && tmp > 0, tmp, def)

  #### 'nmMaxIter': maximum number of iterations to be performed before the
  #### termination.
  ## default value
  def <- 200
  ## Adjust
  tmp <- as.integer(control$EM$nmMaxIter[1])
  control$EM$nmMaxIter <- ifelse(NROW(tmp) > 0 && tmp >= 0, tmp, def)


  ##############################################################################
  ## Part 4: Answer.
  ##############################################################################

  control
}
# ------------------------------------------------------------------------------


.nm.di <-
function( parmVal, infoFilter, data, diControl )
{
  ##############################################################################
  ## Description:
  ##  Nelder-Mead algorithm specific for the daily-intradaily MEM or for a
  ##  simple MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) starting values of parameters.
  ##  infoFilter: (list) settings on the model
  ##   (see .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see .data() for details).
  ##  diControl: (list) (see .nmControl() for details)
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $parmVal: (numeric) parameter estimates
  ##   $f: (numeric) function
  ##   $nIter: (numeric) number of iterations
  ##   $stop: (numeric) stopping code
  ##   $message: (character) optional message from optim
  ##   $time: (numeric) estimation time
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Adjust diControl
  diControl <- .nrControl(parmVal, diControl)

  #### Select type
  typ <- .modelType( infoFilter$parms )

  #### Trace time
  tim <- Sys.time()

  #### Estimate
  if ( typ == "E" )
  {
    control <- diControl$E
    est <- .nm( fun = .f.MEM, parmVal = parmVal, control = control,
      infoFilter = infoFilter, data = data)
  }
  else if ( typ == "M" )
  {
    control <- diControl$M
    est <- .nm( fun = .f.MEM, parmVal = parmVal, control = control,
      infoFilter = infoFilter, data = data)
  }
  else if ( typ == "EM" )
  {
    control <- diControl$EM
    est <- .nm( fun = .f.diMEM, parmVal = parmVal, control = control,
      infoFilter = infoFilter, data = data)
  }

  #### Trace time
  est$time <- Sys.time() - tim

  #### Rename components
  lev1 <- c("estimate", "function", "counts", "code", "message", "time")
  lab1 <- c("parmVal" , "f"       , "nIter" , "stop", "message", "time")
  tmp  <- .factor(x = names(est), levels = lev1, labels = lab1, typeNum = FALSE)
  names(est) <- tmp

  #### Answer
  est
}
# ------------------------------------------------------------------------------


.nm <-
function( fun, parmVal, control, ... )
{
  ##############################################################################
  ## Description:
  ##  Nelder-Mead algorithm. It exploits the R function 'optim'.
  ##
  ## Arguments:
  ##  fun: (numeric) function returning a list with two components:
  ##   $g: (numeric) the gradient vector
  ##   $h: (numeric) the Hessian matrix
  ##  parmVal: (numeric) starting values of parameters.
  ##  control: (list) see function .nmControl() for details.
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $estimate: (numeric) parameter estimates
  ##   $function: (numeric) function value at estimates
  ##   $counts: (numeric) number of function evaluations
  ##   $code: (numeric) stopping code
  ##   $message: (character) optional message from optim
  ##   $time: (numeric) estimation time
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  #### Set control
  control <- .nmControl(control)
  control <- list(trace = control$nmTrace,
     abstol = control$nmAbsTol, reltol = control$nmRelTol,
     maxit = control$nmMaxIter)

  #### Trace time
  tim <- Sys.time()

  #### Case maxit > 0
  if (control$maxit > 0)
  {
    #### Estimation
    est <- optim(par = parmVal, fn = fun, gr = NULL, ...,
        method = "Nelder-Mead", lower = -Inf, upper = Inf,
        control = control, hessian = FALSE)

    #### Trace time
    est$time <- Sys.time() - tim
  
    #### Adjust
    est$counts <- est$counts["function"]
  }
  
  #### Case maxit = 0
  else
  {
     est <- list( par = parmVal,
                  value = NA,
                  counts = 0,
                  convergence = NA,
                  message = NA,
                  time = 0)
  }
  
  #### Rename components
  lev1 <- c("par"     , "value"   , "counts", "convergence", "message", "time")
  lab1 <- c("estimate", "function", "counts", "code"       , "message", "time")
  tmp  <- .factor(x = names(est), levels = lev1, labels = lab1, typeNum = FALSE)
  names(est) <- tmp

  #### Answer
  est
}
# ------------------------------------------------------------------------------


################################################################################
## PART 3: FUNCTIONS for non iterative estimators and other inferences
##  .estimateSigma()                Estimate the standard deviation of the
##                                   innovation component in the di-MEM.
##  .estimateOmega()                Evaluate 'omega' in a MEM from parameters
##                                   and the unconditional mean.
##  .estimateOmegaE()               Estimate 'omegaE' the constant of the daily
##                                   component in the daily-intradaily MEM.
##  .estimateOmegaM()               Estimate 'omegaM' the constant of the
##                                   intradaily component in the
##                                   daily-intradaily MEM.
##  .gammaLogLik()                  LogLikelihood assuming Gamma distribution
##                                   of the innovations.
##  .compute.IC()                   Computes the values of some information
##                                   criteria.
################################################################################

.estimateSigma <-
function(x, condMean)
{
  ##############################################################################
  ## Description:
  ##  Estimate the standard deviation of the innovation component in the
  ##  daily-intradaily model.
  ##
  ## Arguments:
  ##  x: (numeric) time series of data
  ##  condMean: (numeric)
  ##
  ## Value:
  ##  (numeric) standard deviation of residuals.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Working residuals
  u <- x / condMean - 1

  #### Answer
  sqrt( mean(u*u, na.rm = TRUE) )
}
# ------------------------------------------------------------------------------


.estimateOmega <-
function(delta, alpha, gamma, beta, meanX, meanZ)
{
  ##############################################################################
  ## Description:
  ##  Evaluate 'omega' in a MEM from parameters and the unconditional mean.
  ##
  ## Arguments:
  ##  meanX: (numeric) unconditional mean.
  ##  alpha: (numeric)
  ##  gamma: (numeric)
  ##  beta: (numeric)
  ##
  ## Value:
  ##  (numerical) evaluated 'omega'.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Answer
  meanX * ( 1 - sum(alpha) - sum(gamma)/2 - sum(beta) ) - sum(delta * meanZ)
}
# ------------------------------------------------------------------------------


.estimateOmegaE <-
function(parmVal, infoFilter, data)
{
  ##############################################################################
  ## Description:
  ##  Estimate 'omegaE' the constant of the daily component in the
  ##  daily-intradaily MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values
  ##  parms: (numeric) parameter types
  ##  meanX: (numeric) unconditional mean of the time series of data
  ##
  ## Value:
  ##  (numerical) estimated 'omegaE'.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Extracts parameters
  alphaE  <- parmVal[infoFilter$rows$alphaE]
  deltaE  <- parmVal[infoFilter$rows$deltaE]
  gammaE  <- parmVal[infoFilter$rows$gammaE]
  betaE   <- parmVal[infoFilter$rows$betaE]

  #### Answer
  .estimateOmega(deltaE, alphaE, gammaE, betaE, data$meanX, data$meanZE)
}
# ------------------------------------------------------------------------------


.estimateOmegaM <-
function(parmVal, infoFilter, data)
{
  ##############################################################################
  ## Description:
  ##  Estimate 'omegaM' the constant of the intradaily component in the
  ##  daily-intradaily MEM.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values
  ##  parms: (numeric) parameter types
  ##
  ## Value:
  ##  (numerical) estimated 'omegaM'.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Extracts parameters
  alphaM  <- parmVal[infoFilter$rows$alphaM]
  deltaM  <- parmVal[infoFilter$rows$deltaM]
  gammaM  <- parmVal[infoFilter$rows$gammaM]
  betaM   <- parmVal[infoFilter$rows$betaM]

  #### Answer
  .estimateOmega(deltaM, alphaM, gammaM, betaM, 1, data$meanZM)
}
# ------------------------------------------------------------------------------


.gammaLogLik <-
function( x, condMean )
{
  ##############################################################################
  ## Description:
  ##  LogLikelihood assuming Gamma distribution of the innovations.
  ##
  ## Arguments:
  ##  parmVal: (numeric) parameter values
  ##  parms: (numeric) parameter types
  ##  condMean: (numeric) vector of evaluated conditional means
  ##  x: (numeric) time series
  ##
  ## Value:
  ##  (numeric) evaluated loglikelihood.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Setting
  ##############################################################################

  #### Parameters
  sigma <- .estimateSigma(x, condMean)
  phi   <- 1 / sigma^2

  #### Residuals
  eps <- x / condMean

  #### Other settings
  indX <- !is.na(x) & x > 0
  indEps <- !is.na(eps)
  nobs <- sum(indX)

  #### Select
  ind <- indX & indEps
  eps <- eps[ind]
  x <- x[ind]
  
  ### Answer
  ifelse(any(eps < 0), 
    NA, 
		nobs * ( phi * log(phi) - lgamma(phi) ) +
		phi * sum( log(eps) - eps) - sum( log(x) ) )
}
# ------------------------------------------------------------------------------


.compute.IC <-
function(lLik, nobs, np)
{
  ##############################################################################
  ## Description:
  ##  Computes the values of:
  ##   "AIC" (Akaike)
  ##   "BIC"/"SBC" (Bayesian or Schwartz)
  ##   "HQIC" (Hannan-Quinn)
  ##   "AICc" (Corrected Akaike)
  ##   "KIC":
  ##   "KICc":
  ##  information criteria.
  ##
  ## Arguments:
  ##  lLik: (numeric) logLik value
  ##  nobs: (numeric) number of observations
  ##  np: (numeric) number of parameters
  ##
  ## Value:
  ##  Named vector of computed values.
  ##
  ## Implemented by:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Adjust
  nobsM  <- nobs - np - 1
  nobsP  <- nobs - np + 1

  #### Answer
  -2 * lLik +
  c(AIC  = 2 * np,
    HQIC = 2 * log(log(nobs)) * np,
    BIC  = log(nobs) * np,
    AICc = 2 * np * nobs / nobsM,
    KIC  = 3 * np,
    KICc = nobs * ( log(nobs / nobsP) + ( 2 * np + 1 - 2 / nobsP ) / nobsM )
    )
}
# ------------------------------------------------------------------------------


################################################################################
## PART 4: FUNCTIONS for inference
################################################################################

.adjustStart.di <-
function( parmStart, infoFilter, data, diControl )
{
  ##############################################################################
  ## Description:
  ##  Adjust parameter starting values.
  ##
  ## Arguments:
  ##  parmStart: (numeric) starting values for the parameters.
  ##  infoFilter: (list) settings on the model
  ##   (see the funtion .infoFilter() for details).
  ##  data: (list) adjusted data
  ##   (see the funtion .data() for details).
  ##  diControl: (list) (see .diControl() for details)
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $parmVal: (numeric) adjusted parameter values
  ##   $time: (numeric) time spent in adjustment.
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Parameter settings
  parmVal <- parmStart
  parms   <- infoFilter$parms
  lags    <- infoFilter$lags

  #### Data settings
  timeL   <- index(data$x)
  x       <- as.numeric(data$x)

  #### indices
  nBin  <- diControl$nBin
  calendar <- .calendar(timeL, nBin)
  binL <- calendar$binL
  bin <- calendar$bin

  #### Trace
  cat("--------------------------------------------------------------------------------", "\n")
  cat("  Adjust starting values", "\n")
  cat("--------------------------------------------------------------------------------", "\n")

  ####
  traceAlg <- list(nmIterE = 0, nmIterM = 0, nmIterEM = 0,
                   nrIterE = 0, nrIterM = 0, nrIterEM = 0,
                   time = 0)
  

  ##############################################################################
  ## Part 2: Estimate parameters of the daily component
  ##############################################################################

  #### Trace
  cat("--------------------------------------------------------------------------------", "\n")
  cat("  Adjust starting values: DAILY component", "\n")
  cat("--------------------------------------------------------------------------------", "\n")

  #### Parameters
  ind      <- parms %in% ...parmsE.N
  parmValX <- parmVal[ind]

  #### infoFilter
  infoFilterX <- .extract.infoFilter(infoFilter, "E")

  #### Daily data
  dataX <- list(x = .daily.x(x, calendar), zE = data$zE, 
    indRet = data$indDRet, 
    meanX = data$meanX, meanZE = data$meanZE)

  #### Nelder-Mead Estimation
  inference <- .nm.di(parmValX, infoFilterX, dataX, diControl)
  parmValX  <- inference$parmVal
  traceAlg$time    <- traceAlg$time + inference$time
  traceAlg$nmIterE <- inference$nIter

  #### Newton-Raphson Estimation
  inference <- .nr.di(parmValX, infoFilterX, dataX, diControl)
  parmValX  <- inference$parmVal
  traceAlg$time    <- traceAlg$time + inference$time
  traceAlg$nrIterE <- inference$nIter

  #### Adjust x by removing eta
  eta <- .filter.MEM(parmValX, infoFilterX, dataX)
  x   <- .intradaily.x(x, eta, calendar)

  #### Restore parameters
  parmVal[ind] <- parmValX
  

  ##############################################################################
  ## Part 3: Estimate parameters of the periodic component
  ##############################################################################

  #### Checks if periodic parameters are included
  ind <- parms %in% ...seas.N
  
  #### Fit
  if ( any(ind) )
  {
    #### Trace
    cat("--------------------------------------------------------------------------------", "\n")
    cat("  Adjust starting values: INTRADAILY PERIODIC component", "\n")
    cat("--------------------------------------------------------------------------------", "\n")

    #### Periodic Parameters and Indexes
    parmsSsel <- parms[ind]
    lagSsel   <- lags[ind]

    #### Fit (returns the whole time series)
    parmVal[ind] <- .fit.IP(x, infoFilter, calendar)
    seas <- .computeIP(parmVal, infoFilter)
    seas <- .expand(seas, binL, bin)

    #### Remove periodic component from x
    x <- x / seas
  }


  ##############################################################################
  ## Part 4: Estimate parameters of the intradaily component
  ##############################################################################

  #### Trace
  cat("--------------------------------------------------------------------------------", "\n")
  cat("  Adjusting starting values: INTRADAILY NON-PERIODIC component", "\n")
  cat("--------------------------------------------------------------------------------", "\n")

  #### Parameters
  ind      <- parms %in% ...parmsM.N
  parmValX <- parmVal[ind]

  #### infoFilter
  infoFilterX <- .extract.infoFilter(infoFilter, "M")

  #### Data
  dataX <- list(x = x, zE = data$zM, indRet = data$indRet,
    meanX = mean( x, na.rm = TRUE), meanZE = data$meanZM)

  #### Nelder-Mead estimation
  inference <- .nm.di(parmValX, infoFilterX, dataX, diControl)
  parmValX  <- inference$parmVal
  traceAlg$time    <- traceAlg$time + inference$time
  traceAlg$nmIterM <- inference$nIter

  #### Newton-Raphson Estimation
  inference <- .nr.di(parmValX, infoFilterX, dataX, diControl)
  parmValX  <- inference$parmVal
  traceAlg$time    <- traceAlg$time + inference$time
  traceAlg$nrIterM <- inference$nIter

  #### Restore
  parmVal[ind] <- parmValX


  ##############################################################################
  ## Part 5: Answer
  ##############################################################################

  list(parmVal = parmVal, trace = traceAlg)
}
# ------------------------------------------------------------------------------


.fit.diMEM <-
function( parmStart, infoFilter, x, diControl )
{
  ##############################################################################
  ## Description:
  ##  Make inference from the daily-intradaily MEM.
  ##
  ## Arguments:
  ##  parmStart: (numeric) starting values for the parameters.
  ##  infoFilter: (list) settings on the model
  ##   (see the function .infoFilter() for details).
  ##  x: (xts) time series of data, an xts object produced by .data.2.xts().
  ##  diControl: (list) see .diControl() for details
  ##
  ## Value:
  ##  (list) with the following components:
  ##   $nobs: (numeric) number of observations (valid cases)
  ##   $nmiss: (numeric) number of missing observations
  ##   $np: (numeric) number of parameters
  ##   $time: (numeric) estimation time
  ##   $parmName: (character) parameter names
  ##   $parmStart: (numeric) starting values of the parameters
  ##   $parmEst: (numeric) parameter estimates
  ##   $seEst: (numeric) estimated standard errors
  ##   $vcovEst: (matrix) variance matrix estimates
  ##   $gradient: (numeric) gradient at convergence
  ##   $logLik: (numeric) evaluated loglikelihood
  ##   $IC: (numeric) some information criteria
  ##   $pmTest: (matrix) Box-Ljung statistics
  ##   $filter: (list) filtered mu and eta values
  ##   $condMean: (numeric) conditional mean estimates
  ##   $residuals: (numeric) estimated reasiduals
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Parameter settings
  parmVal <- parmStart
  npIter  <- NROW(parmVal)
  parms   <- infoFilter$parms
  lags    <- infoFilter$lags
  parNames <- .make.parNames(parms, lags)

  #### Time series settings
  nBin  <- diControl$nBin
  data1 <- .make.data(data1 = x, meanX = NULL)
  calendar <- .calendar( index(data1$x), nBin )


  ##############################################################################
  ## Part 2: Adjust starting values
  ##############################################################################

  #### Adjust
  inference <- .adjustStart.di( parmVal, infoFilter, data1, diControl )
  parmVal <- inference$parmVal
  traceAlg <- inference$trace


  ##############################################################################
  ## Part 3: Estimation
  ##############################################################################

  #### Adjust data
  data1$x <- as.numeric(data1$x)

  #### Nelder-Mead optimization
  inference <- .nm.di( parmVal, infoFilter, data1, diControl )
  parmVal   <- inference$parmVal
  traceAlg$time <- traceAlg$time + inference$time
  traceAlg$nmIterEM <- inference$nIter

  #### Newton-Raphson optimization
  inference <- .nr.di( parmVal, infoFilter, data1, diControl )
  parmEst   <- inference$parmVal
  traceAlg$time <- traceAlg$time + inference$time
  traceAlg$nrIterEM <- inference$nIter


  ##############################################################################
  ## Part 4: Other info
  ##############################################################################

  #### Settings
  x     <- data1$x
  nobs  <- NROW(x)
  nobsNA <- sum(is.na(x))
  np  <- NROW(parmEst)

  #### Filter
  flt <- .filter.diMEM(parmEst, infoFilter, data1)

  #### Conditional mean
  condMean <- .condMean(flt, calendar)
  seasMu   <- .intradaily.flt(flt, calendar)
  flt      <- c( flt , list(x = condMean, seasMu = seasMu) )

  #### Likelihood based statistics
  lLik <- .gammaLogLik( x, condMean )

  ## Information criteria
  xIC  <- .compute.IC(lLik, nobs, np)

  #### Portmanteau Statistics
  eps    <- x / condMean
  lagMax <- max(5 * nBin, 100)
  pmTest <- .compute.portmanteau(eps, parmEst, lagMax)
#  pmTest <- .compute.portmanteau(eps, NULL, lagMax)


  ##############################################################################
  ## Part 5: Non iterative parameters (if needed)
  ##############################################################################

  #### Initialize
  nullAdd     <- NULL
  parmsAdd    <- NULL
  lagsAdd     <- NULL
  parNamesAdd <- NULL
  parmEstAdd  <- NULL

  #### 'omegaE'
  if ( !(...omegaE.N %in% parms) )
  {
    #### nullAdd
    nullAdd <- c(nullAdd, NA)
    #### parmsAdd
    parmsAdd <- c(parmsAdd, ...omegaE.N)
    #### lagsAdd
    lagsAdd <- c(lagsAdd, 0)
    #### parNames
    parNamesAdd <- c(parNamesAdd, ...omegaE.C)
    #### parmEst
    parmEstAdd <- c(parmEstAdd, .estimateOmegaE(parmEst, infoFilter, data1))
  }

  #### 'omegaM'
  if ( !(...omegaM.N %in% parms) )
  {
    #### nullAdd
    nullAdd <- c(nullAdd, NA)
    #### parmsAdd
    parmsAdd <- c(parmsAdd, ...omegaM.N)
    #### lagsAdd
    lagsAdd <- c(lagsAdd, 0)
    #### parNames
    parNamesAdd <- c(parNamesAdd, ...omegaM.C)
    #### parmEst
    parmEstAdd <- c(parmEstAdd, .estimateOmegaM(parmEst, infoFilter, data1))
  }

  #### 'sigma'
  if ( !(...sigma.N %in% parms) )
  {
    #### nullAdd
    nullAdd <- c(nullAdd, NA)
    #### parmsAdd
    parmsAdd <- c(parmsAdd, ...sigma.N)
    #### lagsAdd
    lagsAdd <- c(lagsAdd, 0)
    #### parNames
    parNamesAdd <- c(parNamesAdd, ...sigma.C)
    #### parmEst
    sigmaEst <- .estimateSigma( x, condMean )
    parmEstAdd <- c(parmEstAdd, sigmaEst)
  }

  #### Total number of parameters
  np <- npIter + NROW(parmEstAdd)


  ##############################################################################
  ## Part 5: Variance matrix
  ##############################################################################

  #### Variance matrix
  if ( !any( is.na(inference$h) ) )
  {
  	vcovEst <- try( solve(-inference$h) )
  	if ( is.numeric(vcovEst) )
  	{
      vcovEst <- sigmaEst^2 * vcovEst
  	}
  	else
  	{
      vcovEst <- NA * inference$h
  	}  	
  }
  else
  {
    vcovEst <- inference$h
  }


  #### Integrate iterative and non-iterative parameters
  parms     <- c(parms, parmsAdd)
  lags      <- c(lags, lagsAdd)
  parNames  <- c(parNames, parNamesAdd)
  parmStart <- c(parmStart, nullAdd)
  parmEst   <- c(parmEst, parmEstAdd)
  seEst     <- c( sqrt( diag(vcovEst) ), nullAdd)
  tmp       <- matrix(NA, np, np)
  tmp[1:npIter, 1:npIter] <- vcovEst
  vcovEst   <- tmp
  inference$g <- c(inference$g, nullAdd)


  ##############################################################################
  ## Part 6: sort
  ##############################################################################

  #### ind
  ind <- .ind.sort.model(cbind(parm = parms, lag = lags), vars = c("parm", "lags"))

  #### Sorting
  parms     <- parms[ind]
  lags      <- lags[ind]
  parNames  <- parNames[ind]
  parmStart <- parmStart[ind]
  parmEst   <- parmEst[ind]
  seEst     <- seEst[ind]
  vcovEst   <- vcovEst[ind, ind]
  inference$g <- inference$g[ind]


  ##############################################################################
  ## Part 8: Answer
  ##############################################################################

  list(
       nobs = nobs, nmiss = nobsNA,
       np = np,
       trace = traceAlg,
       parmName = parNames,
       parmStart = parmStart,
       parmEst = parmEst,
       seEst = seEst,
       vcovEst = vcovEst,
       gradient = inference$g,
       logLik = lLik,
       IC = xIC,
       pmTest = pmTest,
       filter = flt,
       residuals = eps
       )
}
# ------------------------------------------------------------------------------


.print.diMEM <-
function(inference, diControl, fileout = "")
{
  ##############################################################################
  ## Description:
  ##  Print inferences from the daily-intradaily MEM.
  ##
  ## Arguments:
  ##  inference: (list) inferences returned from .fit.diMEM().
  ##  diControl: (list) model control settings.
  ##  fileout: (character) output filename.
  ##
  ## Value:
  ##  NONE
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Extract
  ##############################################################################

  #### Extract
  nobs    <- inference$nobs
  nmiss   <- inference$nmiss
  np      <- inference$np
  parName <- inference$parmName
  parEst  <- inference$parmEst
  seEst   <- inference$seEst
  tStat   <- parEst / seEst
  score   <- inference$gradient
  IC      <- c( "log-Likelihood" = inference$logLik, inference$IC)
  pmTest  <- inference$pmTest
  traceAlg <- inference$trace
  nBin    <- diControl$nBin
  nDay    <- nobs / nBin

  #### Portmanteau Statistics
  apmTest <- 0.05
  pctSpmTest <- round( sum(pmTest[,"pvalue"] < apmTest) / NROW(pmTest) * 100, 2)
  lpmTest <- pmTest[c(1,NROW(pmTest)),"lag"]
  ## From the first one to nBin
  ind     <- 1:(nBin-1)
  pmTest1 <- pmTest[ind, , drop = FALSE]
  ind     <- (1:5)*nBin
  ind     <- pmTest[,"lag"] %in% ind
  pmTest2 <- pmTest[ind, , drop = FALSE]
  pmTest  <- rbind(pmTest1, pmTest2)

  ## Compose
  out    <- cbind("parameter" = parName, "estimate" = parEst, "s.e." = seEst,
                  "t-stat" = tStat, "gradient" = score)

  ## Formatting
  IC      <- rbind(names(IC), IC)
  IC      <- format(x = IC, digits = 6, width = 8, na.encode = TRUE)
  out     <- rbind(colnames(out), out)
  out     <- format(x = out, digits = 6, width = 8, na.encode = TRUE)
  pmTest  <- rbind(colnames(pmTest), pmTest)
  pmTest  <- format(x = pmTest, digits = 6, width = 8, na.encode = TRUE)


  ##############################################################################
  ## Part 2: Print / save
  ##############################################################################

  #### 1) 'fileout'
  if (NROW(fileout) == 0)
  {
    fileout <- ""
  }

  #### 2) Print
  cat(file = fileout, append = TRUE, "--------------------------------------------------------------------------------", "\n")
  cat(file = fileout, append = TRUE, "Daily-Intradaily MEM Inference", "\n")
  cat(file = fileout, append = TRUE, "Saved at", format(Sys.time(), "%Y-%m-%d, %H:%M:%S"), "\n")

  cat(file = fileout, append = TRUE, "Number of observations:",
       "valid cases", nobs, "(", nDay, "days,", nBin, "bins for each day)",
       "\b, missing cases", nmiss, "\n")
  cat(file = fileout, append = TRUE, "Intradaily adjustment:", diControl$intraAdj, "\n")

  cat(file = fileout, append = TRUE, "Estimation:", "\n")

  if (traceAlg$nmIterE > 0)
  {
    cat(file = fileout, append = TRUE, "> Daily: Nelder-Mead, function evaluations: ", traceAlg$nmIterE, "\n")
  }
  if (traceAlg$nrIterE > 0)
  {
    cat(file = fileout, append = TRUE, "> Daily: Newton-Raphson, iterations: ", traceAlg$nrIterE, "\n")
  }
  
  if (traceAlg$nmIterM > 0)
  {
    cat(file = fileout, append = TRUE, "> Intradaily: Nelder-Mead, function evaluations: ", traceAlg$nmIterM, "\n")
  }
  if (traceAlg$nrIterM > 0)
  {
    cat(file = fileout, append = TRUE, "> Intradaily: Newton-Raphson, iterations: ", traceAlg$nrIterM, "\n")
  }

  if (traceAlg$nmIterEM > 0)
  {
    cat(file = fileout, append = TRUE, "> Daily-Intradaily: Nelder-Mead, function evaluations: ", traceAlg$nmIterEM, "\n")
  }
  if (traceAlg$nrIterEM > 0)
  {
    cat(file = fileout, append = TRUE, "> Daily-Intradaily: Newton-Raphson, iterations: ", traceAlg$nrIterEM, "\n")
  }

  cat(file = fileout, append = TRUE, "> Total estimation time", traceAlg$time, "\n")

  cat(file = fileout, append = TRUE, "Fit Statistics:", "\n")
  write.table(file = fileout, append = TRUE, x = IC,
              quote = FALSE, sep = " ", na = "..", row.names = FALSE, col.names = FALSE)

  cat(file = fileout, append = TRUE, "Ljung-Box Statistics:", "\n")
  write.table(file = fileout, append = TRUE, x = pmTest,
              quote = FALSE, sep = " ", na = "..", row.names = FALSE, col.names = FALSE)
  cat(file = fileout, append = TRUE, "Percentage of significant statistics",
       "(alpha", apmTest, "\b, lags", lpmTest[1], "to", lpmTest[2], "):",
       pctSpmTest, "\b%", "\n")

  cat(file = fileout, append = TRUE, "Coefficient Estimates:", "\n")
  write.table(file = fileout, append = TRUE, x = out,
              quote = FALSE, sep = " ", na = "..", row.names = FALSE, col.names = FALSE)

  cat(file = fileout, append = TRUE, "--------------------------------------------------------------------------------", "\n")


  ##############################################################################
  ## Part 3: Answer
  ##############################################################################

  NULL
}
# ------------------------------------------------------------------------------



################################################################################
## PART 5: FUNCTIONS for diagnostics
################################################################################

.compute.portmanteau <-
function(epsMatrix, parmVal, lagMax)
{
  ##############################################################################
  ## Description:
  ##  Computes Portmanteau (Box-Ljung) test from estimated innovations.
  ##
  ## Arguments:
  ##  epsMatrix: (numeric) [nobs,K]-matrix of innovations.
  ##  parmVal: (numeric) parameter values.
  ##  lagMax: (numeric) maximum lag computed.
  ##
  ## Value:
  ##  A matrix with columns:
  ##   "lag"
  ##   "stat"
  ##   "df"
  ##   "pvalue".
  ##
  ## Author:
  ##   Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Check
  ##############################################################################

  ## Maximum time Horizon for portmanteau test
  lagMax <- round( as.numeric(lagMax[1]) )

  if (lagMax <= 0)
  {
    stop("Argument 'lagMax' have to be a positive integer.")
  }


  ##############################################################################
  ## Part 2: Settings
  ##############################################################################

  np   <- NROW(parmVal)
  neqn <- NCOL(epsMatrix)
  nobs <- NROW(epsMatrix)


  ##############################################################################
  ## Part 2: Zero mean residuals
  ##############################################################################

  if (!is.matrix(epsMatrix))
  {
    epsMatrix <- as.matrix(epsMatrix)
  }
  u <- A.op.tx(epsMatrix, colMeans(epsMatrix, na.rm = TRUE), binOp = "-")


  ##############################################################################
  ## Part 3: Computes P, dfs, pvalues
  ##############################################################################

  ## Auto-covariance at lag 0
  C0    <- cov(u, use = "pair")
  C0inv <- solve(C0)

  ## Initialize
  P     <- numeric(lagMax)
  Pi    <- 0

  ## Cycle
  for (i in 1:lagMax)
  {
    in1  <- -(1:i)
    end1 <- -((nobs-i+1):nobs)
    Ci   <- cov(u[in1,], u[end1,], use = "pair")
    Pi   <- Pi + sum( diag( Ci %*% C0inv %*% t(Ci) %*% C0inv ) ) / (nobs - i)
    P[i] <- Pi
  }

  ## lags
  lags <- 1:lagMax

  ## dfs
  dfs <- neqn * neqn * (1:lagMax) - np

  ## Adjusts for negative dfs
  ind <- dfs > 0
  lags <- lags[ind]
  P   <- nobs * (nobs + 2) * P[ind]
  dfs <- dfs[ind]

  ## p-values
  pvalue <- pchisq(q = P, df = dfs, lower.tail = FALSE)


  ##############################################################################
  ## Part 3: Answer
  ##############################################################################

  cbind(lag = lags, stat = P, df = dfs, pvalue = pvalue)
}
# ------------------------------------------------------------------------------


.decomposeSeries <-
function(x, calendar)
{
  ##############################################################################
  ## Description:
  ##  Descriptive decomposition of the series x in:
  ##  - daily pattern
  ##  - intradaily pattern
  ##  - intradaily periodic pattern
  ##  - intradaily non-periodic pattern
  ##
  ## Arguments:
  ##  x: (numeric) time series
  ##  calendar: (list) see .calendar() for details.
  ##
  ## Value:
  ##  (list). It has components:
  ##   $x: (numeric) series in input
  ##   $eta: (numeric) estimated daily component
  ##   $seasMu: (numeric) estimated intradaily component
  ##   $seas: (numeric) estimated intradaily periodic component
  ##   $mu: (numeric) estimated intradaily non-periodic component
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  ##############################################################################
  ## Part 2: Decomposition
  ##############################################################################

  #### eta = average x by day
  eta <- .daily.x(x, calendar)

  #### seas * mu = x / eta
  seasMu <- .intradaily.x(x, eta, calendar)

  #### seas = average (seas * mu) by bin
  seas <- aggregate(x = seasMu, by = list(bin = calendar$binL), FUN = mean,
    na.rm = TRUE)[,2]
    
  #### mu = seas * mu / mu
  tmp <- .expand(seas, calendar$binL, calendar$bin)
  mu <- seasMu / tmp


  ##############################################################################
  ## Part 3: Answer
  ##############################################################################

  list(x = x, eta = eta, seasMu = seasMu, seas = seas, mu = mu)
}
# ------------------------------------------------------------------------------


.plotDiagnostics1 <-
function(xDec, calendar, type, ylim)
{
  ##############################################################################
  ## Description:
  ##  Graphical diagnostics for daily-intradaily data:
  ##  - plot the time series
  ##  - plot the daily averages
  ##  - plot the intradaily pattern
  ##  - plot the bin averages
  ##
  ## Arguments:
  ##  xDec: (list) time series decomposition including the following components
  ##   $x: (numeric) time series
  ##   $eta: (numeric) daily component
  ##   $seas: (numeric) intradaily periodic component
  ##   $mu: (character) intradaily non-periodic component
  ##  tim: (numeric)
  ##  bin: (numeric)
  ##  day: (numeric)
  ##
  ## Value:
  ##  NONE
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ##############################################################################
  ## Part 1: Settings
  ##############################################################################

  #### Settings
  if (type == "../data")
  {
    main1 <- "Data"
    main2 <- "Daily pattern"
    main3 <- "Intradaily pattern"
    main4 <- "Intradaily periodic pattern"
    main5 <- "Intradaily non-periodic pattern"
  }
  else if (type == "fit")
  {
    main1 <- expression( eta[i] ~ s[j] ~ mu[i][j] )
    main2 <- expression( eta[i] )
    main3 <- expression( eta[i] ~ s[j] ~ mu[i][j] )
    main4 <- expression( s[j] )
    main5 <- expression( mu[i][j] )
  }
  else
  {
    stop("Bad 'type' argument")
  }


  ##############################################################################
  ## Part 2: Plots
  ##############################################################################

  #### Plot time series
  plot(x = calendar$time, y = xDec$x, type = "l",
    main = main1, xlab = "time", ylab = "", ylim = ylim$x )

  ## Plot eta component
  plot(x = calendar$day, y = xDec$eta, type = "l",
    main = main2, xlab = "day", ylab = "", ylim = ylim$eta )

  ## Plot seas component
  plot(x = calendar$time, y = xDec$seasMu, type = "l",
    main = main3, xlab = "time", ylab = "", ylim = ylim$mu )

  ## Plot seas component
  plot(x = calendar$bin, y = xDec$seas, type = "l",
    main = main4, xlab = "bin", ylab = "", ylim = ylim$seas )

  ## Plot mu component
  plot(x = calendar$time, y = xDec$mu, type = "l",
    main = main5, xlab = "time", ylab = "", ylim = ylim$mu )


  ##############################################################################
  ## Part 3: Answer
  ##############################################################################

  NULL
}
# ------------------------------------------------------------------------------


.corr1Bin1Lag <-
function(x, binL, bin, lag)
{
  #### Check indexes
  if (lag < 0)
  {
    stop("Argument 'lag' must be non-negative")
  }
  
  #### Select rows
  rows1 <- which(binL == bin)
  rows2 <- rows1 - lag
  
  #### Check if some is negative
  ind <- rows2 <= 0
  if ( any(ind) )
  {
    ind   <- !ind
    rows1 <- rows1[ind]
    rows2 <- rows2[ind]
  }

  #### Correlations
  cor( x[rows1], x[rows2] )
}

.corrBins1Lag <-
function(x, binL, nBin, lag)
{
  mapply(FUN = .corr1Bin1Lag, bin = 1:nBin,
    MoreArgs = list(x = x, binL = binL, lag = lag),
    SIMPLIFY = TRUE, USE.NAMES = TRUE)
}

.corrBinsLags <-
function(x, binL, nBin, lags)
{
  mapply(FUN = .corrBins1Lag, lag = lags,
    MoreArgs = list(x = x, binL = binL, nBin = nBin),
    SIMPLIFY = TRUE, USE.NAMES = TRUE)
}


.acf1Bin <-
function(x, binL, bin, lag.max)
{
  acf(x = x[ binL == bin[1] ], lag.max = lag.max, type = "correlation",
    plot = FALSE, na.action = na.fail, demean = TRUE)$acf
}

.acfAllBins <-
function(x, nBin, lag.max)
{
  #### Make bin and binL
  nobs <- NROW(x)
  nDay <- ceiling(nobs / nBin)
  bin  <- 1:nBin
  binL <- rep(bin, nDay, length.out = nobs)

  #### Replace NA's
  ind <- is.na(x)
  if (any(ind))
  {
    x[ind] <- mean(x, na.rm = TRUE)
  }

  #### Compute
  x <- mapply(FUN = .acf1Bin, bin,
    MoreArgs = list(x = x, binL = binL, lag.max = lag.max),
    SIMPLIFY = TRUE, USE.NAMES = TRUE)

  #### Remove lag 0
  x[-1, , drop = FALSE]
}

.plot.acfAllBins <-
function(x, nBin, type)
{
  #### Make bin and binL
  nobs <- NROW(x)
  nDay <- ceiling(nobs / nBin)
  bin  <- 1:nBin
  binL <- rep(bin, nDay, length.out = nobs)

  #### Compute
  acf.1  <- .corrBins1Lag(x, binL, nBin, 1)
  acf.1 <- as.numeric(acf.1)
  names(acf.1) <- as.character(1:nBin)

  acf.nBin  <- .corrBins1Lag(x, binL, nBin, nBin)
  acf.nBin <- as.numeric(acf.nBin)
  names(acf.nBin) <- as.character(1:nBin)

  #### Settings
  if (type == "residuals")
  {
    ylim <- c(-0.5, 0.5)
    main <- "Residuals: intradaily non-periodic"
  }
  else
  {
    ylim <- c(-1, 1)
    main <- "Data: intradaily non-periodic"
  }

  #### Graph
  ylab <- paste("autocorrelation at lag", 1)
  r <- barplot(height = acf.1, width = 0.1, space = 0.5,
       legend.text = NULL, ylim = ylim,
       xlab = "bin", ylab = ylab, main = main)

  ylab <- paste("autocorrelation at lag", nBin)
  r <- barplot(height = acf.nBin, width = 0.1, space = 0.5,
       legend.text = NULL, ylim = ylim,
       xlab = "bin", ylab = ylab, main = main)
}
# ------------------------------------------------------------------------------


.plotDiagnostics2 <-
function(xDec, time, type)
{
  ##############################################################################
  ## Description:
  ##  Graphical diagnostics for daily-intradaily data:
  ##  - plot the time series
  ##  - plot the ACF of the time series
  ##  - plot the daily averages
  ##  - plot the ACF of the time series of daily averages
  ##  - plot the bin averages
  ##
  ## Arguments:
  ##  x: (numeric) time series
  ##  day: (numeric) days
  ##  bin: (numeric) bins
  ##  type: (character) one between:
  ##   - "x": for the data
  ##   - "residuals": for the residuals
  ##
  ## Value:
  ##  NONE
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### Settings
  nobs <- NROW(time$time)
  nBin <- NROW(time$bin)
  nDay <- NROW(time$day)

  #### If any, replace missing data with the average (for acf only)
  ind <- is.na(xDec$x)
  if ( any(ind) )
  {
    xDec$x[ind] <- mean(xDec$x, na.rm = TRUE)
    xDec$seasMu[ind] <- mean(xDec$seasMu, na.rm = TRUE)
  }

  #### Graphs
  ## Settings
  xEtaLagMax <- 100            ## Max lag for days in ACF
  xLagMax    <- 10 * nBin      ## Max lag for data in ACF

  ## Adjust ylim for ACF of residuals
  if ( type == "residuals" )
  {
    times   <- 20
    tmp     <- min(times / sqrt(nobs), 1)
    ylim    <- c( -tmp, tmp )

    times   <- 10
    tmp     <- times / sqrt(nDay)
    yEtalim <- c( -tmp, tmp )

    times  <- 30
    tmp    <- min(times / sqrt(nobs), 1)
    yMulim <- c( -tmp, tmp )

    ySeasMulim <- yMulim
  }
  else
  {
    ylim  <- NULL

    yEtalim <- NULL

    times <- 30
    tmp   <- min(times / sqrt(nobs), 1)
    yMulim <- c( -tmp, tmp )

    ySeasMulim <- NULL
  }

  #### ACF of the series
  acfX <- acf(x = xDec$x, lag.max = xLagMax, type = "correlation",
    plot = TRUE, demean = TRUE,
    main = type, ylim = ylim )

  #### ACF of eta
  acfEta <- acf(x = xDec$eta, lag.max = xEtaLagMax, type = "correlation",
    plot = TRUE, demean = TRUE,
    main = paste(type, ": Daily", sep = ""), ylim = yEtalim )

  #### ACF of seas * mu
  acfEta <- acf(x = xDec$seasMu, lag.max = xLagMax, type = "correlation",
    plot = TRUE, demean = TRUE,
    main = paste(type, ": Intradaily", sep = ""), ylim = ySeasMulim )

  #### Plot corr(x(i,j), x(i-1,j))
#  acfBin1 <- .plot.acfAllBins(x = xDec$mu, nBin = nBin)

  #### Plot seas component
  plot(x = time$bin, y = xDec$seas, type = "l",
    main = paste(type, ": Intradaily periodic", sep = ""),
    xlab = "bin", ylab = "", ylim = NULL )

  #### ACF of mu
  acfI <- acf(x = xDec$mu, lag.max = xLagMax, type = "correlation", plot = TRUE,
    demean = TRUE,
    main = paste(type, ": Intradaily non-periodic", sep = ""), ylim = yMulim )

  #### Answer
  NULL
}
# ------------------------------------------------------------------------------


.plotDiagnostics <-
function(x, inference, calendar)
{
  ##############################################################################
  ## Description:
  ##  Graphical diagnostics for daily-intradaily data for both the time series
  ##  and the model residuals. Plot:
  ##  - the time series
  ##  - the ACF of the time series
  ##  - the daily averages
  ##  - the ACF of the time series of daily averages
  ##  - the bin averages
  ##
  ## Arguments:
  ##  x: (numeric) time series
  ##  res: (numeric) time series of residuals
  ##  day: (numeric) days
  ##  bin: (numeric) bins
  ##
  ## Value:
  ##  NONE
  ##
  ## Author:
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  ####
  if (NCOL(x) > 1)
  {
    x <- x[,1]
  }

  #### Series
  xDec   <- .decomposeSeries(x, calendar)
  resDec <- .decomposeSeries(inference$residuals, calendar)
  estDec <- inference$filter

  #### Common ylim's
  ylimX      <- c(0, max( max(xDec$x)   , max(estDec$x)   ) )
  ylimEta    <- c(0, max( max(xDec$eta) , max(estDec$eta) ) )
  ylimSeas   <- c(
                  min( min(xDec$seas), min(estDec$seas) ),
                  max( max(xDec$seas), max(estDec$seas) )
                  )
  ylimMu     <- c(0,
                  max( max(xDec$seasMu,   na.rm = TRUE),
                       max(estDec$seasMu, na.rm = TRUE),
                       max(xDec$mu,       na.rm = TRUE),
                       max(estDec$mu,     na.rm = TRUE))
                  )
  ylim       <- list(x = ylimX, eta = ylimEta,
                     seas = ylimSeas, mu = ylimMu )

  ylimBinRes <- c(
                  min( min(xDec$seas), min(estDec$seas) ),
                  max( max(xDec$seas), max(estDec$seas) )
                  )


  #### Plots
  par(mfrow = c(2,5))
  .plotDiagnostics1(xDec  , calendar, "../data", ylim)
  .plotDiagnostics1(estDec, calendar, "fit" , ylim)

  x11()
  par(mfrow = c(2,5))
  .plotDiagnostics2(xDec  , calendar, "../data")
  .plotDiagnostics2(resDec, calendar, "residuals")

  x11()
  par(mfrow = c(2,2))
  nBin <- max(calendar$bin)
  .plot.acfAllBins(xDec$mu  , nBin, "../data")
  .plot.acfAllBins(resDec$mu, nBin, "residuals")


  #### Answer
  NULL
}
# ------------------------------------------------------------------------------
