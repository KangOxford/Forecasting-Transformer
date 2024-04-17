################################################################################
##
## File: vWAPUtilities_20081220.R
##
## Purpose:
##  R utilities for the daily-intradaily MEM.
##
## Created: 2008.12.20
##
## Version: 2009.03.05
##
## Author:
##  Fabrizio Cipollini <cipollini@ds.unifi.it>
##
################################################################################

################################################################################
## FUNCTION:                       TASK:
##
## PART 1: Mathematical FUNCTIONS
##  A.op.tx()                       Compute 'A op t(x)', where A is a matrix and
##                                   x a vector with as many elements as the
##                                   columns of A.
## PART 2: Recoding FUNCTIONS
##  .factor()                       Recode values using a conversion matrix by 
##                                   means of old and new codes.
################################################################################


################################################################################
## PART 1: Mathematical FUNCTIONS
##  A.op.tx(A, x, binOp)            Compute 'A op t(x)', where A is a matrix and
##                                   x a vector with as many elements as the
##                                   columns of A.
################################################################################

A.op.tx <-
function(A, x, binOp)
{
  ##############################################################################
  ## Description:
  ##  Compute 'A op t(x)', where A is a matrix and x a vector with as many
  ##  elements as the columns of A.
  ##
  ## Arguments:
  ##  A: (numeric)  [m,n] matrix
  ##  x: (numeric)  [n]   vector
  ##  binOp: (character) [1] binary operator
  ##
  ## Value:
  ##  out: (matrix)  [m,n] matrix of 'A[i,j] binOp x[j]' values
  ##
  ## Author:
  ##   Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION

  #### Answer
  eval( parse( text =
    paste("A", binOp, "rep.int( x, rep.int(NROW(A), NROW(x)) )", sep = "")
      ) )
}
# ------------------------------------------------------------------------------


.factor <- 
function(x, levels, labels, typeNum = TRUE)
{
  ##############################################################################
  ## Description:
  ##  Recode values of a vector using a conversion matrix with old (levels) 
  ##  and new (labels) codes.
  ##
  ## Arguments:
  ##  x: (vector[n]) original values
  ##  levels: (vector[k]) current codes
  ##  labels: (vector[k]) new codes
  ##  typeNum: (logical[1]) if TRUE (default) returns a numeric otherwise a
  ##   character
  ## 
  ## Value: 
  ##  out: (vector[n]) recoded values
  ##
  ## Implemented by: 
  ##  Fabrizio Cipollini
  ##############################################################################

  ## FUNCTION:

  #### match
  ind <- match(x = x, table = levels)
  
  #### Build
  x <- labels[ind]

  #### Convert output
  if ( as.logical(typeNum[1]) )
  {
    as.numeric(x)
  }
  else
  {             
    as.character(x)       
  }
}
# ------------------------------------------------------------------------------

