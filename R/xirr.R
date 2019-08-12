#' xirr(cashflows, guess, tol, m)
#'
#' Calculates the Internal Rate of Return of a series of cashflows at irregular intervals.
#'
#' @param cashflows: a tibble in which each element is of the form (date, amount), where date is an r lubridate object and amount is an integer or floating point number. Cash outflows (investments) are represented with negative amounts, and cash inflows (returns) are positive amounts.
#' @param guess (optional, default = 0.1): a guess at the solution to be used as a starting point for the numerical solution.
#' @param tol (optional, default = 0.000000001): the error tolerance.
#' @param m (optional, default = 500): the maxmimum number (integer) of iterations used for the secant method.
#' @importFrom cmna secant
#' @return Returns the IRR as a single value (floating point number)
#' @note The Internal Rate of Return (IRR) is the discount rate at which the Net Present Value (NPV) of a series of cash flows is equal to zero. The NPV of the series of cash flows is determined using \code{xnpv} function of this package. The discount rate at which NPV equals zero is found using the secant method of the cmna package. This \code{xirr} function is equivalent to the Microsoft Excel function of the same name.
#'
#' @export
xirr <- function (cashflows, guess, tol, m){
  if(missing(guess)){
    guess = 0.1
  }
  if(missing(tol)){
    tol = 0.000000001
  }
  if (missing(m)){
    m = 500
  }

  f <- function(guess){xnpv(guess, cashflows)}

  secant(f, guess, tol, m)}

