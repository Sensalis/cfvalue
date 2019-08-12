#' xnpv(rate,cashflows):
#'
#' Calculates the net present value (NPV) of a series of dated cashflows at irregular intervals.
#'
#' @param rate: The discount rate to be applied to the cash flows.
#' @param cashflows: A data frame object in which each element is a tuple of the form (date, amount), where date is a lubridate package date object in the form of yyyy-mm-dd and amount is an integer or floating point number. Cash outflows (investments) are represented with negative amounts, and cash inflows (returns) are positive amounts.
#' @importFrom plyr arrange
#' @importFrom lubridate ymd
#' @return Returns a single value which is the NPV of the given cash flows.
#' @note The Net Present Value is the sum of each of cash flows discounted back to the date of the first cash flow, in t0. The formula rounds the result to the next integer.
#' @details The discounted value of a given cash flow is cfamount/(1+r)**(t-t0), where cfamount is the amount, r is the discout rate, and (t-t0) is the time in years (based on a 365-days per year calculation) from the date of the first cash flow in the series (t0) to the date of the cash flow being added to the sum (vec). This function is equivalent to the Microsoft Excel function of the same name.
#'
#' @export
xnpv <- function(rate, cashflows){
  chron_order <-  arrange(cashflows,cfdate)
  t0 <- chron_order$cfdate[1]
  len <- length(chron_order$cfamount)
  vec <- vector("double", len)
  for (t in 1:len){vec[t]<-chron_order$cfamount[t]/(1+rate)**(as.double(chron_order$cfdate[t]-t0)/365)}
  xnpv <- sum(vec)
  return(xnpv)
}
