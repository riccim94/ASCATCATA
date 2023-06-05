#' Compute permutation test to validate the results from ANOVA Simultaneous Component Analysis (ASCA) of Temporal Check All That Apply (TCATA) data.
#' @param data A data frame, or object coercible by as.data.frame to a data frame, containing the variables in the model. It must be in long format and must contain a column for Temporal Check All That Apply binary data, a column reporting the time values, and a column that defines the attributes analyzed.
#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The formula will be applied for each atttribut at each time interval defined by the timecol column
#' @param timecol A string containing the name of the column indicating the time intervals of the TCATA dataset.
#' @param attributes A string containing the name of the column indicating the attributes of the TCATA.
#' @param ... Optional parameters
#' @return A list of objects containing the results of ASCA decomposition of a structured dataset.
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @importFrom tibble is_tibble
#' @export
#' @examples
#' \dontrun{
#' perm_tcatasca(CATA~(samp+cons)^2, data = tempR::ojtcata, time, attribute)
#' describe(dataset, col1, col2)
#' }
#'


perm_tcatasca <- function(data, formula, timecol, attributes, ...){

}



