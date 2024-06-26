#' Creates a plot to report the results of the permutation test by the `perm_asca()` function.
#' @param ASCA_obj A list of PCA estimated by the TCATASCA function.
#' @param perm_object A list of estimated values from permutation test estimated using the perm_asca() function.
#' @param axes A numeric vector indicating two numbers indicating the axes of the ASCA decomposition.
#' @param point.size A numeric value defining the size of the points of the score values.
#' @param max.overlaps.value Numeric, default is 10. Define the maximum number of overlaps allowed for text in the plots.
#' @param print Logical. Indicates whether or not to print the plots.
#' @return A series of plots representing the results of the permutation test and the results of the bootstrap tests.
#' @export
#' @examples
#'
#' \dontrun{
#'#'ASCA_model <- asca_tcata(CATA~(samp+cons)^2, data = data.long, timecol = "time",
#'         attributes = "attribute")
#' perm_model <- perm_asca(data = data.long, ASCA_object = ASCA_model,
#' nrep = 1000, test = "permutation")
#'
#'plot_permutation(perm_model)
#'
#' }
#'
#'
#'
#'

plot_permutation <- function(
    ASCA_obj,
    perm_object,
    axes = c(1,2),
    point.size = 2,
    max.overlaps.value = 10,
    print = T
    ){

}


