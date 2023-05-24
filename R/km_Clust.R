#' Estimation of clusters on the ASCA results vie K-means clustering
#' @param ASCA_obj A list of PCA estimated by the TCATASCA function.
#' @param object A vector of string or numbers indicating which parameter of the ASCA decomposition will be plotted
#' @param axes A numeric vector indicating two numbers indicating the axes of the ASCA decomposition.
#' @return A plot representing the loadings of the ASCA decomposition and the density plot of the loadings of ASCA decomposition.
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract
#' @importFrom factoextra fviz_contrib
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 scale_x_continuous
#' @export
#' @examples
#' \dontrun{
#' plot_time_loadings(ASCA_obj, object = 1:3, axes = c(1,2), print = T)
#' }



