#' Compute ANOVA Simultaneous Component Analysis (ASCA) of Temporal Check All That Apply (TCATA) data.
#' @param ASCA_obj A list of PCA estimated by the TCATASCA function.
#' @param object A vector of string or numbers indicating which parameter of the ASCA decomposition will be plotted
#' @param axes A numeric vector indicating two numbers indicating the axes of the ASCA decomposition.
#' @param ref A string vector indicating which graph print. "attributes" prints a plot using the attributes factor as a faceting variable. "factor" prints a plot with the attribute variable as a faceting factor.
#' @param print Logical. Indicates wether or not to print the plots.
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

plot_time_loadings <- function(
    ASCA_obj,
    ref = c("attributes", "factors"),
    object = 1:(length(ASCA_obj)-2),
    print = T,
    axes = c(1,2)){
  resulting_plots <- list()
  contrib <- data.frame()
  for(reference in object){
    contrib <- rbind(contrib, ASCA_obj %>% .[[reference]] %>%
      fviz_contrib(choice = "var", axes = c(axes)) %>% .$data %>%
      mutate(
        Class = str_extract(name, "^\\d+"),
        Factor = names(ASCA_obj)[reference] ))
  }
  contrib %>%
    mutate(Attributes = str_extract(name,
            paste(((ASCA_obj$info$attributes)), collapse = "|"))) %>%
    group_by(Factor, Class, Attributes) %>%
    summarize(contrib = sum(contrib)) %>% ungroup() %>%
    mutate(contrib_text = as.character(round(contrib,2)),
           Class = as.numeric(Class)) %>% ungroup() %>%
    droplevels() -> data

if("factors" %in% ref){
  resulting_plots[["factors"]] <- data %>% ggplot() +
    geom_line(aes(x = Class, y = contrib, color = Attributes), size = 0.8) +
    xlab("time") + ylab("Contribution to explained variance") +
    facet_wrap(~Factor, scales = "free") +
    theme_minimal() +
    ggtitle("Cumulative contribution of loadings on time organized for factors",
            subtitle = paste0("Estimation on axes: ", axes[1], ", ", axes[2])) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, family = "bold"),
      plot.subtitle = element_text(hjust = 0.5, family = "italic"),
      axis.text = element_text(color = "black")) +
    scale_x_continuous(breaks = seq(min(data$Class), max(data$Class),5))
}

if("attributes" %in% ref){
  resulting_plots[["attributes"]] <- data %>% ggplot() +
    geom_line(aes(x = Class, y = contrib, color = Factor), size = 0.8) +
    xlab("time") + ylab("Contribution to explained variance") +
    facet_wrap(~Attributes, scales = "free") +
    theme_minimal() +
    ggtitle(
      "Cumulative contribution of loadings on time organized for attributes",
      subtitle = paste0("Estimation on axes: ", axes[1], ", ", axes[2])) +
    theme(
      legend.position = "bottom",
      axis.text = element_text(color = "black"),
      plot.subtitle = element_text(hjust = 0.5, family = "italic"),
      plot.title = element_text(hjust = 0.5, family = "bold")) +
    scale_x_continuous(breaks = seq(min(data$Class),max(data$Class),5))
}
if(print){print(resulting_plots[[ref]])}
return(resulting_plots)

}







