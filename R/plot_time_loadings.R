#' Compute ANOVA Simultaneous Component Analysis (ASCA) of Temporal Check All That Apply (TCATA) data.
#' @param ASCA_obj A list of PCA estimated by the TCATASCA function.
#' @param object A vector of string or numbers indicating which parameter of the ASCA decomposition will be plotted
#' @param axes A numeric vector indicating two numbers indicating the axes of the ASCA decomposition.
#' @param ref A string vector indicating which graph print. "attributes" prints a plot using the attributes factor as a faceting variable. "factor" prints a plot with the attribute variable as a faceting factor.
#' @param choice A string that could be "loadings" or "contrib", to indicate whether the loading values or the contribution values will be estimatd and plotted.
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
#' # The function creates two plots from an ASCA_obj in long format
#'
#' ASCA_obj <- asca_tcata(CATA~(samp+cons)^2, data = tempR::ojtcata  %>%
#' gather(time, CATA, 5:25) %>%
#'  mutate(cons = as.factor(cons), samp = as.factor(samp),
#'         time = as.numeric(str_extract(time, "\\d+"))), timecol = "time",
#'         attributes = "attribute")
#'
#' plot_time_loadings(ASCA_obj)
#'
#' # it is possible to define which are the factor considered using the object
#' # parameter, to define wich factors will be reported in the plots
#'
#' plot_time_loadings(ASCA_obj, object = 1:2)
#'
#' #On default settings, the function reports the estimation of the contribution
#' # on the overall variance of each attribute among time.
#' # To report the loading values it is necessary to define `choice` = "loadings",
#' # once it is defined, the function reports a line plot presenting the loading values
#' # among time for each attribute.
#'
#' plot_time_loadings(ASCA_obj, choice = "loadings")
#'
#' # It is possible to define for which axes the contribution or
#' # the loading values will be reported. You can define them at the axes variable
#'
#' plot_time_loadings(ASCA_obj, axes = c(2,3))
#'
#' }

plot_time_loadings <- function(
    ASCA_obj,
    choice = "contrib",
    ref = c("attributes", "factors"),
    object = NA,
    print = T,
    axes = c(1,2)){

  if(is.na(object)){
    object <- 1:(length(names(ASCA_obj)[!(names(ASCA_obj) %in% c(
      "Residuals","Parameters", "SS_decomposition", "info"))]))
  }

  resulting_plots <- list()
  contrib <- data.frame()
  name <- NULL
  Factor <- NULL
  Class <- NULL
  Attributes <- NULL
  . <- NULL


  for(reference in object){

    if(choice == "contrib"){
    contrib <- rbind(contrib, ASCA_obj %>% .[[reference]] %>%
      fviz_contrib(choice = "var", axes = c(axes)) %>% .$data %>%
      mutate(Class = str_extract(name, "\\d+\\.?\\d*"),
        Factor = names(ASCA_obj)[reference]))
    }
    if(choice == "loadings"){
      contrib <- rbind(contrib, ASCA_obj %>% .[[reference]] %>%
        .$rotation %>% .[,axes[1]] %>% as.data.frame() %>%
          `colnames<-`("contrib") %>%
            mutate(name = rownames(.),
              Class = str_extract(name, "\\d+\\.?\\d*"),
              Factor = names(ASCA_obj)[reference]))
    }
  }

if(ASCA_obj[["info"]][["type"]] %in% c("TDS_ASCA", "TCATA_ASCA")){
  contrib %>%
    mutate(Attributes = str_extract(name,
            paste((ASCA_obj$info$attributes), collapse = "|"))) %>%
    group_by(Factor, Class, Attributes) %>%
    summarize(contrib = sum(contrib)) %>% ungroup() %>%
    mutate(contrib_text = as.character(round(contrib,2)),
           Class = as.numeric(Class)) %>% ungroup() %>%
    droplevels() -> data
}

  if(ASCA_obj[["info"]][["type"]] == "TI_ASCA"){
    contrib %>%
      group_by(Factor, Class) %>%
      summarize(contrib = sum(contrib)) %>% ungroup() %>%
      mutate(contrib_text = as.character(round(contrib,2)),
             Class = as.numeric(Class)) %>% ungroup() %>%
      droplevels() -> data
  }

if(choice== "loadings"){
  title <- "Loadings value in time organized per factor"
  subtitle <- paste0("Estimations on axe: ", axes[1])
  ylab_text <- "Loading values"
}

  if(choice == "contrib"){
    title <- "Cumulative contribution of loadings on time organized for attributes"
    subtitle <- paste0("Estimation on axes: ", axes[1], ", ", axes[2])
    ylab_text <- "Contribution to explained variance"
  }

if(ASCA_obj[["info"]][["type"]] %in% c("TDS_ASCA", "TCATA_ASCA")){
if("factors" %in% ref){
  resulting_plots[["factors"]] <- data %>% ggplot() +
    geom_line(aes(x = Class, y = contrib, color = Attributes), size = 0.8) +
    xlab("time") + ylab(ylab_text) +
    facet_wrap(~Factor, scales = "free") + theme_minimal() +
    ggtitle(title, subtitle = subtitle) +
    theme(legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, family = "bold"),
      plot.subtitle = element_text(hjust = 0.5, family = "italic"),
      axis.text = element_text(color = "black")) +
    scale_x_continuous(
      breaks = seq(0, max(data$Class, na.rm = T),5))
}

if("attributes" %in% ref){
  resulting_plots[["attributes"]] <- data %>% ggplot() +
    geom_line(aes(x = Class, y = contrib, color = Factor), size = 0.8) +
    xlab("time") + ylab(ylab_text) +
    facet_wrap(~Attributes, scales = "free") +
    theme_minimal() +
    ggtitle(title, subtitle = subtitle) +
    theme(
      legend.position = "bottom",
      axis.text = element_text(color = "black"),
      plot.subtitle = element_text(hjust = 0.5, family = "italic"),
      plot.title = element_text(hjust = 0.5, family = "bold")) +
    scale_x_continuous(breaks = seq(0,max(data$Class, na.rm = T),5))
}
}

if(ASCA_obj[["info"]][["type"]] == "TI_ASCA"){
  resulting_plots[["attributes"]] <- data %>% ggplot() +
    geom_line(aes(x = Class, y = contrib, color = Factor), size = 0.8) +
    xlab("time") + ylab(ylab_text) +
    theme_minimal() +
    ggtitle(title, subtitle = subtitle) +
    theme(
      legend.position = "bottom",
      axis.text = element_text(color = "black"),
      plot.subtitle = element_text(hjust = 0.5, family = "italic"),
      plot.title = element_text(hjust = 0.5, family = "bold")) +
    scale_x_continuous(breaks = seq(0,max(data$Class, na.rm = T),5))
}



if(print){print(resulting_plots[[ref]])}
return(resulting_plots)

}







