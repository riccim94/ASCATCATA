#' Compute ANOVA Simultaneous Component Analysis (ASCA) of Temporal Check All That Apply (TCATA) data.
#' @param ASCA_obj A list of PCA estimated by the TCATASCA function.
#' @param object A vector of string or numbers indicating which parameter of the ASCA decomposition will be plotted
#' @param dimensions A numeric vector indicating two numbers indicating the dimensions of the ASCA decomposition.
#' @param ref A string vector indicating which graph print. "attributes" prints a plot using the attributes factor as a faceting variable. "factor" prints a plot with the attribute variable as a faceting factor.
#' @param choice A string that could be "loadings" or "contrib", to indicate whether the loading values or the contribution values will be estimatd and plotted.
#' @param lab_two_lvl Logical. Adds a label indicating the direction where the score values are if in the plot there are loading values from a factor with only two levels. Default is FALSE.
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
#' @importFrom ggplot2 geom_label
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
#' # It is possible to define for which dimensions the contribution or
#' # the loading values will be reported. You can define them at the dimensions variable
#'
#' plot_time_loadings(ASCA_obj, dimensions = c(2,3))
#'
#' }

plot_time_loadings <- function(
    ASCA_obj,
    choice = "contrib",
    ref = c("attributes", "factors"),
    object = NA,
    print = TRUE,
    lab_two_lvl = FALSE,
    dimensions = c(1,2)){

  if(is.na(object[1])){
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
  nfactor <- NA
  label_data <- NA

  for(reference in object){

    if(choice == "contrib"){
    contrib <- rbind(contrib, ASCA_obj %>% .[[reference]] %>%
      fviz_contrib(choice = "var", axes = c(dimensions)) %>% .$data %>%
      mutate(Class = str_extract(name, "\\d+\\.?\\d*"),
        Factor = names(ASCA_obj)[reference]))
    }

    if(choice == "loadings"){
      row_num <- nrow(ASCA_obj %>% .[[reference]] %>% .$x)
      test_nrow <- row_num == 2

      contrib <- rbind(contrib, ASCA_obj %>% .[[reference]] %>%
        .$rotation %>% .[,dimensions[1]] %>% as.data.frame() %>%
          `colnames<-`("contrib") %>%
            mutate(name = rownames(.),
              Class = str_extract(name, "\\d+\\.?\\d*"),
              Factor = names(ASCA_obj)[reference]))


      if(test_nrow & lab_two_lvl){
        nfactor <- ifelse(is.numeric(reference),
                          names(ASCA_obj)[[reference]], reference)

        label_data <- data.frame(labels = paste0(nfactor,":\n",
          ASCA_obj %>% .[[reference]] %>% .$x %>% rownames(.)),
                    position = ASCA_obj %>% .[[reference]] %>%
                      .$x %>% .[,dimensions[1]] == ASCA_obj %>% .[[reference]] %>%
                      .$x %>% .[,dimensions[1]] %>% max(.)) %>%
          mutate(position = ifelse(position,
              max(ASCA_obj %>% .[[reference]] %>%
                    .$rotation %>% .[,dimensions[1]]),
              min(ASCA_obj %>% .[[reference]] %>%
                    .$rotation %>% .[,dimensions[1]])))

      }

    }
  }

if(ASCA_obj[["info"]][["type"]] %in% c("TDS_ASCA", "TCATA_ASCA")){
  contrib %>%
    mutate(Attributes = str_extract(name,
            paste((ASCA_obj$info$attributes), collapse = "|"))) %>%
    group_by(Factor, Class, Attributes) %>%
    reframe(contrib = sum(contrib)) %>% ungroup() %>%
    mutate(contrib_text = as.character(round(contrib,2)),
           Class = as.numeric(Class)) %>% ungroup() %>%
    droplevels() -> data
}

  if(ASCA_obj[["info"]][["type"]] == "TI_ASCA"){
    contrib %>% group_by(Factor, Class) %>% reframe(contrib = sum(contrib)) %>%
      ungroup() %>% mutate(contrib_text = as.character(round(contrib,2)),
             Class = as.numeric(Class)) %>% droplevels() -> data
  }

if(choice== "loadings"){
  title <- "Loadings value in time organized per factor"
  subtitle <- paste0("Estimations on dimension: ", dimensions[1])
  ylab_text <- "Loading values"
}

  if(choice == "contrib"){
    title <- "Cumulative contribution of loadings on time organized for attributes"
    subtitle <- paste0("Estimation on dimensions: ", dimensions[1], ", ", dimensions[2])
    ylab_text <- "Contribution to explained variance"
  }

if(ASCA_obj[["info"]][["type"]] %in% c("TDS_ASCA", "TCATA_ASCA")){
if("factors" %in% ref){
  resulting_plots[["factors"]] <- data %>% ggplot() +
    geom_line(aes(x = Class, y = contrib, color = Attributes), size = 0.8) +
    xlab("time") + ylab(ylab_text) +
    facet_wrap(~Factor, scales = "free") + theme_minimal() +
    {
      if(choice== "loadings"){
        geom_hline(yintercept = 0, linetype = 2)
      }
    } + ggtitle(title, subtitle = subtitle) +
    theme(legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "italic"),
      axis.text = element_text(color = "black")) +
    scale_x_continuous(
      breaks = seq(0, max(data$Class, na.rm = T),5))
}

if("attributes" %in% ref){
  resulting_plots[["attributes"]] <- data %>% ggplot() +
    geom_line(aes(x = Class, y = contrib, color = Factor), size = 0.8) +
    xlab("time") + ylab(ylab_text) +
    facet_wrap(~Attributes, scales = "free") +
    { if(choice== "loadings"){geom_hline(yintercept = 0, linetype = 2) } } +
    { if(choice== "loadings" & lab_two_lvl & is.data.frame(label_data)){
        geom_label(aes(x = min(data$Class), y = position, label = labels),
                   data = label_data) } } + theme_minimal() +
    ggtitle(title, subtitle = subtitle) +
    theme(
      legend.position = "bottom",
      axis.text = element_text(color = "black"),
      plot.subtitle = element_text(hjust = 0.5, face = "italic"),
      plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_x_continuous(breaks = seq(0,max(data$Class, na.rm = T),5))
}
}

if(ASCA_obj[["info"]][["type"]] == "TI_ASCA"){
  resulting_plots[["attributes"]] <- data %>% ggplot() +
    geom_line(aes(x = Class, y = contrib, color = Factor), size = 0.8) +
    xlab("time") + ylab(ylab_text) + theme_minimal() +
    ggtitle(title, subtitle = subtitle) +
    { if(choice== "loadings"){
        geom_hline(yintercept = 0, linetype = 2) } }  +
    {
      if(choice== "loadings" & lab_two_lvl & is.data.frame(label_data)){
        geom_label(aes(x = min(data$Class), y = position, label = labels),
                   data = label_data) } } +
    theme(legend.position = "bottom",
      axis.text = element_text(color = "black"),
      plot.subtitle = element_text(hjust = 0.5, face = "italic"),
      plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_x_continuous(breaks = seq(0,max(data$Class, na.rm = T),5))
}



if(print){
  print(resulting_plots)
  }

invisible(resulting_plots)

}







