#' Creates bi-plots for each factor decompsed in the ASCA object elaborated by the function tcatasca().
#' @param ASCA_obj A list of PCA estimated by the TCATASCA function.
#' @param object A vector of string or numbers indicating which parameter of the ASCA decomposition will be plotted
#' @param axes A numeric vector indicating two numbers indicating the axes of the ASCA decomposition.
#' @param path Logical. Adds a path indicating the position of the loadings according to their cronological order. Default is TRUE.
#' @param path.smooth Logical. Adds a path smoothed indicating the position of the loadings according to their cronological order. Default is TRUE.
#' @param density Logical. Superimpose a 2-dimensional density plot, indicating th distribution of the temporal resolved loadings according to their density. Default is FLASE.
#' @param h_clus Numeric. Indicates wether to calculate or not hierchical clustering for the levels of each factor. The algorithm applied for hierarchical clustering is "Ward-D2", and it is estimated from euclidean distance for all the principal component estimated.
#' @param point.size A numeric value defining the size of the points of the score values.
#' @param print Logical. Indicates wether or not to print the plots.
#' @return A series of plots representing the scores of the ASCA decomposition and the values of the loadings of the same ASCA decomposition.
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @importFrom magrittr %>%
#' @importFrom stats reorder
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 stat_density2d
#' @importFrom ggplot2 after_stat
#' @importFrom ggplot2 scale_alpha_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 arrow
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 stat_ellipse
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @export
#' @examples
#' \dontrun{
#' plot_ASCA(ASCA_obj, object = 1:3)
#' }

plot_ASCA <- function(
    ASCA_obj,
    object = 1:(length(ASCA_obj)-3),
    print = T,
    axes = c(1,2), path = T, density = F,
    path.smooth = T,
    h_clus = NULL,
    point.size = 2){

  { unicode_minus <- function(x){sub('^-', '\U2212', format(x))} }

  cluster <- NULL
  param <- NULL
  level <- NULL
  time <- NULL
  col_p <- NULL
  . <- NULL

  attr <- ASCA_obj$info$attributes
  num_attr <- length(attr)


  resulting_plots <- list()

for(reference in object){
  row_num <- nrow(ASCA_obj %>% .[[reference]] %>% .$x)
  test_nrow <- row_num == 2

  if(test_nrow){
    message(paste0("The factor ", ASCA_obj %>% names() %>% .[reference],
    " contains only 2 levels, therefore can't be represented using a biplot."));
  next
    }
  ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>% as.data.frame() %>%
    dplyr::select(axes) %>% `colnames<-`(c("x", "y")) -> ind

  ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>% as.data.frame() %>%
    dplyr::select(axes) %>% `colnames<-`(c("x", "y")) -> var

  r <- min((max(ind[,"x"]) - min(ind[,"x"])/(
    max(var[,"x"]) - min(var[,"x"]))),
    (max(ind[,"y"]) - min(ind[,"y"])/(
      max(var[,"y"]) - min(var[,"y"]))))

axes_x <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>% as.data.frame() %>%
  colnames(.) %>% .[axes[1]]

axes_y <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>% as.data.frame() %>%
  colnames() %>% .[axes[2]]


axe_x_title <- paste0("Dim", axes[1]," (", as.character(ASCA_obj %>%
 .[[reference]] %>% summary() %>% .[] %>% .$importance %>% .[2,axes[1]]*100) %>%
   str_extract("\\d+\\.\\d{1,2}"), "%)")


axe_y_title <- paste0("Dim",axes[2]," (", as.character(ASCA_obj %>%
 .[[reference]] %>% summary() %>% .[] %>% .$importance %>% .[2,axes[2]]*100) %>%
   str_extract("\\d+\\.\\d{1,2}"), "%)")


if(is.numeric(h_clus)){
  if(h_clus > row_num){
    factor <- ASCA_obj %>% names() %>% .[reference]
    message(paste0("The factor ", factor, " has only ", row_num,
      " levels, therefore can not be clustered in ", h_clus, " groups."))
    next
  }
  ASCA_obj %>% .[[reference]] %>% .$x %>% dist() %>%
    hclust(method = "ward.D2") %>% cutree(k = h_clus) %>% as.data.frame() %>%
    `colnames<-`(c("cluster")) %>% mutate(col_p = rownames(.),
           cluster = paste0("cluster ", cluster)) -> cluster_km
  ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>%
    as.data.frame() %>% .[,axes] %>% mutate(col_p = rownames(.)) %>%
    left_join(cluster_km, by = c("col_p")) %>%
    mutate(cluster = as.factor(cluster)) -> data_plot
}else{
  data_plot <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>%
  as.data.frame() %>% .[,axes] %>% mutate(col_p = rownames(.), cluster = NA)
  }




pl <- ggplot()
if(density){
  pl <- pl + stat_density2d(aes(x = !!sym(axes_x), y = !!sym(axes_y),
        fill = param, alpha = after_stat(level/max(level))),
    data = ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
      as.data.frame() %>% .[,axes] %>%
      mutate_all(function(x){x <- x*(r)}) %>%
      mutate(param = rownames(.)) %>%
      separate(param, c("time", "param"), sep = "_") %>% arrange(as.numeric(time)) %>%
        mutate(time = reorder(time, as.numeric(time), mean)),
    geom = "polygon") +
    theme_minimal() + scale_alpha_continuous(range = c(0.25,0.7)) +
    guides(fill = guide_legend(title = "Attributes")) +
    theme(plot.title = element_text(hjust = 0.5, face = "italic"),
          legend.position = "bottom", legend.title = element_blank())
  }


  pl <- pl + {
    if(is.numeric(h_clus)){
      geom_point(aes(x = !!sym(axes_x), y = !!sym(axes_y), color = cluster),
            data = data_plot, size = point.size)
        }else{
          geom_point(aes(x = !!sym(axes_x), y = !!sym(axes_y)),
            color = "black", data = data_plot, size = point.size)
        }
      } + scale_shape_manual(values = rep(19,7)) +
    scale_x_continuous(labels = unicode_minus) +
    scale_y_continuous(labels = unicode_minus) + theme_minimal() +
      {if(is.numeric(h_clus)){
        geom_text_repel(aes(x = !!sym(axes_x), y = !!sym(axes_y),
          label = col_p, color = cluster), data = data_plot)
        }else{
          geom_text_repel(aes(x = !!sym(axes_x), y = !!sym(axes_y),
            label = col_p), data = data_plot) }} +
    theme(legend.position = "none") +
    guides(color = "none", alpha = "none") +
    xlab(axe_x_title) + ylab(axe_y_title)


  if(path.smooth){
    pl <- pl + geom_path(
      aes(x = !!sym(axes_x), y = !!sym(axes_y), color = param),
      linetype = 1, linewidth = 5, alpha = 0.3,
        data = ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
          as.data.frame() %>% .[,axes] %>%
          mutate_all(function(x){x <- x*(r)}) %>%
      mutate(param = rownames(.)) %>%
        separate(param, c("time", "param"), sep = "_") %>%
        arrange(as.numeric(time)) %>%
        mutate(time = reorder(time, as.numeric(time), mean))) +
      guides(color = guide_legend(title = "Legend"))
  }

    if(path){
      pl <- pl + geom_path(aes(x = !!sym(axes_x), y = !!sym(axes_y),
        color = param), linetype = 1, arrow = arrow(type = "closed",
          length = unit(0.25, "cm")),
        data = ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
          as.data.frame() %>% .[,axes] %>%
          mutate_all(function(x){x <- x*(r)}) %>%
          mutate(param = rownames(.)) %>%
          separate(param, c("time", "param"), sep = "_") %>%
          arrange(as.numeric(time)) %>%
          mutate(time = reorder(time, as.numeric(time), mean))) +
        guides(color = guide_legend(title = "Attributes"))
    }





if(is.numeric(reference)){
  final_label <- names(ASCA_obj)[reference]
}
if(is.character(reference)){
  final_label <- reference
}


pl <- pl + geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  # {
  #   if(is.numeric(h_clus)){
  #     scale_color_manual(breaks = c(attr),
  #       values = c(palette(hcl.colors(h_clus, palette = "Green-Brown")),
  #                  palette(hcl.colors(num_attr, palette = "Set 2"))))
  #   }else{
  #     scale_color_manual(breaks = c(attr),
  #       values = palette(hcl.colors(num_attr, palette = "Set 2")))
  #   } } +
  ggtitle("", subtitle = paste0("Factor: ", final_label)) +
  theme(legend.position = "bottom")
resulting_plots[[names(ASCA_obj)[reference]]] <- pl
if(print){print(pl)}
}
  invisible(resulting_plots)

  }



