#' Compute ANOVA Simultaneous Component Analysis (ASCA) of Temporal Check All That Apply (TCATA) data.
#' @param ASCA_obj A list of PCA estimated by the TCATASCA function.
#' @param object A vector of string or numbers indicating which parameter of the ASCA decomposition will be plotted
#' @param axes A numeric vector indicating two numbers indicating the axes of the ASCA decomposition.
#' @param path Logical. Adds a path indicating the position of the loadings according to their cronological order. Default is TRUE.
#' @param path.smooth Logical. Adds a path smoothed indicating the position of the loadings according to their cronological order. Default is TRUE.
#' @param density Logical. Superimpose a 2-dimensional density plot, indicating th distribution of the temporal resolved loadings according to their density. Default is FLASE.
#' @param point.size A numeric value defining the size of the points of the score values.
#' @return A plot representing the scores of the ASCA decomposition and the density plot of the loadings of ASCA decomposition.
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import magrittr
#' @importFrom stats reorder
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 stat_density2d
#' @importFrom ggplot2 after_stat
#' @importFrom ggplot2 scale_alpha_continuous
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_point
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
    ASCA_obj, object = 1:(length(ASCA_obj)-2),
    axes = c(1,2), path = T, density = F,
    path.smooth = T,
    point.size = 2){
for(reference in object){
  ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>% as.data.frame() %>%
    dplyr::select(axes) %>%
    #mutate_all(function(x){x/20}) %>%
    `colnames<-`(c("x", "y")) -> ind

  ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
    as.data.frame() %>%
    #mutate_all(function(x){x <- x*5}) %>%
    dplyr::select(axes) %>%
    `colnames<-`(c("x", "y")) -> var

  r <- min((max(ind[, "x"]) - min(ind[, "x"])/(
    max(var[, "x"]) - min(var[, "x"]))),
    (max(ind[, "y"]) - min(ind[, "y"])/(
      max(var[, "y"]) - min(var[, "y"]))))

axes_x <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>% as.data.frame() %>%
  colnames(.) %>% .[axes[1]]

axes_y <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>% as.data.frame() %>%
  colnames() %>% .[axes[2]]

axe_x_title <- paste0("Dim", axes[1]," (", ASCA_obj %>% .[[reference]] %>%
  summary() %>% .[] %>% .$importance %>% .[2,axes[1]]*100 %>% round(., 1), "%)")

axe_y_title <- paste0("Dim",axes[2]," (", ASCA_obj %>% .[[reference]] %>%
  summary() %>% .[] %>% .$importance %>% .[2,axes[2]]*100 %>% round(., 1), "%)")

pl <- ggplot()
if(density){
  pl <- pl + stat_density2d(
    aes(x = !!sym(axes_x), y = !!sym(axes_y),
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
   # stat_ellipse(aes(x = !!sym(axes_x), y = !!sym(axes_y)), type = "euclid",
   #              level = 0.95,
   #              data = ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
   #                as.data.frame() %>% .[,axes] %>%
   #                mutate_all(function(x){x <- x*(r*0.7)})) +
   # stat_ellipse(aes(x = !!sym(axes_x), y = !!sym(axes_y)),type = "euclid",
   #              level = 0.5, linetype = 2,
   #              data = ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
   #                as.data.frame() %>% .[,axes] %>%
   #                mutate_all(function(x){x <- x*(r*0.7)})) +
    pl <- pl + geom_point(
      aes(x = !!sym(axes_x), y = !!sym(axes_y)#, color = col_p
          ), data = ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>%
        as.data.frame() %>% .[,axes] %>%
        #mutate_all(function(x){x <- x*(r*0.7)}) %>%
        mutate(col_p = rownames(.)), size = point.size) +
    scale_shape_manual(values = rep(19,7)) + theme_minimal() +
    geom_text_repel(aes(x = !!sym(axes_x), y = !!sym(axes_y),
        label = col_p#, color = col_p
        ),
        data = ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>%
          as.data.frame() %>% .[,axes] %>%
          #mutate_all(function(x){x <- x*(r*0.7)}) %>%
          mutate(col_p = rownames(.))) +
    guides(color = F, alpha = F) +
    xlab(axe_x_title) + ylab(axe_y_title)


  if(path.smooth){
    pl <- pl + geom_path(aes(x = !!sym(axes_x), y = !!sym(axes_y),
        color = param), linetype = 1, size = 5, alpha = 0.3,
        data = ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
          as.data.frame() %>% .[,axes] %>%
          mutate_all(function(x){x <- x*(r)}) %>%
      mutate(param = rownames(.)) %>%
        separate(param, c("time", "param") ) %>%
        arrange(as.numeric(time)) %>% mutate(
          time = reorder(time, as.numeric(time), mean)), sep = "_") +
      guides(color = guide_legend(title = "Attributes"))
  }

    if(path){
      pl <- pl + geom_path(
        aes(x = !!sym(axes_x), y = !!sym(axes_y),
            color = param), linetype = 1,
        data = ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
          as.data.frame() %>% .[,axes] %>%
          mutate_all(function(x){x <- x*(r)}) %>%
          mutate(param = rownames(.)) %>%
          separate(param, c("time", "param") ) %>%
          arrange(as.numeric(time)) %>% mutate(
            time = reorder(time, as.numeric(time), mean)), sep = "_") +
        guides(color = guide_legend(title = "Attributes"))
    }


pl <- pl + geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(legend.position = "bottom")
  print(pl)
}
  }
