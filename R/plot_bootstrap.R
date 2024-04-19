#' Creates bi-plots similar to those created by the `plot_ASCA()` function, but it includes also the confidence intervals estimated by the bootstrapped values estimated by the `perm_asca()` function.
#' @param ASCA_obj A list of PCA estimated by the TCATASCA function.
#' @param bootstrap_obj A list of estimated values from bootstrap test estimated using the perm_asca() function.
#' @param object A vector of string or numbers indicating which parameter of the ASCA decomposition will be plotted
#' @param dimensions A numeric vector indicating two numbers indicating the axes of the ASCA decomposition.
#' @param point.size A numeric value defining the size of the points of the score values.
#' @param score.points Logical. Adds the points indicating the scores values. Standard is TRUE.
#' @param score.labels Logical. Adds the text labels indicating the score names. Standard is TRUE.
#' @param path Logical. Adds a path indicating the position of the loadings according to their cronological order. Default is TRUE.
#' @param path.smooth Logical. Adds a path smoothed indicating the position of the loadings according to their cronological order. Default is TRUE.
#' @param density Logical. Superimpose a 2-dimensional density plot, indicating th distribution of the temporal resolved loadings according to their density. Default is FLASE.
#' @param time.label Numeric. Adds numeric values of the time units in the plot for loadings values for loadings.time.structure == "long". The number indicates how big has to be the interval between each label.
#' @param loadings.column Logical. Defines whether or not report the barplot depicting the loadings values for the plot reporting loadings in case of ASCA model with short loadings.time.structure == "short".
#' @param loadings.errorbar Logical. Specify wether or not adding the confidence intervals estimated from bootstrap on loading values.
#' @param h_clus Numeric. Indicates whether to calculate or not hierchical clustering for the levels of each factor. The algorithm applied for hierarchical clustering is "Ward-D2", and it is estimated from euclidean distance for all the principal component estimated.
#' @param point.size A numeric value defining the size of the points of the score values.
#' @param max.overlaps.value Numeric, default is 10. Define the maximum number of overlaps allowed for text in the plots.
#' @param print Logical. Indicates whether or not to print the plots.
#' @return A series of plots representing the results of the bootstrap tests.
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_ribbon
#' @export
#' @examples
#'
#' \dontrun{
#'#'ASCA_model <- asca_tcata(CATA~(samp+cons)^2, data = data.long, timecol = "time",
#'         attributes = "attribute")
#'
#' perm_model <- perm_asca(data = data.long, ASCA_object = ASCA_model,
#' nrep = 1000, test = "bootstrap")
#'
#'plot_bootstrap(ASCA_obj = ASCA_model, bootstrap_obj = perm_model)
#'
#' }
#'
#'
#'
#'


plot_bootstrap <- function(
    ASCA_obj,
    bootstrap_obj,
    object = NA, time.label = NULL, print = T,
    score.points = T, score.labels = T, dimensions = c(1,2),
    path = T, density = F, path.smooth = T,
    loadings.column = T,
    loadings.errorbar = T,
    h_clus = NULL, max.overlaps.value = 10, point.size = 2){

  if(is.na(object)){
    object <- 1:(length(names(ASCA_obj)[!(names(ASCA_obj) %in% c(
      "Parameters", "SS_decomposition", "info"))]))
  }

  if(!is.null(time.label)){
    if(!is.numeric(time.label)){
      message("Error: 'time.label' must be numeric.")
    }
  }


  { unicode_minus <- function(x){sub('^-', '\U2212', format(x))} }


  cluster <- NULL
  param <- NULL
  level <- NULL
  time <- NULL
  col_p <- NULL
  . <- NULL
  Component <- NULL
  Score <- NULL
  Loadings <- NULL
  Levels <- NULL
  contrib <- NULL
  name <- NULL
  position <- NULL
  low <- NULL
  high <- NULL
  lowpc1 <- NULL
  highpc1 <- NULL
  lowpc2 <- NULL
  highpc2 <- NULL

  attr <- ASCA_obj$info$attributes
  num_attr <- length(attr)


  resulting_plots <- list()

  for(reference in object){
    row_num <- nrow(ASCA_obj %>% .[[reference]] %>% .$x)
    test_nrow <- row_num == 2

    if(test_nrow){
      message(paste0("The factor ", ASCA_obj %>% names() %>% .[reference],
                     " contains only 2 levels, therefore can't be represented using a biplot.
    It is recommended using the function plot_time_loadings().")
      );
      next
    }
    ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>% as.data.frame() %>%
      dplyr::select(dimensions) %>% `colnames<-`(c("x", "y")) -> ind

    ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>% as.data.frame() %>%
      dplyr::select(dimensions) %>% `colnames<-`(c("x", "y")) -> var

    r <- min((max(ind[,"x"]) - min(ind[,"x"])/(
      max(var[,"x"]) - min(var[,"x"]))),
      (max(ind[,"y"]) - min(ind[,"y"])/(
        max(var[,"y"]) - min(var[,"y"]))))

    axes_x <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>% as.data.frame() %>%
      colnames(.) %>% .[dimensions[1]]

    axes_y <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>% as.data.frame() %>%
      colnames() %>% .[dimensions[2]]


    axe_x_title <- paste0("Dim", dimensions[1]," (", as.character(ASCA_obj %>%
                                                                    .[[reference]] %>% summary() %>% .[] %>% .$importance %>% .[2,dimensions[1]]*100) %>%
                            str_extract("\\d+\\.\\d{1,2}"), "%)")


    axe_y_title <- paste0("Dim",dimensions[2]," (", as.character(ASCA_obj %>%
                                                                   .[[reference]] %>% summary() %>% .[] %>% .$importance %>% .[2,dimensions[2]]*100) %>%
                            str_extract("\\d+\\.\\d{1,2}"), "%)")


    if(is.numeric(reference)){
      final_label <- names(ASCA_obj)[reference]
    }
    if(is.character(reference)){
      final_label <- reference
    }

    if(ASCA_obj[["info"]][["type"]] %in% c("TCATA_ASCA", "TDS_ASCA")){

      if(ASCA_obj %>% .[["info"]] %>% .[["structure"]] == "long"){

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
            as.data.frame() %>% .[,dimensions] %>% mutate(col_p = rownames(.)) %>%
            left_join(cluster_km, by = c("col_p")) %>%
            mutate(cluster = as.factor(cluster)) -> data_plot
        }else{
          data_plot <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>%
            as.data.frame() %>% .[,dimensions] %>% mutate(col_p = rownames(.), cluster = NA)
        }

        data_loadings <- ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
          as.data.frame() %>% .[,dimensions] %>%
          mutate_all(function(x){x <- x*(r)}) %>%
          mutate(param = rownames(.)) %>%
          separate(param, c("time", "param"), sep = "_") %>% arrange(as.numeric(time)) %>%
          mutate(time = reorder(time, as.numeric(time), mean))

        data_loadings <- data_loadings %>%
          mutate(time = as.numeric(time)) %>%
          left_join(bootstrap_obj$Loadings %>% .[[reference]] %>%
            separate(reference, c("time", "param")) %>%
            filter(Component == paste0("PC", dimensions[1])) %>% droplevels() %>%
            mutate(time = as.numeric(time)) %>%
            rename(lowpc1 = low, highpc1 = high),
            by = c("time", "param")) %>%
          left_join(bootstrap_obj$Loadings %>% .[[reference]] %>%
            separate(reference, c("time", "param")) %>%
            filter(Component == paste0("PC", dimensions[2])) %>% droplevels() %>%
            mutate(time = as.numeric(time)) %>%
              rename(lowpc2 = low, highpc2 = high),
          by = c("time", "param")) %>%
          filter(!(lowpc1 < 0 & highpc1 > 0) | !(lowpc2 < 0 & highpc2 > 0)) %>%
          droplevels() %>% mutate(time = as.numeric(time)) %>%
          .[,!(names(.) %in% c("lowpc1", "lowpc2", "highpc1", "highpc2"))]


        pl <- ggplot()
        if(density){
          pl <- pl + stat_density2d(aes(x = !!sym(axes_x), y = !!sym(axes_y),
                                        fill = param, alpha = after_stat(level/max(level))),
                                    data = data_loadings,
                                    geom = "polygon") +
            theme_minimal() + scale_alpha_continuous(range = c(0.25,0.7)) +
            guides(fill = guide_legend(title = "Attributes")) +
            theme(plot.title = element_text(hjust = 0.5, face = "italic"),
                  legend.position = "bottom", legend.title = element_blank())
        }


        pl <- pl + {
          if(is.numeric(h_clus) & score.points){
            geom_point(aes(x = !!sym(axes_x), y = !!sym(axes_y), color = cluster),
                       data = data_plot, size = point.size)
          }else if(score.points){
            geom_point(aes(x = !!sym(axes_x), y = !!sym(axes_y)),
                       color = "black", data = data_plot, size = point.size)
          }
        } + scale_shape_manual(values = rep(19,15)) +
          scale_x_continuous(labels = unicode_minus) +
          scale_y_continuous(labels = unicode_minus) + theme_minimal() +
          {if(is.numeric(h_clus) & score.labels){
            geom_text_repel(aes(x = !!sym(axes_x), y = !!sym(axes_y),
                                label = col_p, color = cluster),
                            max.overlaps = getOption("ggrepel.max.overlaps",
                                                     default = max.overlaps.value),
                            data = data_plot)
          }else if(score.labels){
            geom_text_repel(aes(x = !!sym(axes_x), y = !!sym(axes_y),
                                label = col_p),
                            max.overlaps = getOption("ggrepel.max.overlaps",
                                                     default = max.overlaps.value),
                            data = data_plot) }} +
          theme(legend.position = "none") +
          guides(color = "none", alpha = "none") +
          xlab(axe_x_title) + ylab(axe_y_title)


        if(path.smooth){
          pl <- pl + geom_path(
            aes(x = !!sym(axes_x), y = !!sym(axes_y), color = param),
            linetype = 1, linewidth = 5, alpha = 0.3,
            data = data_loadings) +
            guides(color = guide_legend(title = "Legend"))
        }

        if(is.numeric(time.label)){
          time_series <- ASCA_obj[["info"]]$timecol %>% {
            if(is_tibble(.)){
              pull(.)
            }else{
              .
            }
          } %>% .[seq(1, length(.), (time.label))]
          pl <- pl + geom_text(aes(x = !!sym(axes_x), y = !!sym(axes_y),
                                   color = param, label = time),
                               data = filter(data_loadings, as.numeric(time) %in% time_series))

        }

        if(path){
          pl <- pl + geom_path(aes(x = !!sym(axes_x), y = !!sym(axes_y),
                                   color = param), linetype = 1, arrow = arrow(type = "closed",
                                                                               length = unit(0.25, "cm")),
                               data = data_loadings) +
            guides(color = guide_legend(title = "Attributes"))
        }

      }

      if(ASCA_obj[["info"]][["structure"]] == "short"){
        if(is.numeric(h_clus)){
          message("Clustering is not estimated for loadings.time.structure = 'short'")
        }

  if(names(ASCA_obj)[reference] != "Residuals" & reference != "Residuals"){
    facet_names <- c(axe_x_title, axe_y_title)
    names(facet_names) <- ASCA_obj[[reference]]$x %>% .[] %>%
      as.data.frame() %>% .[,dimensions] %>% colnames(.)
    data_plot <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>%
      as.data.frame() %>% .[,dimensions] %>% mutate(col_p = rownames(.)) %>%
      separate(col_p, c("time", "levels"), sep = "_") %>%
      gather(Component, Score, 1:2) %>%
      left_join(
        bootstrap_obj$Scores %>% .[[reference]] %>%
          separate(reference, c("time", "levels"), sep = "_"),
        by = c("time", "levels", "Component") ) %>%
      mutate(time = as.numeric(time))



          pl <- ggplot()
          if(score.points){
          pl <- pl + geom_line(aes(x = time, y = Score, color = levels), data_plot) +
            geom_ribbon(aes(x = time, y = Score, ymin = low, ymax = high,
              fill = levels), alpha = 0.08, data = data_plot) +
            facet_grid(rows = "Component", scales = "free_y",
                       labeller = labeller(Component = facet_names)) +
            geom_hline(yintercept = 0, linetype = 2) +
            ggtitle("", subtitle = paste0("Factor: ", final_label)) +
            theme_minimal() + theme(legend.position = "bottom",
              plot.title = element_blank(), legend.title = element_blank(),
              axis.text = element_text(color = "black"),
              legend.key.size = unit(0.3, 'cm') ) +
            guides(color = guide_legend(ncol = 12, byrow = TRUE,
              override.aes = list(linewidth = 4)))
          }


          if(loadings.column){

          data_loadings <- ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
            as.data.frame() %>% .[,dimensions] %>% mutate(Levels = rownames(.)) %>%
            gather(Component, Loadings, 1:2) %>%
            left_join(
              bootstrap_obj[["Loadings"]] %>% .[[reference]] %>%
                  rename(Levels = reference),
              by = c("Levels", "Component") )

          pl2 <- ggplot() + geom_col(aes(x = Levels, y = Loadings),
                   color = "black", fill = "white", data = data_loadings) +
            facet_grid(rows = "Component", scales = "free_y",
                       labeller = labeller(Component = facet_names)) +
            theme_minimal() +
            theme(axis.title.x = element_blank(),
                  axis.text = element_text(color = "black"),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

          if(loadings.errorbar){
            pl2 <- pl2 + geom_errorbar(aes(
              x = Levels, y = Loadings, ymin = low, ymax = high ),
              linetype = 2)
          }


          if(score.points){
          pl <- ggarrange(pl, pl2, nrow = 1)
          }else{
            pl <- pl2
          }

          }



        }

        if(names(ASCA_obj)[reference] == "Residuals"|reference == "Residuals"){

          data_plot <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>%
            as.data.frame() %>% .[,dimensions] %>% mutate(levels = rownames(.))
          pl <- ggplot()
          pl <- pl + {
            if(score.points){
              geom_point(aes(x = !!sym(axes_x), y = !!sym(axes_y)), data_plot,
                         alpha = 0.2)}
          } +
            geom_hline(yintercept = 0, linetype = 2) +
            geom_vline(xintercept = 0, linetype = 2) +
            xlab(axe_x_title) + ylab(axe_y_title) +
            ggtitle("", subtitle = paste0("Factor: ", final_label)) +

            theme_minimal() + theme(legend.position = "bottom",
                                    plot.title = element_blank()) +
            guides(color = guide_legend(ncol = 10, byrow = TRUE))
          data_loadings <- ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
            as.data.frame() %>% .[,dimensions] %>%
            mutate_all(function(x){x <- x*(r)}) %>%
            mutate(Levels = rownames(.))


          pl <- pl + geom_text(
            aes(x = !!sym(axes_x), y = !!sym(axes_y), label = Levels, color = Levels),
            data = data_loadings) +
            theme(legend.position = "none")


        }

      }




      if(ASCA_obj %>% .[["info"]] %>% .[["structure"]] == "long"){
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
          theme(legend.position = "bottom",
                axis.text = element_text(color = "black"))
      }
    }

    if(ASCA_obj[["info"]][["type"]] %in% c("TI_ASCA")){

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
          as.data.frame() %>% .[,dimensions] %>% mutate(col_p = rownames(.)) %>%
          left_join(cluster_km, by = c("col_p")) %>%
          mutate(cluster = as.factor(cluster)) -> data_plot
      }else{
        data_plot <- ASCA_obj %>% .[[reference]] %>% .$x %>% .[] %>%
          as.data.frame() %>% .[,dimensions] %>% mutate(col_p = rownames(.), cluster = NA)
      }

      data_loadings <- ASCA_obj %>% .[[reference]] %>% .$rotation %>% .[] %>%
        as.data.frame() %>% .[,dimensions] %>%
        mutate_all(function(x){x <- x*(r)}) %>%
        mutate(time = rownames(.)) %>%
        arrange(as.numeric(time)) %>%
        mutate(time = reorder(time, as.numeric(time), mean))



      pl <- ggplot()

      if(density){
        pl <- pl + stat_density2d(aes(x = !!sym(axes_x), y = !!sym(axes_y),
                                      fill = param, alpha = after_stat(level/max(level))),
                                  data = data_loadings,
                                  geom = "polygon") +
          theme_minimal() + scale_alpha_continuous(range = c(0.25,0.7)) +
          guides(fill = guide_legend(title = "Attributes")) +
          theme(plot.title = element_text(hjust = 0.5, face = "italic"),
                legend.position = "bottom", legend.title = element_blank())
      }


      if(is.numeric(time.label)){
        time_series <- ASCA_obj[["info"]]$timecol %>% {
          if(is_tibble(.)){
            pull(.)
          }else{
            .
          }
        } %>% .[seq(1, length(.),  time.label)]

        pl <- pl + geom_text(aes(x = !!sym(axes_x), y = !!sym(axes_y),
                                 label = time),
                             data = filter(data_loadings, as.numeric(time) %in% time_series))

      }


      pl <- pl + {
        if(is.numeric(h_clus) & score.points){
          geom_point(aes(x = !!sym(axes_x), y = !!sym(axes_y), color = cluster),
                     data = data_plot, size = point.size)
        }else if(score.points){
          geom_point(aes(x = !!sym(axes_x), y = !!sym(axes_y)),
                     color = "black", data = data_plot, size = point.size)
        }
      } + scale_shape_manual(values = rep(19,15)) +
        scale_x_continuous(labels = unicode_minus) +
        scale_y_continuous(labels = unicode_minus) + theme_minimal() +
        {if(is.numeric(h_clus) & score.labels){
          geom_text_repel(aes(x = !!sym(axes_x), y = !!sym(axes_y),
                              label = col_p, color = cluster),
                          max.overlaps = getOption("ggrepel.max.overlaps",
                                                   default = max.overlaps.value),
                          data = data_plot)
        }else if(score.labels){
          geom_text_repel(aes(x = !!sym(axes_x), y = !!sym(axes_y),
                              label = col_p),
                          max.overlaps = getOption("ggrepel.max.overlaps",
                                                   default = max.overlaps.value),
                          data = data_plot) }} +
        theme(legend.position = "none") +
        guides(color = "none", alpha = "none") +
        xlab(axe_x_title) + ylab(axe_y_title)


      if(path.smooth){
        pl <- pl + geom_path(aes(x = !!sym(axes_x), y = !!sym(axes_y)),
                             linetype = 1, linewidth = 5, alpha = 0.3,
                             data = data_loadings) +
          guides(color = guide_legend(title = "Legend"))
      }

      if(path){
        pl <- pl + geom_path(aes(x = !!sym(axes_x), y = !!sym(axes_y)),
                             linetype = 1,
                             arrow = arrow(type = "closed", length = unit(0.25, "cm")),
                             data = data_loadings) +
          guides(color = guide_legend(title = "Time"))
      }

      pl <- pl + geom_vline(xintercept = 0, linetype = 2) +
        geom_hline(yintercept = 0, linetype = 2) +
        ggtitle("", subtitle = paste0("Factor: ", final_label)) +
        theme(legend.position = "bottom",
              axis.text = element_text(color = "black"))

    }

    resulting_plots[[names(ASCA_obj)[reference]]] <- pl


    if(print){print(pl)}


  }
  invisible(resulting_plots)

}







