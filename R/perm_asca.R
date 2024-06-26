#' Compute permutation test to validate the results from ANOVA Simultaneous Component Analysis (ASCA) of Temporal Check All That Apply (TCATA) data.
#' @param data A data frame, or object coercible by as.data.frame to a data frame, containing the variables in the model. It must be in long format and must contain a column for Temporal Check All That Apply binary data, a column reporting the time values, and a column that defines the attributes analyzed.
#' @param ASCA_object An object obtained from the functions asca_ti(), asca_tds(), and asca_tcata() from the package ASCATCATA. The model selected must be th one estimated from the data value.
#' @param test A vector of strings defining which kind of iteration test will be performed by the function between permutation test (to estimate the statistical significance of the model) and bootstrap test (to define the confidence intervals of the parameters estimated). The application of permutation depends on the presence of the string "permutation" in the vector, and the application of bootstrap dpends on the presence of the string "bootstrap".
#' @param nrep Number of iteration for bootstrap and permutation test, default is 1000.
#' @param plot Logical. If TRUE, prints a plot after estimating the permutation test.
#' @param dimensions A numeric vector indicating the dimensions included in the final results of the bootstrap procedure. It can be larger than two.
#' @param confidence Numeric, values from 0 to 1. Indicates the probability applied for the estimation of the confidence interval via bootstrapping.
#' @param ... Optional parameters
#' @return A list of objects containing the results of ASCA decomposition of a structured dataset.
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @importFrom tibble is_tibble
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom car Anova
#' @importFrom stats quantile
#' @importFrom ggplot2 geom_histogram
#' @importFrom EFA.dimensions PROCRUSTES
#' @importFrom matrixcalc frobenius.norm
#' @export
#' @examples
#' \dontrun{
#'
#'data.long <- tempR::ojtcata  %>%
#' gather(time, CATA, 5:25) %>%
#'  mutate(cons = as.factor(cons), samp = as.factor(samp),
#'         time = as.numeric(str_extract(time, "\\d+")))
#'
#'ASCA_model <- asca_tcata(CATA~(samp+cons)^2, data = data.long, timecol = "time",
#'         attributes = "attribute")
#' perm_model <- perm_asca(data = data.long, ASCA_object = ASCA_model,
#' nrep = 1000)
#' }
#'


perm_asca <- function(data, ASCA_object, nrep = 1000,
                      plot = F,
                      dimensions = c(1,2),
                      confidence = 0.95,
                      test = c("permutation", "bootstrap"),
                      ...){

  prev_contr <- options()$contrasts
  options(contrasts =  rep("contr.sum", 2),
          dplyr.show_progress = F)

  . <- NULL
  colnames1 <- NULL
F_norm <- NULL
Component <- NULL
value <- NULL
reference <- NULL
plot_perm <- NULL
real_norm <- NULL
p_values <- NULL
ref_names <- NULL

  if(!is.numeric(nrep)){
    message("'nrep' must be numeric.")
    #return(NULL)
    stop()
  }

if(!(ASCA_object[["info"]][["type"]] %in% c("TI_ASCA", "TDS_ASCA", "TCATA_ASCA"))){
  message(
    "'type' is not defined properly. You can only use 'TI', 'TDS', and 'TCATA'")
  #return(NULL)
  stop()
}

  formula <- ASCA_object[["info"]][["formula"]]
  timecol <- as.symbol(ASCA_object[["info"]][["labels"]]$timecol)
  attributes <- as.symbol(ASCA_object[["info"]][["labels"]]$attributes)

  ref <- formula[[2L]]
  fact <- as.character(formula[[3L]]) %>% as.vector() %>%
    str_split(., "[+*:]", simplify = T) %>% str_trim() %>%
    .[str_detect(., ".+")] %>%
    .[!str_detect(.,"^\\d")] %>% str_remove(., "^\\(") %>%
    str_remove(., "\\)$")



  timecol <- ASCA_object[["info"]][["labels"]]$timecol

  if(ASCA_object[["info"]][["type"]] %in% c("TCATA_ASCA", "TDS_ASCA")){
  data1 <- data %>% group_by_at(vars(as.name(timecol), as.name(attributes))) %>%
    do(filter(., length(pull(unique(.[, ref]))) != 1) %>% droplevels()) %>%
    ungroup() %>% droplevels() %>% mutate_at(vars(fact), as.factor)
  }


  if(ASCA_object[["info"]][["type"]] %in% c("TI_ASCA")){

  }


  colnames1 <- names(data1)

  if("permutation" %in% test){

    pb_perm <- txtProgressBar(min = 0, max = nrep, style = 3,
        width = 75, char = "=")

    data_temp <- data.frame()
    data_perm <- data.frame()

    for(j in 1:nrep){


      data1 %>% mutate_at(vars(fact), as.factor) %>%
        split(dplyr::select(., timecol)) %>%
        map(~droplevels(.) %>% split(., dplyr::select(., attributes)) %>%
              map(~droplevels(.) %>%
                    mutate_at(vars(fact), function(x){sample(x)} ) %>%
                    dplyr::select(timecol, attributes, fact, ref) %>%
                    data.frame(effect = predict(
                        glm(as.formula(formula),
                            data = mutate_at(., vars(ref), scale),
                            family = gaussian()), type = "terms"))) %>%
              plyr::ldply(., data.frame)) %>%
        plyr::ldply(., data.frame) %>% mutate(n = j) -> data2


      data2 <- data2[,-1]
      colnames2 <- data2 %>% names(.)
      #print(colnames2)

      ref_names <- c()
      for(i in colnames2[!(colnames2 %in% c(colnames1, "n"))]){
        anchor <- i
        name <- anchor %>% str_remove(., "^effect.") %>% str_split_1(., "\\.")

        name2 <- paste(name, collapse = ":") %>% str_remove(., "[_\\.]1$")

        ref_names <- c(ref_names, name2)
        refk <- c("refk")
#print(i)
        data2 %>% dplyr::select(1:2, i) %>% cbind(
          data2 %>% .[,3:length(colnames1)] %>%
            .[, names(.) %in% name] %>% as.data.frame(.) %>%
            ifelse(!is.null(ncol(.)), unite(., col = name2, sep = "_"), .)) %>%
          `colnames<-`(c(names(dplyr::select(data2, c(1,2,i))), name2)) -> temp;
       # print(temp)
        if(ASCA_object[["info"]][["structure"]] == "long"){
          temp[refk] <- paste0(temp[,as.character(timecol)], "_",
                               temp[,as.character(attributes)])

          data_temp <- data.frame(
            F_norm = temp %>% dplyr::select(-timecol, -attributes) %>%
            group_by_at(vars(as.name(refk), as.name(name2))) %>% slice(1) %>%
            ungroup() %>%
            pivot_wider(names_from = refk, values_from = as.symbol(anchor)) %>%
            column_to_rownames(name2) %>%
            mutate_all(., function(x){x <- ifelse(is.na(x), mean(x, na.rm = T), x)}) %>%
            mutate_all(., function(x){x <- x - mean(x, na.rm = T)}) %>%
            prcomp() %>% .$x %>% matrixcalc::frobenius.norm(.),
            factor = name2, rep = j)
        }

data_perm <- rbind(data_perm, data_temp)

      }
      setTxtProgressBar(pb_perm, j)
    }

    close(pb_perm)




    real_norm <- ASCA_object %>% .[ref_names] %>%
      lapply(., function(x){matrixcalc::frobenius.norm(x$x)}) %>%
      as.data.frame() %>% gather(factor, norm) %>%
      mutate(factor = ifelse( factor %in% ref_names, factor, ref_names))



  if(plot){
plot_perm <- data_perm %>% ggplot() +
  geom_histogram(aes(x = F_norm), bins = 50) +
  geom_vline(aes(xintercept = norm), linetype = 2, size = 1, color = "red",
    data = real_norm) +
  facet_wrap(~ factor, scales = "free") +
  theme_minimal()
 print(plot_perm)
    }

    #message(paste0("The estimated p.value considering the Frobenius norm is: "))

    p_values <- real_norm %>% group_by(factor) %>%
      do(mutate(., p_value = mean(unique(norm) < (filter(
        data_perm, factor == unique(.$factor)) %>%
          droplevels() %>% .$F_norm)))) %>% slice(1) %>% ungroup() %>%
      dplyr::select(-norm)


apply(p_values, 1, function(x){
  print(paste0("The p.value estimated for the factor ", x[1], " is: ", x[2]))
  })

  return(p_values)


  }
    #View(data_perm)
#print(fact)

  if("bootstrap" %in% test){

    data_temp <- list()
data_boot <- list()
data_boot_2 <- list()
data_score <- list()
  data_score_2 <- list()


  pb_boot <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                            max = nrep, # Maximum value of the progress bar
                            style = 3,    # Progress bar style (also available style = 1 and style = 2)
                            width = 75,   # Progress bar width. Defaults to getOption("width")
                            char = "=")

    for(k in 1:nrep){
      data1 %>%
        mutate_at(vars(fact), as.factor) %>%
        split(dplyr::select(., timecol)) %>%
        map(~droplevels(.) %>% split(., dplyr::select(., attributes)) %>%
          map(~droplevels(.) %>%
                .[sample(1:nrow(.), replace = T),] %>%
                # modelr::bootstrap(1) %>% plyr::ldply(data.frame) %>%
                # .[,-c(1, ncol(.))] %>%
                # filter(!is.na(!!sym(ref))) %>% droplevels() %>%
                #colnames() %>% print()
                #View()
                 mutate_at(vars(ref), function(x){
                   if(sum(x, na.rm = T) < 5){
                     x <- replace(x, sample(1:length(x),5),1)
                     }else{x} }) %>%
                mutate_at(., vars(ref), scale) %>%
                #  .[, as.character(ref)] %>%
                #.$CATA %>% summary() %>% print()
                  # glm(
                  #   formula = as.formula(as.character(formula)),
                  #   data = mutate_at(., vars(ref), scale),
                  #   family = gaussian()) %>% summary() %>% print()
                data.frame( effect = predict(glm(
                    formula = as.formula(formula), data = .,
                    family = gaussian()), type = "terms"))
              )) -> data2


      colnames2 <- data2[[1]][[1]] %>% names(.)
      #print(colnames2)
      data2 <- data2 %>% map(., ~plyr::ldply(., function(x){as.data.frame(x) %>%
          `colnames<-`(c(colnames2))})) %>% plyr::ldply(., function(x){
            data.frame(x) %>% `colnames<-`(c(as.character(attributes),
                  colnames2))}) %>% dplyr::select(-timecol)

      #str(data2)
      names(data2)[1] <- as.character(timecol)

      for(i in (length(colnames1)+1):ncol(data2)){
        anchor <- names(data2)[i]
        name <- anchor %>% str_remove(., "^effect.") %>% str_split_1(., "\\.")
        name2 <- paste(name, collapse = ":") %>% str_remove(., "_1$")
        refk <- c("refk")

        data2 %>% .[,c(1:2, i)] %>% cbind(
          data2 %>% .[,3:length(colnames1)] %>%
            .[, names(.) %in% name] %>% as.data.frame(.) %>%
            ifelse(!is.null(ncol(.)), unite(., col = name2, sep = ":"), .)) %>%
          `colnames<-`(c(names(data2)[c(1,2,i)], name2)) -> temp;

    if(ASCA_object[["info"]][["structure"]] == "long"){
          temp[refk] <- paste0(temp[,as.character(timecol)], "_",
                               temp[,as.character(attributes)])

          data_temp[[name2]] <- temp %>% dplyr::select(-timecol, -attributes) %>%
            group_by_at(vars(as.name(refk), as.name(name2))) %>% slice(1) %>%
            ungroup() %>%
             pivot_wider(names_from = refk, values_from = as.symbol(anchor)) %>%
             column_to_rownames(name2) %>%
             mutate_all(., function(x){x <- ifelse(is.na(x), mean(x, na.rm = T), x)}) %>%
             mutate_all(., function(x){x <- x - mean(x, na.rm = T)}) %>%
             prcomp()


         data_boot[[name2]] <- rbind(data_boot[[name2]],
            (data_temp[[name2]] %>% .$rotation %>%
            EFA.dimensions::PROCRUSTES(
              ., ASCA_object[[name2]][["rotation"]],
              type = "orthogonal", verbose = F) %>% .$loadingsPROC) %>%
              as.data.frame() %>% .[,dimensions] %>%
              mutate(reference = rownames(.)) )



         data_score[[name2]] <- rbind(data_score[[name2]],
              (data_temp[[name2]] %>% .$x %>%
                 EFA.dimensions::PROCRUSTES(., ASCA_object[[name2]][["x"]],
                            type = "orthogonal", verbose = F) %>%
                 .$loadingsPROC) %>%
                as.data.frame() %>% .[,dimensions] %>%
                mutate(reference = rownames(.))  )
         }


        if(ASCA_object[["info"]][["structure"]] == "short"){

          temp[refk] <- paste0(temp[,as.character(timecol)], "_",
                               temp[,as.character(name2)])

          data_temp[[name2]] <- temp %>% dplyr::select(-timecol) %>%
            dplyr::select(-c(name2)) %>%
            group_by_at(vars(as.name(refk), as.name(attributes))) %>%
            slice(1) %>% ungroup() %>%
            pivot_wider(names_from = attributes, values_from = as.symbol(anchor)) %>%
            column_to_rownames(refk) %>%
            mutate_all(., function(x){x <- ifelse(is.na(x), mean(x, na.rm = T), x)}) %>%
            mutate_all(., function(x){x <- x - mean(x, na.rm = T)}) %>% prcomp()

          data_boot[[name2]] <- rbind(data_boot[[name2]],
            (data_temp[[name2]] %>% .$rotation %>%
               EFA.dimensions::PROCRUSTES(.,
                 ASCA_object[[name2]][["rotation"]],
                 type = "orthogonal", verbose = F) %>% .$loadingsPROC) %>%
              as.data.frame() %>% .[,dimensions] %>%
              mutate(reference = rownames(.)) )


          data_score[[name2]] <- rbind(data_score[[name2]],
            (data_temp[[name2]] %>% .$x %>%
               EFA.dimensions::PROCRUSTES(., ASCA_object[[name2]][["x"]],
                    type = "orthogonal", verbose = F) %>% .$loadingsPROC) %>%
              as.data.frame() %>% .[,dimensions] %>%
              mutate(reference = rownames(.)) )
        }
      }
      setTxtProgressBar(pb_boot, k)

  }

  close(pb_boot)

  #print(data_score)
for(j in names(data_boot)){
  print(j)
  print( data_boot[[j]] )

  data_boot_2[[j]] <- rbind(
    data_boot_2[[j]],
    data_boot[[j]] %>%
      as.data.frame() %>%
      mutate(reference = str_remove(reference, "\\.\\d+$")) %>%
      gather(Component, value, -reference) %>%
      group_by(reference, Component) %>%
      summarize(
        low = as.numeric(quantile(
          value, probs = 0 + ((1-confidence)/2), na.rm = T)),
        high = as.numeric(quantile(
          value, probs = 1 - ((1-confidence)/2), na.rm = T))) %>%
      ungroup() )#s %>% as.data.frame()



  data_score_2[[j]] <- rbind(
    data_score_2[[j]],
    data_score[[j]] %>% as.data.frame() %>%
      mutate(reference = str_remove(reference, "\\.\\d+$")) %>%
       gather(Component, value, -reference) %>%
       group_by(reference, Component) %>%
       summarize(
         low = as.numeric(quantile(value, probs = 0.025, na.rm = T)),
         high = as.numeric(quantile(value, probs = 0.975, na.rm = T))) %>%
       ungroup()  )


}
results <- list(Loadings = data_boot_2, Scores = data_score_2)

return(results)
  options(contrasts =  prev_contr)

}

}

