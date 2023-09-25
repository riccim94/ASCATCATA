#' Compute permutation test to validate the results from ANOVA Simultaneous Component Analysis (ASCA) of Temporal Check All That Apply (TCATA) data.
#' @param data A data frame, or object coercible by as.data.frame to a data frame, containing the variables in the model. It must be in long format and must contain a column for Temporal Check All That Apply binary data, a column reporting the time values, and a column that defines the attributes analyzed.
#' @param ASCA_object An object obtained from the functions asca_ti(), asca_tds(), and asca_tcata() from the package ASCATCATA. The model selected must be th one estimated from the data value.
#' @param test A vector of strings defyning which kind of iteration test will be performed by the function between permutation test (to estimate the statistical significance of the model) and bootstrap test (to define the confidence intervals of the parameters estimated). The application of permutation depends on the presence of the string "permutation" in the vector, and the application of bootstrap dpends on the presence of the string "bootstrap".
#' @param nrep Number of iteration for bootstrap and permutation test, default is 1000.
#' @param ... Optional parameters
#' @return A list of objects containing the results of ASCA decomposition of a structured dataset.
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @importFrom tibble is_tibble
#' @importFrom car Anova
#' @export
#' @examples
#' \dontrun{
#' perm_asca(CATA~(samp+cons)^2, data = tempR::ojtcata, type = "tcata", timecol = "time", attributes = "attribute", nrep = 1000)
#' describe(dataset, col1, col2)
#' }
#'


perm_asca <- function(data, ASCA_object, nrep = 1000,
                      test = c("permutation", "bootstrap"),
                      ...){

  prev_contr <- options()$contrasts
  options(contrasts =  rep("contr.sum", 2))

  . <- NULL
  colnames1 <- NULL
  loading.time.structure <- NULL


  if(!is.numeric(nrep)){
    message("'nrep' must be numeric.")
    return(NULL)
  }

if(!(ASCA_object[["info"]][["type"]] %in% c("TI_ASCA", "TDS_ASCA", "TCATA_ASCA"))){
  message(
    "'type' is not defined properly. You can only use 'TI', 'TDS', and 'TCATA'")
  return(NULL)
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

  data1 <- data %>% group_by_at(vars(as.name(timecol), as.name(attributes))) %>%
    do(filter(., length(pull(unique(.[, ref]))) != 1) %>% droplevels()) %>%
    ungroup() %>% droplevels() %>% mutate_at(vars(fact), as.factor)

  #colnames1 <- names(data1)

  if("permutation" %in% test){
data_perm <- data.frame()
    for(i in 1:nrep){


      data1 %>%
        mutate_at(vars(fact), as.factor) %>%
        split(dplyr::select(., timecol)) %>%
        map(~droplevels(.) %>% split(., dplyr::select(., attributes)) %>%
              map(~droplevels(.) %>%
                    mutate_at(vars(fact), function(x){sample(x)} ) %>%
                    dplyr::select(timecol, attributes, fact, ref) %>%
                    data.frame(F_val = car::Anova(
                glm(as.formula(formula),
            data = mutate_at(., vars(ref), scale), family = gaussian()),
            test.statistic = "F") %>% .$`F value`,
              factor = rownames(as.data.frame(car::Anova(
                glm(as.formula(formula),
                    data = mutate_at(., vars(ref), scale), family = gaussian()),
                test.statistic = "F")))) %>%
               filter(factor != "Residuals") %>% droplevels() %>%
              group_by(factor) %>% slice(1) %>% ungroup() ) %>%
              plyr::ldply(., data.frame)
            ) %>%
        plyr::ldply(., data.frame) %>% mutate(n = i) -> data2

data_perm <- rbind(data_perm, data2)

    }
#View(data_perm)

  }

  if("bootstrap" %in% test){

data_boot <- data.frame()

    for(i in 1:nrep){
      data1 %>%
        mutate_at(vars(fact), as.factor) %>%
        split(dplyr::select(., timecol)) %>%
        map(~droplevels(.) %>% split(., dplyr::select(., attributes)) %>%
              map(~droplevels(.) %>% split(., dplyr::select(., attributes)) %>%
                    map(~droplevels(.) %>%
                          data.frame(
                            effect = predict(glm(as.formula(formula),
                              data = mutate_at(., vars(ref), scale),
                              family = gaussian()),
                              type = "terms"))))) -> data2

      colnames2 <- data2[[1]][[1]] %>% names(.)

      data2 <- data2 %>% map(., ~plyr::ldply(., function(x){as.data.frame(x) %>%
          `colnames<-`(c(colnames2))})) %>% plyr::ldply(., function(x){
            data.frame(x) %>% `colnames<-`(c(as.character(attributes),
                                             colnames2))}) %>% dplyr::select(-timecol)

      names(data2)[1] <- as.character(timecol)

      for(i in (length(colnames1)+1):ncol(data2)){
        anchor <- names(data2)[i]
        name <- anchor %>% str_remove(., "^effect.") %>% str_split_1(., "\\.")
        name2 <- paste(name, collapse = "_") %>% str_remove(., "_1$")
        refk <- c("refk")

        data2 %>% .[,c(1:2, i)] %>% cbind(data2 %>% .[,3:length(colnames1)] %>%
                                            .[, names(.) %in% name] %>% as.data.frame(.) %>% ifelse(!is.null(ncol(.)),
                                                                                                    unite(., col = name2, sep = "_"), .)) %>%
          `colnames<-`(c(names(data2)[c(1,2,i)], name2)) -> temp;
        if(loadings.time.structure == "long"){
          temp[refk] <- paste0(temp[,as.character(timecol)], "_",
                               temp[,as.character(attributes)])

          data3[[name2]] <- temp %>% dplyr::select(-timecol, -attributes) %>%
            group_by_at(vars(as.name(refk), as.name(name2))) %>% slice(1) %>%
            ungroup() %>%
            pivot_wider(names_from = refk, values_from = as.symbol(anchor)) %>%
            column_to_rownames(name2) %>%
            mutate_all(., function(x){x <- ifelse(is.na(x), mean(x, na.rm = T), x)}) %>%
            mutate_all(., function(x){x <- x - mean(x, na.rm = T)}) %>% prcomp()
        }

View(data2)

    }

  }




  options(contrasts =  prev_contr)

}

}

