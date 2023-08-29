#' Compute ANOVA Simultaneous Component Analysis (ASCA) of Time Intensity (TI) data.
#' @param data A data frame, or object coercible by as.data.frame to a data frame, containing the variables in the model. It must be in long format and must contain a column for Temporal Dominant Sensation binary data, a column reporting the time values, and a column that defines the attributes analyzed.
#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The formula will be applied for each atttribut at each time interval defined by the timecol column
#' @param timecol A string containing the name of the column indicating the time intervals of the TI dataset.
#' @param time.quantization A single number reporting the number of time units contained in the new intervals used to quantize the time column.
#' @param ... Optional parameters
#' @return A list of objects containing the results of ASCA decomposition of a structured dataset.
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @importFrom stats hclust
#' @importFrom stats cutree
#' @importFrom stats dist
#' @importFrom car Anova
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stringr str_split
#' @importFrom stringr str_split_1
#' @importFrom stringr str_remove
#' @importFrom stringr str_trim
#' @importFrom stringr str_detect
#' @importFrom plyr ldply
#' @importFrom stats prcomp
#' @importFrom stats residuals
#' @importFrom tibble column_to_rownames
#' @importFrom tibble is_tibble
#' @export
#' @examples
#' \dontrun{
#' asca_ti(CATA~(sample+assessor)^2, data = read_excel("time-intensity-data_0.xlsm") %>%
#' gather(time, intensity, 6:36),timecol = "time")
#' }


asca_ti <- function(formula, data, timecol,
                     time.quantization = NULL, ...){
  prev_contr <- options()$contrasts
  options(contrasts =  rep("contr.sum", 2))
  colref <- NULL
  . <-NULL
  ref <- NULL
  fact <- NULL
  formula_ <- formula

  data3 <- list()

  data1 <- data %>% group_by_at(vars(as.name(timecol))) %>%
    do(filter(., length(pull(unique(.[, ref]))) != 1) %>% droplevels()) %>%
    ungroup() %>% droplevels()

  colnames1 <- names(data1)
  #View(data1)
  data2 <- data1 %>%
    mutate_at(vars(fact), as.factor) %>% split(dplyr::select(., timecol )) %>%
      map(~droplevels(.) %>% data.frame(
        effect = predict(glm(as.formula(formula),
          data = mutate_at(., vars(ref), scale),
          family = gaussian()), type = "terms")))
  #View(data2)


  colnames2 <- data2[[1]][[1]] %>% names(.)


  data2 <- data2 %>% plyr::ldply(., function(x){
    data.frame(x) %>% `colnames<-`(c(colnames2))}) %>% dplyr::select(-timecol)

  names(data2)[1] <- as.character(timecol)

  #View(data2)
  for(i in (length(colnames1)+1):ncol(data2)){
    anchor <- names(data2)[i]
    name <- anchor %>% str_remove(., "^effect.") %>% str_split_1(., "\\.")
    name2 <- paste(name, collapse = "_") %>% str_remove(., "_1$")
    refk <- c("refk")

    data2 %>% .[,c(1:2, i)] %>% cbind(data2 %>% .[,3:length(colnames1)] %>%
                                        .[, names(.) %in% name] %>% as.data.frame(.) %>% ifelse(!is.null(ncol(.)),
                                                                                                unite(., col = name2, sep = "_"), .)) %>%
      `colnames<-`(c(names(data2)[c(1,2,i)], name2)) -> temp;
    temp[refk] <- temp[,as.character(timecol)]

    data3[[name2]] <- temp %>% dplyr::select(-timecol) %>%
      group_by_at(vars(as.name(refk), as.name(name2))) %>% slice(1) %>%
      ungroup() %>%
      pivot_wider(names_from = refk, values_from = as.symbol(anchor)) %>%
      column_to_rownames(name2) %>%
      mutate_all(., function(x){x <- ifelse(is.na(x), mean(x, na.rm = T), x)}) %>%
      mutate_all(., function(x){x <- x - mean(x, na.rm = T)}) %>%
      prcomp()

  }

  data3[["info"]][["type"]] <- "TI_ASCA"
  data3[["info"]][["timecol"]] <- unique(data[,as.character(timecol)])


  if(is_tibble(data3[["info"]][["attributes"]])){
    data3[["info"]][["attributes"]] <- pull(data3[["info"]][["attributes"]])
  }

  options(contrasts =  prev_contr)
  return(data3)

}
