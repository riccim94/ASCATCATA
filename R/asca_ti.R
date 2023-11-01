#' Compute ANOVA Simultaneous Component Analysis (ASCA) of Time Intensity (TI) data.
#' @param data A data frame, or object coercible by as.data.frame to a data frame, containing the variables in the model. It must be in long format and must contain a column for Temporal Dominant Sensation binary data, a column reporting the time values, and a column that defines the attributes analyzed.
#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The formula will be applied for each atttribute at each time interval defined by the timecol column
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
#'
#' #first of all a dataset of Time Intensity data is used.
#' # An open Time Intensity dataset is downloaded from the website
#' # https://help.xlstat.com/dataset/time-intensity-data_0.xlsm
#'library(readxl)
#'library(tidyr)
#'library(stringr)
#'library(httr)
#'url1 <- "https://help.xlstat.com/dataset/time-intensity-data_0.xlsm"

#'GET(url1, write_disk(tf <- tempfile(fileext = ".xlsm")))
#'tf <- str_replace_all(tf, "\\\\", "//")
#'data <- read_excel(tf)
#'data.long <- data %>%
#'pivot_longer(names_to = "time", values_to = "intensity", cols = 6:36) %>%
#'droplevels()
#'
#' asca_ti(intensity~PRODUCT*PANELIST,
#' data = data.long,timecol = "time")
#' }


asca_ti <- function(formula, data, timecol, time.quantization = NULL, ...){

  prev_contr <- options()$contrasts
  options(contrasts =  rep("contr.sum", 2),
          dplyr.show_progress = F)
  colref <- NULL
  . <-NULL
  ref <- NULL
  fact <- NULL
  formula_ <- formula
  data3 <- list()
  ref <- formula[[2L]]
  fact <- all.vars(formula) %>% .[!(. %in% as.character(ref))]
  timecol <- as.symbol(timecol)

  if(!is.null(time.quantization)){
    data <- mutate_at(data, vars(timecol),
                      function(x){x <- cut(x, round(x/time.quantization))})
  }

  data1 <- data %>% group_by_at(vars(as.name(timecol))) %>%
    do(filter(., length(pull(unique(.[, ref]))) != 1) %>%
         droplevels()) %>% ungroup() %>% droplevels()
#View(data1)
  colnames1 <- names(data1)

  data2 <- data1 %>%
    split(dplyr::select(., timecol )) %>%
      map(~droplevels(.) %>%
            mutate_at(., vars(ref), scale) %>%
            data.frame(
        effect = predict(glm(as.formula(formula),
          data = mutate_at(., vars(fact), as.factor),
          family = gaussian()), type = "terms")))
  #View(data2)


  colnames2 <- data2[[1]] %>% names(.)

  #print(colnames2)

  data2 <- data2 %>% plyr::ldply(., function(x){
    data.frame(x) %>% `colnames<-`(c(colnames2))}) %>%
    dplyr::select(-timecol)

  names(data2)[1] <- as.character(timecol)

  #View(data2)
  for(i in (length(colnames1)+1):ncol(data2)){
    anchor <- names(data2)[i]
    name <- anchor %>% str_remove(., "^effect.") %>% str_split_1(., "\\.")
    name2 <- paste(name, collapse = ":") %>% str_remove(., "_1$")
    refk <- c("refk")

    data2 %>% .[,c(1:2, i)] %>% cbind(data2 %>% .[,3:length(colnames1)] %>%
      .[, names(.) %in% name] %>% as.data.frame(.) %>%
        ifelse(!is.null(ncol(.)), unite(., col = name2, sep = ":"), .)) %>%
      `colnames<-`(c(names(data2)[c(1,2,i)], name2)) -> temp;
    temp[refk] <- temp[,as.character(timecol)]

    data3[[name2]] <- temp %>% dplyr::select(-timecol) %>%
      group_by_at(vars(as.name(refk), as.name(name2))) %>% slice(1) %>%
      ungroup() %>%
      pivot_wider(names_from = refk, values_from = as.symbol(anchor)) %>%
      column_to_rownames(name2) %>% .[,-1] %>%
      mutate_all(., function(x){x <- ifelse(is.na(x), mean(x, na.rm = T), x)}) %>%
      mutate_all(., function(x){x <- x - mean(x, na.rm = T)}) %>%
      prcomp()

  }



  data3[["info"]][["structure"]] <- "long"
  data3[["info"]][["type"]] <- "TI_ASCA"
  data3[["info"]][["timecol"]] <- unique(data[,as.character(timecol)])

  if(is_tibble(data3[["info"]][["timecol"]])){
    data3[["info"]][["timecol"]] <- pull(data3[["info"]][["timecol"]])
  }

  data3[["info"]][["formula"]] <- formula
  data3[["info"]][["labels"]]$timecol <- as.character(timecol)

  options(contrasts =  prev_contr,
          dplyr.show_progress = T)
  return(data3)

}

