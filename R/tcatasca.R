#' Compute ANOVA Simultaneous Component Analysis (ASCA) of Temporal Check All That Apply (TCATA) data.
#' @param data A data frame, or object coercible by as.data.frame to a data frame, containing the variables in the model. It must be in long format and must contain a column for Temporal Check All That Apply binary data, a column reporting the time values, and a column that defines the attributes analyzed.
#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The formula will be applied for each atttribut at each time interval defined by the timecol column
#' @param timecol A string containing the name of the column indicating the time intervals of the TCATA dataset.
#' @param attributes A string containing the name of the column indicating the attributes of the TCATA.
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
#' tcatasca(CATA~(samp+cons)^2, data = tempR::ojtcata, time, attribute,)
#' describe(dataset, col1, col2)
#' }

tcatasca <- function(formula, data, timecol, attributes,
                     time.quantization = NULL, ...){
  prev_contr <- options()$contrasts
  options(contrasts =  rep("contr.sum", 2))
  colref <- NULL
  . <-NULL
  formula_ <- formula

  data3 <- list()

  ref <- formula[[2L]]
  fact <- as.character(formula[[3L]]) %>% as.vector() %>%
    str_split(., "[+*:]", simplify = T) %>% str_trim() %>%
    .[str_detect(., ".+")]
  timecol <- as.symbol(timecol)
  attributes <- as.symbol(attributes)
  if(!is.null(time.quantization)){
data <- mutate_at(data, vars(timecol),
                  function(x){x <- cut(x, round(x/time.quantization))})
  }
  data1 <- data %>% group_by_at(vars(as.name(timecol), as.name(attributes))) %>%
    do(filter(., length(pull(unique(.[, ref]))) != 1) %>% droplevels()) %>%
    ungroup() %>% droplevels()

  colnames1 <- names(data1)

  #View(data1)
  data2 <- data1 %>% split(dplyr::select(., timecol)) %>%
    map(~droplevels(.) %>% split(., dplyr::select(., attributes)) %>%
  map(~droplevels(.) %>% data.frame(effect = predict(glm(as.formula(formula),
    data = mutate_at(., vars(ref), scale), family = gaussian()),
    type = "terms"))))



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

   #residuals & fitted
    data1 %>% split(dplyr::select(., timecol)) %>%
      map(~droplevels(.) %>% split(., dplyr::select(., attributes)) %>%
            map(~droplevels(.) %>% data.frame(
              residuals = rstandard(glm(as.formula(formula),
           data = mutate_at(., vars(ref), scale), family = gaussian()),
           type = "deviance"),
           fitted = fitted.values(glm(as.formula(formula),
             data = mutate_at(., vars(ref), scale), family = gaussian())))) %>%
        plyr::ldply(., function(x){data.frame(x)}) ) %>%
       plyr::ldply(., function(x){data.frame(x) %>%
       `colnames<-`(c(as.character(attributes), colnames1,
                      "residuals", "fitted"))}) %>%
      dplyr::select(-ref, -timecol) %>% `colnames<-`(
        c(as.character(timecol),names(.)[2:length(names(.))])) -> temp

temp[refk] <- paste0(temp[,as.character(timecol)], "_",
                     temp[,as.character(attributes)])

data3[["Residuals"]] <- temp %>% dplyr::select(-timecol, -attributes) %>%
   .[,names(.) %in% c(fact, refk, "residuals")] %>% group_by(refk) %>%
   do(mutate(., colref = 1:nrow(.))) %>% ungroup() %>%
  pivot_wider(names_from = refk, values_from = residuals) %>%
  dplyr::select(-colref) %>% mutate_at(.,
    colnames(.)[!(colnames(.) %in% as.character(fact))], scale) %>%
  dplyr::select(., -c(as.character(fact))) %>%
  mutate_all(., function(x){x <- ifelse(is.na(x), mean(x, na.rm = T), x)}) %>%
  prcomp()


data3[["Parameters"]] <- temp %>% dplyr::select(-timecol, -attributes) %>%
  .[,names(.) %in% c(fact, refk, "residuals", "fitted")] %>%
  separate(refk, c("time", "attribute"), sep = "_")


#SSum of squares
temp <- data1 %>% split(dplyr::select(., timecol)) %>%
  map(~droplevels(.) %>% split(., dplyr::select(., attributes)) %>%
        map(~droplevels(.) %>% data.frame(Anova(glm(as.formula(formula),
          data = mutate_at(., vars(ref), scale), family = gaussian()),
          test.statistic = "F", type = "III") %>%
            mutate(Factor = as.character(rownames(.)))) %>%
            dplyr::select(attributes, timecol, Sum.Sq, Df, Factor)) %>%
        plyr::ldply(., function(x){data.frame(x)})) %>%
  plyr::ldply(., function(x){data.frame(x) %>% `colnames<-`(c("__",
    as.character(attributes), as.character(timecol), "Sum.Sq", "Df",
    "Factor"))}) %>% dplyr::select(-c(1,2)) %>%
  group_by_at(vars(as.name(timecol), as.name(attributes), Factor) ) %>%
  slice(1) %>%
  group_by_at(vars(as.name(timecol), as.name(attributes))) %>%
  do(mutate(., Sum.Sq_ = Sum.Sq/sum(Sum.Sq))) %>%
  ungroup() %>% mutate(reference = sum(Sum.Sq_)) %>%
  group_by(Factor) %>% summarize(Sum.Sq = sum(Sum.Sq_)/reference) %>%
  slice(1) %>% ungroup()



data3[["SS_decomposition"]] <- temp


data3[["info"]][["timecol"]] <- unique(data[,as.character(timecol)])
data3[["info"]][["attributes"]] <- unique(data %>%
    mutate(across(attributes, as.character)) %>%
    .[,as.character(attributes)])

if(is_tibble(data3[["info"]][["attributes"]])){
  data3[["info"]][["attributes"]] <- pull(data3[["info"]][["attributes"]])
  }

  options(contrasts =  prev_contr)
  return(data3)
}

