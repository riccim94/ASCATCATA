#' Estimate the cumulative sum of squares for each factor included in an ANOVA Simultaneous Component Analysis (ASCA) of Temporal Check All That Apply (TCATA) data, to estimate the effect size of the decomposition.
#' @param data A data frame, or object coercible by as.data.frame to a data frame, containing the variables in the model. It must be in long format and must contain a column for Temporal Check All That Apply binary data, a column reporting the time values, and a column that defines the attributes analyzed.
#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The formula will be applied for each atttribut at each time interval defined by the timecol column
#' @param timecol A string containing the name of the column indicating the time intervals of the TCATA dataset.
#' @param attributes A string containing the name of the column indicating the attributes of the TCATA.
#' @param graph Logical. If TRUE prints a barplot showing the percentage of cumulative sum of squares related to each factor.
#' @param ... Optional parameters
#' @return A list of objects containing the results of ASCA decomposition of a structured dataset.
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_text
#' @importFrom car Anova
#' @importFrom scales percent
#' @export
#' @examples
#' \dontrun{
#' SS_tcatasca(CATA~(samp+cons)^2, data = tempR::ojtcata, time, attribute)
#' describe(dataset, col1, col2)
#' }
#'


SS_tcatasca <- function(data, formula, timecol, attributes, graph = T, ...){

  { unicode_minus <- function(x){sub('^-', '\U2212', format(x))} }
  . <- NULL
  Sum.Sq <- NULL
  Factor <- NULL
  label2 <- NULL


  data3 <- list()

  ref <- formula[[2L]]
  fact <- as.character(formula[[3L]]) %>% as.vector() %>%
    str_split(., "[+*:]", simplify = T) %>% str_trim() %>%
    .[str_detect(., ".+")]
  timecol <- as.symbol(timecol)
  attributes <- as.symbol(attributes)
  data1 <- data %>% group_by_at(vars(c(timecol, attributes))) %>%
    do(filter(., length(pull(unique(.[, ref]))) != 1) %>% droplevels()) %>%
    ungroup() %>% droplevels()

  colnames1 <- names(data1)

  #View(data1)
  data2 <- data1 %>% split(dplyr::select(., timecol)) %>%
    map(~droplevels(.) %>% split(., dplyr::select(., attributes)) %>%
      map(~droplevels(.) %>% data.frame(
        car::Anova(glm(formula, data = mutate_at(., vars(ref), scale),
          family = gaussian()), test.statistic = "F", type = "III") %>%
          mutate(Factor = rownames(.)) ) ) )

#print(data2)

  colnames2 <- data2[[1]][[1]] %>% names(.)

  data2 <- data2 %>% map(., ~plyr::ldply(., function(x){
    as.data.frame(x) %>% `colnames<-`(c(colnames2))
    }) ) %>%
  plyr::ldply(., function(x){
    data.frame(x) %>%
      `colnames<-`(c(as.character(attributes), colnames2))
    }) %>% dplyr::select(-timecol)

  names(data2)[1] <- as.character(timecol)

   # data2 %>% group_by_at(vars(c(timecol, attributes))) %>%
   #   do(mutate(., Sum.Sq = Sum.Sq/sum(Sum.Sq))) %>% ungroup() %>%
   #   View()

  data3 <- data2 %>% group_by_at(vars(c(timecol, attributes))) %>%
    do(mutate(., Sum.Sq = (Sum.Sq/sum(Sum.Sq))*100)) %>% ungroup() %>%
    group_by(Factor) %>% do(mutate(., count = nrow(.))) %>%
    summarize(Sum.Sq = (sum(Sum.Sq)/unique(count)) ) %>%
    mutate(label2 = paste0(round(Sum.Sq*100, 2), " %"))

if(graph){
  print(
data3 %>%
  ggplot(aes(x = Factor, y = Sum.Sq)) +
  geom_col(fill = "white", color = "black") +
  geom_text(aes(label = label2), vjust = -0.75) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() )
}

return(data3)


}
