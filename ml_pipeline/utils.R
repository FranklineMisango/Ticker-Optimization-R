transform_ticker_name <- function(ticker_names_table){
  ticker_names <- as.vector(ticker_names_table %>% select(Symbol))
  ticker_transform_names <-  gsub("[[:punct:]]", "-", ticker_names$Symbol)
  ticker_transform_names <- as.vector(stringi::stri_paste(ticker_transform_names, ".TO"))
  return(ticker_transform_names)
}

finding_random_weight <- function(quintile_ticker_names,repetitions,weights){
  random_weights_multiple <- sample(weights, length(quintile_ticker_names)*repetitions, replace=TRUE)
  index_zeroes_multiple <- sample(c(1:length(random_weights_multiple)), round(length(random_weights_multiple)*.75, 0))
  random_weights_multiple[index_zeroes_multiple]<-0
  return(random_weights_multiple)
}




