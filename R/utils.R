calculate_z_row <- function(df){
  dfz <- t(scale(t(df)))
  dfz[is.na(dfz)] <- 0
  return(dfz)
}

sliding_window <- function(
  vec,
  window_width
){
  all_windows <- length(vec)-window_width+1
  return(lapply(1:all_windows, function(k){vec[k:(k+window_width-1)]}))
}

addLeadingZeros <- function(
  num_vec
){
  digit_number <- floor(max(log10(num_vec)+1))
  return(stringr::str_pad(as.character(num_vec), digit_number, pad = "0"))
}


remove_combination_duplicates <- function(
  df,
  columns
){
  return(df[!duplicated(t(apply(df[columns], 1, sort))), ])
}


dataframe_to_list <- function(
  df
){
  list_items <- unique(df[, 1])
  output_list <- lapply(list_items, function(x){return(df[df[, 1] == x, 2])})
  names(output_list) <- list_items
  output_list <- output_list[order(lengths(output_list), decreasing = TRUE)]
  return(output_list)
}


renumber_vector <- function(x){
  y <- factor(x %>% as.character(), levels = unique(x %>% as.character())) %>% as.numeric()
  names(y) <- names(x)
  return(y)
}
