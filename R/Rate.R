#' 
#' cumsum(累積和)したときに占める割合を求める
#' @param x numeric 累積和するnumeric型のベクトル
#' @param arrange boolean TRUEなら昇順にしてから求める.FALSEならそのまま
#' 
#' @return numeric 全体の何割を占めているかのベクトル
#' 
#' @example get_rate_with_cusum(numeric_vector)
#' 
#' @export

get_rate_with_cusum <- function(x, arrange = TRUE){
  if(!all(x > 0))
    stop("input value needs > 0")
  
  x_sum <- sum(x)
  
  if(arrange){
    x_tibble <- 
      dplyr::tibble(origin = x) %>% 
      dplyr::mutate(row_origin = dplyr::row_number()) %>% 
      dplyr::arrange(origin) %>% 
      dplyr::mutate(cusum = cumsum(origin)) %>% 
      dplyr::mutate(cusum = cusum / x_sum) %>% 
      dplyr::arrange(row_origin)
  }
  else{
    x_tibble <- 
      dplyr::tibble(origin = x) %>% 
      dplyr::mutate(cusum = cumsum(origin)) %>% 
      dplyr::mutate(cusum = cusum / x_sum)
  }
  
  return(x_tibble$cusum)
}