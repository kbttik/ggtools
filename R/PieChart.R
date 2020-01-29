#' 
#' Caluculating the text posion in pie chart
#' 
#' @param y numeric pie chartで描く値
#' 
#' @return numeric textのポジション
#' 
#' @examples make_piechart_text_position(vector)
#' 
#' @export
make_piechart_text_position <- function(y){
  y <- rev(y)
  
  # cumsumして自分の切れ目の端から半分(0.5)をポジションとする
  y_cumsum <- cumsum(y) 
  y_position <- y_cumsum - (y*.5)
  
  return(rev(y_position))
}