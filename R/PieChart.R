#' 
#' pie chartのテキストのポジションを求める
#' 
#' @param y numeric pie chartで描く値
#' 
#' @return numeric textのポジション
#' 
#' @export
#' @examples make_piechart_text_position(c(30, 10, 20))
make_piechart_text_position <- function(y){
  y <- rev(y)
  
  # cumsumして自分の切れ目の端から半分(0.5)をポジションとする
  y_cumsum <- cumsum(y) 
  y_position <- y_cumsum - (y * 0.5)
  
  return(rev(y_position))
}
