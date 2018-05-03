#' pct(x)
#' 
#' This function calculate percent distribution for factors variables
#' 
#' @param x      factor for which we want to claculate percent distribution.
#' 

pct <- function(x){
  tbl <- table(x)
  tbl_pct <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(tbl_pct) <- c('Count','Percentage')
  kable(tbl_pct)
}

