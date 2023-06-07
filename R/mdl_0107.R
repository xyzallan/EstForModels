#' @title single tree (crown), height 
#' @return single tree (crown), height 
#' @param p01 single tree (stem), species 
#' @param p02 stand (stem), basal area (breast height)
#' @param p03 stand (stem), height (dominant)
#' @param p04 single tree (stem), diameter (breast height)
#' @param p05 single tree (stem), height 
mdl_H_107 <- function(p01, p02, p03, p04, p05){

	xmdl.1<-function(p02, p03, p04, p05){
		c02 = 3.0994
		c03 = 0.4496
		c01 = 0.0314
		1-exp(-(exp(-c01 * p02)+c02 / p03)) * (p04 / p05)^c03
	}

	with(data.frame( p01, p02, p03, p04, p05 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05), 
		NA
	))
}
