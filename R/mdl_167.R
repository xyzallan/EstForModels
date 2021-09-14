#' @title stand (stem), basal area (breast height)
#' @return stand (stem), basal area (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), basal area (breast height)
#' @param p03 stand (stem), age 
#' @param p04 stand (stem), age (random point)
#' @param p05 stand (stem), number of trees 
#' @param p06 stand (stem), number of trees 
#' @param p07 stand (stem), height (100 years old)
mdl_G_167 <- function(p01, p02, p03, p04, p05, p06, p07){

	xmdl.2<-function(p02, p03, p04, p05, p06, p07){
		c02 = -0.0122
		c03 = -0.00681
		c01 = 5.5357
		ipf1 = p03/p04
		exp(ipf1 * log(p02)+c01 * (1-ipf1)+c02 * p07 * (1-ipf1)+c03 * (log(p06)-ipf1 * log(p05)))
	}

	with(data.frame( p01, p02, p03, p04, p05, p06, p07 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05, p06, p07), 
		NA
	))
}
