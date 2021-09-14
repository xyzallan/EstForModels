#' @title stand (stem), height (dominant)
#' @return stand (stem), height (dominant)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), age 
mdl_H_174 <- function(p01, p02, p03){

	xmdl.2<-function(p02, p03){
		c02 = 1.507
		c01 = 0.0006
		p02 * ((1-exp(-c01 * p02 * p03)) / (1-exp(-c01 * 100 * p03)))^c02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	))
}
