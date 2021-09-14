#' @title stand (stem), height 
#' @return stand (stem), height 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), age 
mdl_H_137 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 1776
		c03 = 32.4
		c04 = 15
		c01 = 0.017
		ipf1 = exp(c01*(c02/(p02+c03)-c04))
		ipf2 = p02/(1-exp(-100*c01))^ipf1
		ipf2 * (1-exp(-c01 * p03))^ipf1
	}

	xmdl.4<-function(p02, p03){
		c02 = 794
		c03 = 18.2
		c04 = 12.4
		c01 = 0.023
		ipf1 = exp(c01*(c02/(p02+c03)-c04))
		ipf2 = p02/(1-exp(-100*c01))^ipf1
		ipf2 * (1-exp(-c01 * p03))^ipf1
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KS','KU','MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03), 
		NA
	)))
}
