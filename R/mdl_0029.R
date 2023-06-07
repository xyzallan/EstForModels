#' @title stand (stem), height 
#' @return stand (stem), height 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (50 years old)
#' @param p03 stand (stem), age 
mdl_H_29 <- function(p01, p02, p03){

	xmdl.3<-function(p02, p03){
		c02 = 1.743
		c03 = 1.5937
		c04 = 0.5633
		c05 = 0.1405
		c01 = 0.27951
		ipf1 = (c01*p02-c02)^0.5
		ipf2 = c03-ipf1*(c04-ipf1*c05)
		p02 * (1-exp(-0.01 * ipf1 * p03^ipf2)) / (1-exp(-0.01 * ipf1 * 50^ipf2))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('TL','PN','PP','RE','LV','LM','HB','KS'), xmdl.3(p02, p03), 
		NA
	))
}
