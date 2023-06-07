#' @title single tree (stem), diameter (random point)
#' @return single tree (stem), diameter (random point)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
#' @param p04 single tree (stem), height (random point)
#' @export
#' @rdname mdl_D_156
mdl_D_156 <- function(p01, p02, p03, p04){

	xmdl.7<-function(p02, p03, p04){
		c02 = 0.2452940
		c03 = 0.5698770
		c01 = 0.0450652
		ipf1 = (c03*p02/2)/(1-exp(c02*(1.3-p03)))+(p02/2-(c03*p02/2))*(1-1/(1-exp(c01*(1.3-p03))))
		ipf2 = (p02/2-(c03*p02/2))*exp(c01*1.3)/(1-exp(c01*(1.3-p03)))
		ipf3 = (c03*p02/2)*exp(-c02*p03)/(1-exp(c02*(1.3-p03)))
		(ipf1+ipf2 * exp(-c01 * p04)-ipf3 * exp(c02 * p04)) * 2
	}

	xmdl.21<-function(p02, p03, p04){
		c02 = 0.1359840
		c03 = 0.6946140
		c01 = 0.0862735
		ipf1 = (c03*p02/2)/(1-exp(c02*(1.3-p03)))+(p02/2-(c03*p02/2))*(1-1/(1-exp(c01*(1.3-p03))))
		ipf2 = (p02/2-(c03*p02/2))*exp(c01*1.3)/(1-exp(c01*(1.3-p03)))
		ipf3 = (c03*p02/2)*exp(-c02*p03)/(1-exp(c02*(1.3-p03)))
		(ipf1+ipf2 * exp(-c01 * p04)-ipf3 * exp(c02 * p04)) * 2
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04), 
		ifelse(p01 %in% c('PO'), xmdl.21(p02, p03, p04), 
		NA
	)))
}
