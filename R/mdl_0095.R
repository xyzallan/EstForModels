#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), basal area (breast height)
#' @param p04 stand (stem), age 
mdl_V_95 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 0.000153
		c03 = -0.0248
		c01 = 1.695
		p02 * p03 * (c01 / p04+c02 * p04+c03)
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = 0.0000038
		c03 = -0.00878
		c01 = 1.516
		p02 * p03 * (c01 / p04+c02 * p04+c03)
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		NA
	)))
}
