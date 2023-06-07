#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), diameter (breast height)
mdl_L_124 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = -3.4
		c03 = 14.46
		c01 = 121.1
		(c01+c02 * p02+c03 * p03)
	}

	xmdl.2<-function(p02, p03){
		c02 = -0.74
		c03 = 12.77
		c01 = 64.8
		(c01+c02 * p02+c03 * p03)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	)))
}
