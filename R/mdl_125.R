#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), diameter (breast height)
mdl_L_125 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = -3.0
		c03 = 11.42
		c01 = 149.8
		(c01+c02 * p02+c03 * p03)
	}

	xmdl.2<-function(p02, p03){
		c02 = -1.8
		c03 = 10.51
		c01 = 96.6
		(c01+c02 * p02+c03 * p03)
	}

	xmdl.3<-function(p02, p03){
		c02 = -0.37
		c03 = 13.68
		c01 = 72.7
		(c01+c02 * p02+c03 * p03)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	))))
}
