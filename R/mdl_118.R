#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), diameter (breast height)
mdl_L_118 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 16.8
		c03 = -4.7
		c01 = 179.1
		c01+c02 * p03+c03 * p02
	}

	xmdl.2<-function(p02, p03){
		c02 = 15.5
		c03 = -4.2
		c01 = 187.2
		c01+c02 * p03+c03 * p02
	}

	xmdl.3<-function(p02, p03){
		c02 = 18.7
		c03 = 0.0
		c01 = 81.5
		c01+c02 * p03+c03 * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	))))
}
