#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), diameter (breast height)
mdl_L_114 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 14.0
		c03 = -3.0
		c01 = 126.7
		c01+c02 * p03+c03 * p02
	}

	xmdl.2<-function(p02, p03){
		c02 = 13.0
		c03 = -3.4
		c01 = 152.1
		c01+c02 * p03+c03 * p02
	}

	xmdl.3<-function(p02, p03){
		c02 = 14.7
		c03 = 0.0
		c01 = 71.4
		c01+c02 * p03+c03 * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	))))
}
