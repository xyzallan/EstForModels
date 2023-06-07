#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), diameter (breast height)
#' @param p03 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_L_132
mdl_L_132 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 120.0
		c03 = -3.0
		c01 = 15.39
		c02+c01 * p02+c03 * p03
	}

	xmdl.2<-function(p02, p03){
		c02 = 167.1
		c03 = -3.5
		c01 = 13.58
		c02+c01 * p02+c03 * p03
	}

	xmdl.3<-function(p02, p03){
		c02 = 71.2
		c03 = 0.0
		c01 = 14.76
		c02+c01 * p02+c03 * p03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	))))
}
