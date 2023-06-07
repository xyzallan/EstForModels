#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), diameter (breast height)
#' @export
#' @rdname mdl_L_123
mdl_L_123 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 15.2
		c03 = -3.7
		c01 = 166.8
		c01+c02 * p03+c03 * p02
	}

	xmdl.2<-function(p02, p03){
		c02 = 13.3
		c03 = -1.9
		c01 = 121.6
		c01+c02 * p03+c03 * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	)))
}
