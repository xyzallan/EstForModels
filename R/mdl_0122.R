#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), diameter (breast height)
#' @export
#' @rdname mdl_L_122
mdl_L_122 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 14.2
		c03 = -3.4
		c01 = 143.6
		c01+c02 * p03+c03 * p02
	}

	xmdl.2<-function(p02, p03){
		c02 = 12.9
		c03 = -1.2
		c01 = 91.8
		c01+c02 * p03+c03 * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	)))
}
