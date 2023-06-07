#' @title stand (stem), height 
#' @return stand (stem), height 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), age 
#' @export
#' @rdname mdl_H_94
mdl_H_94 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 0.03625
		c03 = 2.175
		c01 = 1.051
		c01 * p02 * (1-exp(-c02 * p03))^c03
	}

	xmdl.2<-function(p02, p03){
		c02 = 0.01206
		c03 = 1.592
		c01 = 1.719
		c01 * p02 * (1-exp(-c02 * p03))^c03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	)))
}
