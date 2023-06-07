#' @title stand (stem), height 
#' @return stand (stem), height 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age 
#' @param p03 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_H_28
mdl_H_28 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 3.3841
		c03 = 8.0296
		c04 = 2.2279
		c05 = 3.141
		c06 = 0.37
		c07 = 1.1938
		c01 = 1.82295
		ipf1 = c01+c02/(p03-c03)
		ipf2 = c04*ipf1-c05
		ipf3 = c06+c07*p03
		ipf3 * (1-exp(-0.01 * ipf1 * p02))^ipf2
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('TO','NU','SD','TS','LH','KP','JA','VA','SA','TA','KU','MA'), xmdl.1(p02, p03), 
		NA
	))
}
