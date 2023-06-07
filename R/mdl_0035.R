#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), age 
#' @export
#' @rdname mdl_V_35
mdl_V_35 <- function(p01, p02, p03){

	xmdl.2<-function(p02, p03){
		c02 = 2.421
		c03 = 29.23
		c04 = 0.265
		c05 = -0.000556
		c06 = -2.1
		c01 = -3.6
		c01+(c02 * p02+c03) * p02 / sqrt(p03)+(c04+c05 * p02 / log(p03)) * p02^2+c06 * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	))
}
