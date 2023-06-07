#' @title stand (stem), height (100 years old)
#' @return stand (stem), height (100 years old)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (dominant)
#' @param p03 stand (stem), age (breast height)
mdl_H_62 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 4.70248
		c03 = 0.47692
		c01 = 128.229
		c01 * exp((log(p02)-log(c01)) / exp(c02 / p03^c03-c02 / 100^c03))
	}

	xmdl.2<-function(p02, p03){
		c02 = 4.64631
		c03 = 0.29981
		c01 = 147.481
		c01 * exp((log(p02)-log(c01)) / exp(c02 / p03^c03-c02 / 100^c03))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	)))
}
