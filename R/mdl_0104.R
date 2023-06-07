#' @title stand (stem), height 
#' @return stand (stem), height 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), height (100 years old)
mdl_H_104 <- function(p01, p02, p03){

	xmdl.2<-function(p02, p03){
		c02 = 1.301
		c03 = -0.3877
		c04 = 1.409
		c01 = -4.858
		p02+exp(c01+c02 * log(p02)+c03 * log(p02)^2+c04 * log(p03))
	}

	xmdl.3<-function(p02, p03){
		c02 = 0.9128
		c03 = -0.2975
		c04 = 1.936
		c01 = -6.178
		p02+exp(c01+c02 * log(p02)+c03 * log(p02)^2+c04 * log(p03))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	)))
}
