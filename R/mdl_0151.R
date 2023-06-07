#' @title single tree (crown), height (base)
#' @return single tree (crown), height (base)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), height 
#' @param p03 single tree (stem), diameter (breast height)
mdl_H_151 <- function(p01, p02, p03){

	xmdl.2<-function(p02, p03){
		c02 = 0.3676
		c01 = 0.6412
		p02 * (1-exp(-(c01+c02 * p02 / p03)^2))
	}

	xmdl.21<-function(p02, p03){
		c02 = 0.1760
		c01 = 0.8019
		p02 * (1-exp(-(c01+c02 * p02 / p03)^2))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('PO'), xmdl.21(p02, p03), 
		NA
	)))
}
