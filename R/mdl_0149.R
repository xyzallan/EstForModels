#' @title single tree (crown), height (base)
#' @return single tree (crown), height (base)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), height 
#' @param p03 single tree (stem), diameter (breast height)
#' @export
#' @rdname mdl_H_149
mdl_H_149 <- function(p01, p02, p03){

	xmdl.2<-function(p02, p03){
		c02 = -0.8490
		c03 = 0.0000377
		c01 = -0.0944
		p02 * (1-exp(c01+c02 * p02 / p03+c03 * p03))
	}

	xmdl.21<-function(p02, p03){
		c02 = -0.1709
		c03 = -0.0007804
		c01 = -0.4995
		p02 * (1-exp(c01+c02 * p02 / p03+c03 * p03))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('PO'), xmdl.21(p02, p03), 
		NA
	)))
}
