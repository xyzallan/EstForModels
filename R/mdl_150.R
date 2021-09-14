#' @title single tree (crown), diameter 
#' @return single tree (crown), diameter 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
mdl_D_150 <- function(p01, p02, p03){

	xmdl.2<-function(p02, p03){
		c02 = 0.3126
		c03 = 0.0131
		c04 = -0.2825
		c05 = 2.3164
		c01 = -0.0924
		exp(c01+c02 * log(p02)+c03 * p03+c04 * log(p03 / p02))
	}

	xmdl.21<-function(p02, p03){
		c02 = 0.3265
		c03 = 0.0412
		c04 = -0.0412
		c05 = 2.6912
		c01 = 0.3399
		exp(c01+c02 * log(p02)+c03 * p03+c04 * log(p03 / p02))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('PO'), xmdl.21(p02, p03), 
		NA
	)))
}
