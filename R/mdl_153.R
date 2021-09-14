#' @title Stem H
#' @return Stem H
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem D
#' @param p04 Stem H
mdl_H_153 <- function(p01, p02, p03, p04){
	xmdl.2<-function(p02, p03, p04){
		c03 = 18.357
		c04 = 0.0
		c05 = 0.0
		c01 = 7.213
		c02 = 39.680
		c01+c02 * p03 / p02+c03 * p03+c04 / p02+c05 * p04
	}
	xmdl.21<-function(p02, p03, p04){
		c03 = 21.59
		c02 = 0.0
		c04 = 0.0
		c05 = -0.0252
		c01 = 11.79
		c01+c02 * p03 / p02+c03 * p03+c04 / p02+c05 * p04
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('PO'), xmdl.21(p02, p03, p04), 
		NA
		)))
}
