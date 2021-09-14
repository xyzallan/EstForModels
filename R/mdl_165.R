#' @title LigSt F
#' @return LigSt F
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
mdl_F_165 <- function(p01, p02, p03){
	xmdl.21<-function(p02, p03){
		c02 = 0.0017335
		c03 = 1.1267
		c04 = 118.188
		c05 = 0.0000042
		c01 = 0.4039
		c01+c02 * p03+c03 / p03+c04 / (p02 * p02 * p02)+c05 * p02 * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('PO'), xmdl.21(p02, p03), 
		NA
		))
}
