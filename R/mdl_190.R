#' @title Stem D
#' @return Stem D
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
#' @param p04 Stem G
mdl_D_190 <- function(p01, p02, p03, p04){
	xmdl.1<-function(p02, p03, p04){
		c02 = -0.0256
		c03 = 0.0403
		c04 = -0.0198
		c01 = 1.1909
		c01+c02 * p02+c03 * p03+c04 * p04
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		NA
		))
}
