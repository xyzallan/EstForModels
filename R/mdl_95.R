#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem G
#' @param p04 Stem A
mdl_V_95 <- function(p01, p02, p03, p04){
	xmdl.1<-function(p02, p03, p04){
		c02 = 0.000153
		c03 = -0.0248
		c01 = 1.695
		p02 * p03 * (c01 / p04+c02 * p04+c03)
	}
	xmdl.2<-function(p02, p03, p04){
		c02 = 0.0000038
		c03 = -0.00878
		c01 = 1.516
		p02 * p03 * (c01 / p04+c02 * p04+c03)
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		NA
		)))
}
