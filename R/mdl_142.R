#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem G
#' @param p03 Stem A
#' @param p04 Stem H
mdl_V_142 <- function(p01, p02, p03, p04){
	xmdl.2<-function(p02, p03, p04){
		c02 = 0.3459
		c03 = 0.0525
		c04 = -0.1007
		c01 = 5.283
		c01 * (p03 * p02)^c02 * (p03 * p02)^(c03 * p04) * 10^(c04 * p04)
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		NA
		))
}
