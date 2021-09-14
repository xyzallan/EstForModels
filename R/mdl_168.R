#' @title Stem N
#' @return Stem N
#' @param p01 Stem Spec
#' @param p02 Stem N
#' @param p03 Stem H
#' @param p04 Stem H
mdl_N_168 <- function(p01, p02, p03, p04){
	xmdl.2<-function(p02, p03, p04){
		c02 = 0.000264
		c03 = 2.341983
		c01 = -0.618365
		1000 * ((p02 / 1000)^c01+c02 * (p03^c03-p04^c03))^(1 / c01)
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		NA
		))
}
