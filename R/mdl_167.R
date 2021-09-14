#' @title Stem G
#' @return Stem G
#' @param p01 Stem Spec
#' @param p02 Stem G
#' @param p03 Stem A
#' @param p04 Stem A
#' @param p05 Stem N
#' @param p06 Stem N
#' @param p07 Stem H
mdl_G_167 <- function(p01, p02, p03, p04, p05, p06, p07){
	xmdl.2<-function(p02, p03, p04, p05, p06, p07){
		c02 = -0.0122
		c03 = -0.00681
		c01 = 5.5357
		ipf1 = p03/p04
		exp(ipf1 * log(p02)+c01 * (1-ipf1)+c02 * p07 * (1-ipf1)+c03 * (log(p06)-ipf1 * log(p05)))
	}

	with(data.frame( p01, p02, p03, p04, p05, p06, p07 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05, p06, p07), 
		NA
		))
}
