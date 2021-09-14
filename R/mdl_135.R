#' @title Stem HF
#' @return Stem HF
#' @param p01 Stem Spec
#' @param p02 Stem D
#' @param p03 Stem H
#' @param p04 Crwn H
#' @param p05 Stem H
#' @param p06 Stem G
mdl_HF_135 <- function(p01, p02, p03, p04, p05, p06){
	xmdl.1<-function(p02, p03, p04, p05, p06){
		c02 = -0.0381
		c03 = 2.4243
		c04 = 6.5014
		c05 = -1.6114
		c06 = -0.0638
		c07 = -0.0021
		c08 = -0.0020
		c01 = 0.8217
		1-exp(-(c01+c02 * (p02 / p03)^c03+c04 * p03^c05+c06 * p04+c07 * p05+c08 * p06))
	}

	with(data.frame( p01, p02, p03, p04, p05, p06 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05, p06), 
		NA
		))
}
