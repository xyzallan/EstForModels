#' @title Crwn H
#' @return Crwn H
#' @param p01 Stem Spec
#' @param p02 Stem G
#' @param p03 Stem H
#' @param p04 Stem D
#' @param p05 Stem H
mdl_H_107 <- function(p01, p02, p03, p04, p05){
	xmdl.1<-function(p02, p03, p04, p05){
		c02 = 3.0994
		c03 = 0.4496
		c01 = 0.0314
		1-exp(-(exp(-c01 * p02)+c02 / p03)) * (p04 / p05)^c03
	}

	with(data.frame( p01, p02, p03, p04, p05 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05), 
		NA
		))
}
