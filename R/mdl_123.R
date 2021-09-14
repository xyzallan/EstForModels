#' @title Stem L
#' @return Stem L
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem D
mdl_L_123 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 15.2
		c03 = -3.7
		c01 = 166.8
		c01+c02 * p03+c03 * p02
	}
	xmdl.2<-function(p02, p03){
		c02 = 13.3
		c03 = -1.9
		c01 = 121.6
		c01+c02 * p03+c03 * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
		)))
}
