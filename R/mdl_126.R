#' @title Stem L
#' @return Stem L
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem D
mdl_L_126 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 0.928
		c03 = 19.94
		c04 = -0.22
		c01 = -5.2
		c01+c02 * p02+(c03+c04 * p03) * p03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		NA
		))
}
