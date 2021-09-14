#' @title Stem N
#' @return Stem N
#' @param p01 Stem Spec
#' @param p02 Stem A
#' @param p03 Stem G
#' @param p04 Stem H
mdl_N_139 <- function(p01, p02, p03, p04){
	xmdl.2<-function(p02, p03, p04){
		c02 = 0.4246
		c03 = 3.26
		c04 = 0.32
		c01 = 0.228
		c01 * (p02 * p03)^c02 * (c03 * p04-1)^c04
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		NA
		))
}
