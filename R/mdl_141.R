#' @title Stem G
#' @return Stem G
#' @param p01 Stem Spec
#' @param p02 Stem A
#' @param p03 Stem H
mdl_G_141 <- function(p01, p02, p03){
	xmdl.2<-function(p02, p03){
		c02 = 0.1791
		c03 = 6.37
		c01 = 0.0942
		c01 * p02 * p03+c02 * p02-c03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
		))
}
