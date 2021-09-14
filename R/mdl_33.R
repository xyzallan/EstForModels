#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem A
mdl_V_33 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = 2.806
		c03 = 4.74
		c01 = 6.631
		c01+(c02 * p02 / sqrt(p03)+c03) * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		NA
		))
}
