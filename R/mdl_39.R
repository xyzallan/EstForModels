#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem A
mdl_V_39 <- function(p01, p02, p03){
	xmdl.7<-function(p02, p03){
		c02 = 3.9
		c01 = 1.9
		c01+c02 * p02^2 / sqrt(p03)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03), 
		NA
		))
}
